program test_project_layering_cli
    !! `fo graph --layers` on the project model: an fpm and a CMake fixture of
    !! the same structure (core, util, model -> core, an executable and an
    !! external dependency), each clean and with one violation. Both backends
    !! must report the same violation. Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_lines
    use fo_test_harness, only: remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    implicit none

    character(len=*), parameter :: expected_violation = &
        'src/model.f90|lay_util|model|src/util.f90|util|model -> util;'
    character(:), allocatable :: driver, scratch, fpm_dir, cmake_dir, ext_dir
    character(:), allocatable :: cache, fpm_report, cmake_report
    type(string_list_t) :: cmake_env, fpm_env

    call resolve_driver(driver)
    call make_scratch('fo-project-layering-cli', scratch)
    cache = join_path(scratch, 'cache')
    fpm_dir = join_path(scratch, 'fpm')
    cmake_dir = join_path(scratch, 'cmake')
    ext_dir = join_path(scratch, 'ext')
    call list_add(cmake_env, 'FO_BACKEND=cmake')
    call list_add(cmake_env, 'FO_JOBS=2')
    call list_add(fpm_env, 'FO_JOBS=2')
    call write_shared_sources(fpm_dir)
    call write_shared_sources(cmake_dir)
    call write_external()
    call write_lines(join_path(fpm_dir, 'fpm.toml'), [character(len=72) :: &
        'name = "lay_fpm"', &
        '[dependencies]', &
        'lay_ext = { path = "../ext" }', &
        '[extra.fo.layers]', &
        'core = { source = ["src/core.f90"] }', &
        'util = { source = ["src/util.f90"] }', &
        'model = { source = ["src/model.f90"], uses = ["core"] }'])
    call write_lines(join_path(cmake_dir, 'CMakeLists.txt'), [character(len=96) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(lay_cmake Fortran)', &
        'add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../ext ext)', &
        'add_library(core STATIC src/core.f90)', &
        'add_library(util STATIC src/util.f90)', &
        'add_library(model STATIC src/model.f90)', &
        'target_link_libraries(model core)', &
        'add_executable(lay_main app/main.f90)', &
        'target_link_libraries(lay_main model)'])

    ! Clean: model uses core (allowed) and the external module (ignored).
    call write_model(.false.)
    call check_clean(fpm_dir, fpm_env, 'fpm')
    call check_clean(cmake_dir, cmake_env, 'cmake')

    ! model also uses util, which it neither declares nor links.
    call write_model(.true.)
    fpm_report = violations(fpm_dir, fpm_env, 'fpm')
    cmake_report = violations(cmake_dir, cmake_env, 'cmake')
    call assert_equal_string(fpm_report, expected_violation, 'fpm violation')
    call assert_equal_string(cmake_report, expected_violation, 'cmake violation')
    call assert_equal_string(cmake_report, fpm_report, &
        'both backends report the same violation')
    call check_text(fpm_dir, fpm_env, 'fpm')
    call check_text(cmake_dir, cmake_env, 'cmake')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'project-layering-cli: clean and violating fpm and CMake passed'

contains

    subroutine write_shared_sources(dir)
        character(len=*), intent(in) :: dir

        call write_lines(join_path(dir, 'src/core.f90'), [character(len=48) :: &
            'module lay_core', 'implicit none', 'integer, parameter :: c = 1', &
            'end module lay_core'])
        call write_lines(join_path(dir, 'src/util.f90'), [character(len=48) :: &
            'module lay_util', 'implicit none', 'integer, parameter :: u = 2', &
            'end module lay_util'])
        call write_lines(join_path(dir, 'app/main.f90'), [character(len=48) :: &
            'program lay_main', 'use lay_model, only: m', 'implicit none', &
            'print *, m', 'end program lay_main'])
    end subroutine write_shared_sources

    subroutine write_external()
        call write_lines(join_path(ext_dir, 'fpm.toml'), [character(len=48) :: &
            'name = "lay_ext"'])
        call write_lines(join_path(ext_dir, 'CMakeLists.txt'), [character(len=48) :: &
            'add_library(lay_ext STATIC src/ext.f90)'])
        call write_lines(join_path(ext_dir, 'src/ext.f90'), [character(len=48) :: &
            'module lay_ext', 'implicit none', 'integer, parameter :: e = 3', &
            'end module lay_ext'])
    end subroutine write_external

    subroutine write_model(use_util)
        logical, intent(in) :: use_util
        character(len=48), allocatable :: lines(:)

        if (use_util) then
            lines = [character(len=48) :: 'module lay_model', 'use lay_core, only: c', &
                'use lay_ext, only: e', 'use lay_util, only: u', 'implicit none', &
                'integer, parameter :: m = c + e + u', 'end module lay_model']
        else
            lines = [character(len=48) :: 'module lay_model', 'use lay_core, only: c', &
                'use lay_ext, only: e', 'implicit none', &
                'integer, parameter :: m = c + e', 'end module lay_model']
        end if
        call write_lines(join_path(fpm_dir, 'src/model.f90'), lines)
        call write_lines(join_path(cmake_dir, 'src/model.f90'), lines)
    end subroutine write_model

    subroutine fo(dir, env, words, result)
        character(len=*), intent(in) :: dir, words(:)
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments
        integer :: i

        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, dir, cache, result, env)
    end subroutine fo

    subroutine layers_json(dir, env, report, exit_code)
        character(len=*), intent(in) :: dir
        type(string_list_t), intent(in) :: env
        type(json_value_t), intent(out) :: report
        integer, intent(out) :: exit_code
        type(process_result_t) :: result
        character(:), allocatable :: message
        logical :: valid

        call fo(dir, env, [character(len=16) :: 'graph', '--layers', '--json'], result)
        exit_code = result%exit_code
        call json_parse(result%stdout, report, valid, message)
        call assert_true(valid, 'layering JSON parses: '//result%stdout//result%stderr)
    end subroutine layers_json

    function joined_strings(array) result(text)
        type(json_value_t), intent(in) :: array
        character(:), allocatable :: text
        integer :: i

        text = ''
        do i = 1, json_size(array)
            text = text//json_string_value(json_element(array, i))//';'
        end do
    end function joined_strings

    subroutine check_clean(dir, env, tag)
        character(len=*), intent(in) :: dir, tag
        type(string_list_t), intent(in) :: env
        type(json_value_t) :: report
        integer :: exit_code

        call layers_json(dir, env, report, exit_code)
        call assert_equal_integer(exit_code, 0, tag//' clean layering exits 0')
        call assert_true(json_boolean_value(json_member(report, 'ok')), tag//' ok')
        call assert_equal_integer(json_size(json_member(report, 'violations')), 0, &
            tag//' clean has no violation')
        call assert_equal_string(joined_strings(json_member(report, 'unlayered')), &
            'app/main.f90;', tag//' lists the unlayered executable source')
        call assert_equal_integer(json_size(json_member(report, 'layers')), 3, &
            tag//' has the three project layers (no dependency layer)')
    end subroutine check_clean

    function violations(dir, env, tag) result(text)
        !! Violations as source|module|layer|provider|provider_layer|edge;
        character(len=*), intent(in) :: dir, tag
        type(string_list_t), intent(in) :: env
        character(:), allocatable :: text
        type(json_value_t) :: report, v
        integer :: exit_code, i

        call layers_json(dir, env, report, exit_code)
        call assert_equal_integer(exit_code, 1, tag//' violation exits 1')
        call assert_true(.not. json_boolean_value(json_member(report, 'ok')), &
            tag//' not ok')
        text = ''
        do i = 1, json_size(json_member(report, 'violations'))
            v = json_element(json_member(report, 'violations'), i)
            text = text//json_string_value(json_member(v, 'source'))//'|'// &
                json_string_value(json_member(v, 'module'))//'|'// &
                json_string_value(json_member(v, 'layer'))//'|'// &
                json_string_value(json_member(v, 'provider'))//'|'// &
                json_string_value(json_member(v, 'provider_layer'))//'|'// &
                json_string_value(json_member(v, 'missing_edge'))//';'
        end do
    end function violations

    subroutine check_text(dir, env, tag)
        character(len=*), intent(in) :: dir, tag
        type(string_list_t), intent(in) :: env
        type(process_result_t) :: result

        call fo(dir, env, [character(len=16) :: 'graph', '--layers'], result)
        call assert_equal_integer(result%exit_code, 1, tag//' text violation exits 1')
        call assert_contains(result%stdout, 'violation: src/model.f90 (layer model)'// &
            ' uses module lay_util from src/util.f90 (layer util); missing edge '// &
            'model -> util', tag//' text names source, module, layers and edge')
        call assert_contains(result%stdout, 'unlayered: app/main.f90', &
            tag//' text lists the unlayered source')
    end subroutine check_text

end program test_project_layering_cli
