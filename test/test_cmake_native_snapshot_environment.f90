program test_cmake_native_snapshot_environment
    !! A frozen native CMake context keeps its environment entries exactly.
    !! The reader formerly built them with cm_variable_t(key, event%string_val),
    !! which GNU Fortran 12 compiles with a wrong component length and which
    !! then corrupts the heap.
    use fo_test_harness, only: make_scratch, join_path, write_text, &
        remove_tree, assert_true, assert_equal_string, assert_equal_integer, &
        finish_assertions
    use fo_cmake_context, only: cmake_context_t, cmake_context_init
    use fo_cmake_native_model, only: cm_plan_t, cm_variable_t
    use fo_cmake_native_snapshot, only: native_snapshot_read, &
        native_snapshot_write
    implicit none
    integer, parameter :: n_env = 3
    character(:), allocatable :: scratch, text
    character(len=96) :: names(n_env), values(n_env)
    type(cmake_context_t) :: context
    type(cm_plan_t) :: written, read_back
    integer :: i

    names = [character(len=96) :: 'SUNDIALS_ARKODE_SPLITTINGSTEP_LONG_NAME', &
        'X', 'CMAKE_PREFIX_PATH_FOR_THE_KAMEL_LIBNEO_RECOVERY']
    values = [character(len=96) :: repeat('/opt/long/prefix', 4), 'y', &
        'value-of-forty-or-more-characters-for-the-heap-check']

    call make_scratch('fo-native-snapshot-env', scratch)
    call cmake_context_init(context, scratch, build_root=join_path(scratch, 'build'))
    allocate (written%environment(n_env))
    do i = 1, n_env
        written%environment(i)%name = trim(names(i))
        written%environment(i)%value = trim(values(i))
    end do
    call native_snapshot_write(context, written, text)
    call write_text(join_path(join_path(scratch, '.fo-cmake'), &
        'native-context.json'), text)

    allocate (read_back%environment(0))
    call native_snapshot_read(context, read_back)
    call assert_true(.not. allocated(read_back%error), 'snapshot reads back')
    call assert_equal_integer(size(read_back%environment), n_env, &
        'every environment entry is read back')
    if (size(read_back%environment) == n_env) then
        do i = 1, n_env
            call assert_equal_string(read_back%environment(i)%name, &
                trim(names(i)), 'environment name')
            call assert_equal_string(read_back%environment(i)%value, &
                trim(values(i)), 'environment value')
        end do
    end if
    call remove_tree(scratch)
    call finish_assertions()
end program test_cmake_native_snapshot_environment
