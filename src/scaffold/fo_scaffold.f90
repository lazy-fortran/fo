module fo_scaffold
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none
    private
    public :: scaffold_project

contains

    subroutine scaffold_project(path, name, is_library, ierr)
        character(len=*), intent(in) :: path, name
        logical, intent(in) :: is_library
        integer, intent(out) :: ierr

        character(len=512) :: app_dir, src_dir, test_dir, main_file, lib_file, &
            toml_file, readme_file, gitignore_file
        logical :: exists

        ierr = 0

        inquire (file=trim(path)//'/.', exist=exists)
        if (exists) then
            if (.not. is_empty_dir(trim(path))) then
                write (error_unit, '(a)') 'fo: '//trim(path)//' exists and is not empty'
                ierr = 1
                return
            end if
        end if

        app_dir = trim(path)//'/app'
        src_dir = trim(path)//'/src'
        test_dir = trim(path)//'/test'

        if (.not. is_library) then
            call execute_command_line('mkdir -p '//trim(app_dir), wait=.true., &
                exitstat=ierr)
            if (ierr /= 0) return
        end if

        call execute_command_line('mkdir -p '//trim(src_dir), wait=.true., exitstat=ierr)
        if (ierr /= 0) return

        call execute_command_line('mkdir -p '//trim(test_dir), wait=.true., exitstat=ierr)
        if (ierr /= 0) return

        toml_file = trim(path)//'/fpm.toml'
        call write_fpm_toml(trim(toml_file), trim(name), ierr)
        if (ierr /= 0) return

        if (.not. is_library) then
            main_file = trim(app_dir)//'/main.f90'
            call write_main_f90(trim(main_file), trim(name), ierr)
            if (ierr /= 0) return
        end if

        lib_file = trim(src_dir)//'/lib.f90'
        call write_lib_f90(trim(lib_file), trim(name), ierr)
        if (ierr /= 0) return

        call write_test_f90(trim(test_dir)//'/test_'//trim(name)//'.f90', &
            trim(name), ierr)
        if (ierr /= 0) return

        gitignore_file = trim(path)//'/.gitignore'
        call write_gitignore(trim(gitignore_file), ierr)
        if (ierr /= 0) return

        readme_file = trim(path)//'/README.md'
        call write_readme(trim(readme_file), trim(name), ierr)
        if (ierr /= 0) return

        write (output_unit, '(a)') 'Created project: '//trim(name)

    end subroutine scaffold_project

    logical function is_empty_dir(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios, n_items
        character(len=512) :: cmd, tmpfile

        tmpfile = '/tmp/fo_empty_check.tmp'
        write (cmd, '(a)') 'find '//trim(path)// &
            ' -mindepth 1 -maxdepth 1 2>/dev/null | wc -l > '//trim(tmpfile)

        call execute_command_line(trim(cmd), wait=.true.)

        open (newunit=unit, file=trim(tmpfile), status='old', iostat=ios)
        if (ios == 0) then
            read (unit, *, iostat=ios) n_items
            close (unit, status='delete')
            is_empty_dir = (n_items == 0)
        else
            is_empty_dir = .true.
        end if
    end function is_empty_dir

    subroutine write_fpm_toml(path, name, ierr)
        character(len=*), intent(in) :: path, name
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') 'name = "'//trim(name)//'"'
        write (unit, '(a)') 'version = "0.1.0"'
        write (unit, '(a)') 'license = "MIT"'
        write (unit, '(a)') ''
        write (unit, '(a)') '[build]'
        write (unit, '(a)') 'auto-executables = true'
        write (unit, '(a)') 'auto-tests = true'
        write (unit, '(a)') 'auto-examples = false'
        write (unit, '(a)') 'module-naming = false'
        write (unit, '(a)') ''
        write (unit, '(a)') '[fortran]'
        write (unit, '(a)') 'implicit-typing = false'
        write (unit, '(a)') 'implicit-external = false'
        write (unit, '(a)') 'source-form = "free"'

        close (unit)
    end subroutine write_fpm_toml

    subroutine write_main_f90(path, name, ierr)
        character(len=*), intent(in) :: path, name
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') 'program main'
        write (unit, '(a)') '    use lib_'//trim(name)//', only: greet'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') ''
        write (unit, '(a)') '    call greet()'
        write (unit, '(a)') ''
        write (unit, '(a)') 'end program main'

        close (unit)
    end subroutine write_main_f90

    subroutine write_lib_f90(path, name, ierr)
        character(len=*), intent(in) :: path, name
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') 'module lib_'//trim(name)
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    private'
        write (unit, '(a)') '    public :: greet'
        write (unit, '(a)') ''
        write (unit, '(a)') 'contains'
        write (unit, '(a)') ''
        write (unit, '(a)') '    subroutine greet()'
        write (unit, '(a)') '        use, intrinsic :: iso_fortran_env, only: output_unit'
        write (unit, '(a)') '        write (output_unit, ''(a)'') ''Hello from ' &
            //trim(name)//''''
        write (unit, '(a)') '    end subroutine greet'
        write (unit, '(a)') ''
        write (unit, '(a)') 'end module lib_'//trim(name)

        close (unit)
    end subroutine write_lib_f90

    subroutine write_test_f90(path, name, ierr)
        character(len=*), intent(in) :: path, name
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') 'program test_'//trim(name)
        write (unit, '(a)') '    use lib_'//trim(name)//', only: greet'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') ''
        write (unit, '(a)') '    call greet()'
        write (unit, '(a)') ''
        write (unit, '(a)') 'end program test_'//trim(name)

        close (unit)
    end subroutine write_test_f90

    subroutine write_gitignore(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') '/build'
        write (unit, '(a)') '/*.mod'
        write (unit, '(a)') '/*.smod'
        write (unit, '(a)') '/*.o'
        write (unit, '(a)') '.vscode/'
        write (unit, '(a)') '.DS_Store'

        close (unit)
    end subroutine write_gitignore

    subroutine write_readme(path, name, ierr)
        character(len=*), intent(in) :: path, name
        integer, intent(out) :: ierr

        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ierr)
        if (ierr /= 0) return

        write (unit, '(a)') '# '//trim(name)
        write (unit, '(a)') ''
        write (unit, '(a)') 'A Fortran project.'
        write (unit, '(a)') ''
        write (unit, '(a)') '## Build'
        write (unit, '(a)') ''
        write (unit, '(a)') '```bash'
        write (unit, '(a)') 'fo build'
        write (unit, '(a)') 'fo test'
        write (unit, '(a)') '```'

        close (unit)
    end subroutine write_readme

end module fo_scaffold
