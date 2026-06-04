module fo_artifact_cache
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fo_cache, only: HASH_LEN, hash_mod_file
    implicit none
    private
    public :: artifact_cache_dir, artifact_store, artifact_restore

contains

    subroutine artifact_cache_dir(basedir)
        character(len=*), intent(out) :: basedir

        character(len=512) :: home

        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) call get_environment_variable('USERPROFILE', home)
        basedir = trim(home)//'/.cache/fo/objects'
    end subroutine artifact_cache_dir

    subroutine artifact_store(build_dir, ierr)
        character(len=*), intent(in) :: build_dir
        integer, intent(out) :: ierr

        character(len=512) :: basedir, tmpfile, line, objdir
        character(len=HASH_LEN) :: key
        character(len=1024) :: cmd
        integer :: u, iostat

        ierr = 0
        call artifact_cache_dir(basedir)
        call execute_command_line('mkdir -p '//trim(basedir), wait=.true.)

        tmpfile = '/tmp/fo_artifact_store.tmp'

        ! find all .mod files in the build tree
        cmd = 'find '//trim(build_dir)// &
            " -name '*.mod' -type f 2>/dev/null > "//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open(newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read(u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            ! hash the .mod file
            call hash_mod_file(trim(line), key)

            ! store .mod under objects/<key>/
            objdir = trim(basedir)//'/'//key
            call execute_command_line('mkdir -p '//trim(objdir), wait=.true.)
            cmd = 'cp -n '//trim(line)//' '//trim(objdir)//'/ 2>/dev/null'
            call execute_command_line(cmd, wait=.true.)

            ! store matching .o if it exists (same basename)
            block
                character(len=512) :: ofile
                logical :: exists

                ofile = line(1:len_trim(line) - 4)//'.o'
                inquire(file=ofile, exist=exists)
                if (exists) then
                    cmd = 'cp -n '//trim(ofile)//' '//trim(objdir)//'/ 2>/dev/null'
                    call execute_command_line(cmd, wait=.true.)
                end if
            end block
        end do

        close(u)
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
    end subroutine artifact_store

    subroutine artifact_restore(build_dir, n_restored, ierr)
        character(len=*), intent(in) :: build_dir
        integer, intent(out) :: n_restored, ierr

        character(len=512) :: basedir, tmpfile, line, objdir
        character(len=HASH_LEN) :: key
        character(len=1024) :: cmd
        integer :: u, iostat
        logical :: exists

        ierr = 0
        n_restored = 0
        call artifact_cache_dir(basedir)

        inquire(file=trim(basedir)//'/.', exist=exists)
        if (.not. exists) return

        tmpfile = '/tmp/fo_artifact_restore.tmp'

        ! find all .mod files in the build tree
        cmd = 'find '//trim(build_dir)// &
            " -name '*.mod' -type f 2>/dev/null > "//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open(newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read(u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call hash_mod_file(trim(line), key)
            objdir = trim(basedir)//'/'//key

            inquire(file=trim(objdir)//'/.', exist=exists)
            if (exists) then
                ! cached artifacts exist; .mod is already current (same hash)
                ! but check for .o
                block
                    character(len=512) :: ofile, cached_o, mod_basename
                    integer :: last_slash, last_dot
                    logical :: o_exists, cached_o_exists

                    ! extract mod filename
                    last_slash = index(line, '/', back=.true.)
                    mod_basename = line(last_slash+1:)
                    last_dot = index(mod_basename, '.', back=.true.)

                    ofile = line(1:len_trim(line) - 4)//'.o'
                    cached_o = trim(objdir)//'/'// &
                        mod_basename(1:last_dot-1)//'.o'

                    inquire(file=ofile, exist=o_exists)
                    inquire(file=cached_o, exist=cached_o_exists)

                    if (.not. o_exists .and. cached_o_exists) then
                        cmd = 'cp '//trim(cached_o)//' '//trim(ofile)
                        call execute_command_line(cmd, wait=.true.)
                        n_restored = n_restored + 1
                    end if
                end block
            end if
        end do

        close(u)
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
    end subroutine artifact_restore

end module fo_artifact_cache
