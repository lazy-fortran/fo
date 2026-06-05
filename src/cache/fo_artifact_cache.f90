module fo_artifact_cache
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fx_cache, only: cache_t, fx_cache_init => cache_init, &
                        fx_cache_has => cache_has, &
                        fx_cache_store => cache_store, &
                        fx_cache_restore => cache_restore, &
                        fx_cache_key => cache_key
    use fx_hash, only: fnv1a_file, hash_to_hex
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: artifact_cache_dir, artifact_store, artifact_restore

contains

    subroutine artifact_cache_dir(basedir)
        character(len=*), intent(out) :: basedir

        character(len=512) :: home

        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) call get_environment_variable('USERPROFILE', home)
        basedir = trim(home)//'/.cache/fo/artifacts'
    end subroutine artifact_cache_dir

    subroutine artifact_store(build_dir, ierr)
        character(len=*), intent(in) :: build_dir
        integer, intent(out) :: ierr

        type(cache_t) :: c
        character(len=512) :: basedir, tmpfile, line
        character(len=16) :: hex
        character(len=64) :: mod_key, o_key
        character(len=16) :: parts_1(1)
        character(len=16) :: parts_2(2)
        character(len=1024) :: cmd
        character(len=512) :: ofile
        integer :: u, iostat, cache_ierr
        integer(int64) :: h
        logical :: o_exists
        integer :: hash_ierr

        ierr = 0
        call artifact_cache_dir(basedir)
        call fx_cache_init(c, trim(basedir))

        call make_tmpfile('fo_artifact_store', tmpfile)
        cmd = 'find '//trim(build_dir)// &
              " -name '*.mod' -type f 2>/dev/null > "//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call fnv1a_file(trim(line), h, hash_ierr)
            if (hash_ierr /= 0) cycle
            hex = hash_to_hex(h)

            parts_1(1) = hex
            mod_key = fx_cache_key(parts_1, 1)
            call fx_cache_store(c, trim(mod_key), trim(line), cache_ierr)

            ofile = line(1:len_trim(line) - 4)//'.o'
            inquire (file=trim(ofile), exist=o_exists)
            if (o_exists) then
                parts_2(1) = hex
                parts_2(2) = 'obj'
                o_key = fx_cache_key(parts_2, 2)
                call fx_cache_store(c, trim(o_key), trim(ofile), cache_ierr)
            end if
        end do

        close (u)
        call delete_tmpfile(tmpfile)
    end subroutine artifact_store

    subroutine artifact_restore(build_dir, n_restored, ierr)
        character(len=*), intent(in) :: build_dir
        integer, intent(out) :: n_restored, ierr

        type(cache_t) :: c
        character(len=512) :: basedir, tmpfile, line
        character(len=16) :: hex
        character(len=64) :: o_key
        character(len=16) :: parts_2(2)
        character(len=1024) :: cmd
        character(len=512) :: ofile
        integer :: u, iostat, cache_ierr
        integer(int64) :: h
        logical :: o_exists
        integer :: hash_ierr

        ierr = 0
        n_restored = 0
        call artifact_cache_dir(basedir)
        call fx_cache_init(c, trim(basedir))
        if (.not. c%initialized) return

        call make_tmpfile('fo_artifact_restore', tmpfile)
        cmd = 'find '//trim(build_dir)// &
              " -name '*.mod' -type f 2>/dev/null > "//trim(tmpfile)
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) then
            call delete_tmpfile(tmpfile)
            return
        end if

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle

            call fnv1a_file(trim(line), h, hash_ierr)
            if (hash_ierr /= 0) cycle
            hex = hash_to_hex(h)

            ofile = line(1:len_trim(line) - 4)//'.o'
            inquire (file=trim(ofile), exist=o_exists)
            if (.not. o_exists) then
                parts_2(1) = hex
                parts_2(2) = 'obj'
                o_key = fx_cache_key(parts_2, 2)
                if (fx_cache_has(c, trim(o_key))) then
                    call fx_cache_restore(c, trim(o_key), trim(ofile), cache_ierr)
                    if (cache_ierr == 0) n_restored = n_restored + 1
                end if
            end if
        end do

        close (u)
        call delete_tmpfile(tmpfile)
    end subroutine artifact_restore

end module fo_artifact_cache
