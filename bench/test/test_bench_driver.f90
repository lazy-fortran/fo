program test_bench_driver
    use, intrinsic :: iso_c_binding, only: c_char,c_int,c_size_t
    use, intrinsic :: iso_fortran_env, only: real64
    use bench_engine, only: run_one, report_jsonl, median_value
    implicit none
    interface
        integer function self_path(buffer,capacity) bind(C,name='fo_bench_self_path')
            import :: c_char,c_size_t
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
        end function
        subroutine sleep_ms(milliseconds) bind(C,name='fo_bench_sleep_ms')
            import :: c_int
            integer(c_int), value :: milliseconds
        end subroutine
    end interface
    character(4096) :: path,fake_args(2)
    character(4096) :: arg
    character(256) :: captured
    character(512) :: log
    integer :: argc,status,exitcode,u,ios,path_end
    real(real64) :: elapsed

    argc=command_argument_count()
    if(argc>0) then
        call get_command_argument(1,arg)
        select case(trim(arg))
        case('--fake-success')
            write(*,'(a)') 'fake success'; stop 0
        case('--fake-failure')
            write(*,'(a)') 'known failure'; stop 7
        case('--fake-delay')
            call sleep_ms(60); write(*,'(a)') 'delayed success'; stop 0
        end select
    end if
    status=self_path(path,int(len(path),c_size_t))
    call assert(status==0,'resolve controlled fake executable')
    log='/var/tmp/fo-bench-oracle.log'
    path_end=index(path,achar(0))
    call assert(path_end>1,'self executable path is terminated')
    fake_args(1)=path(:path_end-1); fake_args(2)='--fake-success'
    call run_one('/var/tmp',fake_args,trim(log),elapsed,status)
    call assert(status==0 .and. elapsed>=0.0_real64,'controlled success exit and monotonic timing')
    fake_args(2)='--fake-delay'
    call run_one('/var/tmp',fake_args,trim(log),elapsed,status)
    call assert(status==0 .and. elapsed>=0.05_real64,'controlled delay is measured')
    fake_args(2)='--fake-failure'
    call run_one('/var/tmp',fake_args,trim(log),elapsed,status)
    call assert(status==7,'failed measured command preserves exact exit status')
    open(newunit=u,file=trim(log),status='old',action='read')
    read(u,'(a)') captured
    read(u,'(a)') captured
    close(u)
    call assert(index(captured,'known failure')>0,'failed child output is preserved exactly')
    call assert(abs(median_value([1.0_real64,3.0_real64,2.0_real64])-2.0_real64)<1.0e-12_real64, &
        'odd median')
    call assert(abs(median_value([4.0_real64,1.0_real64,3.0_real64,2.0_real64])-2.5_real64)< &
        1.0e-12_real64,'even median')

    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"metric":"check","note":"quoted \"text\" and \u00E9","median_s":0.400,"expected_exit":0,"exit_codes":[0,0],"n":2,"times_s":[0.3,0.5],"output_paths":[null,null],"case":"many_tests"}'
    write(u,'(a)') '{"tool":"fake","exit_codes":[0],"expected_exit":0,"case":"bigmod","n":1,"median_s":0.150,"metric":"incremental_leaf","times_s":[0.15],"output_paths":[null]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode==0,'reordered keys and escaped strings parse; target pass')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"case":"bigmod","metric":"incremental_leaf","median_s":0.250,"expected_exit":0,"n":1,"exit_codes":[0],"times_s":[0.25],"output_paths":[null]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'exceeded target fails')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"case":"many_tests","metric":"check","median_s":null,"expected_exit":0,"n":1,"exit_codes":[7],"times_s":[0.1],"output_paths":["/var/tmp/fo-bench-oracle.log"]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'child failure record fails')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{malformed'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'malformed JSONL fails')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'empty JSONL fails')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
    open(newunit=u,file=trim(log),status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
contains
    subroutine assert(condition,message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if(.not.condition) then
            write(*,'(a)') 'FAIL: '//message
            error stop 1
        end if
    end subroutine assert
end program test_bench_driver
