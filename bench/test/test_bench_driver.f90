program test_bench_driver
    use, intrinsic :: iso_c_binding, only: c_char,c_int,c_size_t,c_null_char
    use, intrinsic :: iso_fortran_env, only: real64
    use bench_engine, only: run_one, run_benchmarks, report_jsonl, median_value
    use bench_json, only: json_value_t, json_parse, json_member, json_string_value
    implicit none
    interface
        integer(c_int) function self_path(buffer,capacity) bind(C,name='fo_bench_self_path')
            import :: c_char,c_int,c_size_t
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
        end function
        subroutine sleep_ms(milliseconds) bind(C,name='fo_bench_sleep_ms')
            import :: c_int
            integer(c_int), value :: milliseconds
        end subroutine
        integer(c_int) function create_temp(prefix,buffer,capacity) &
                bind(C,name='fo_bench_create_temp')
            import :: c_char,c_int,c_size_t
            character(kind=c_char), intent(in) :: prefix(*)
            character(kind=c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: capacity
        end function
        integer(c_int) function make_dir(path) bind(C,name='fo_bench_mkdir')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: path(*)
        end function
        integer(c_int) function remove_owned_temp(path) &
                bind(C,name='fo_bench_remove_owned_temp')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: path(*)
        end function
        integer(c_int) function get_process_id() bind(C,name='fo_bench_getpid')
            import :: c_int
        end function
    end interface
    character(4096) :: path,fake_args(2)
    character(4096) :: arg
    character(256) :: cache_before,cache_after
    character(512) :: log
    character(kind=c_char) :: fixture_buffer(512)
    character(:), allocatable :: fixture
    integer :: argc,status,exitcode,u,ios,path_end,line_count,pid,i
    real(real64) :: elapsed
    logical :: valid,found_output
    character(4) :: invalid_numbers(4)
    type(json_value_t) :: unicode_row,unicode_note
    character(:), allocatable :: unicode_text,unicode_message

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
        case('build','test')
            write(*,'(a)') 'controlled fo success'; stop 0
        case('check')
            block
                logical :: is_diagnostic,is_unexpected
                character(32) :: option
                call get_command_argument(2,option)
                inquire(file='.fo-bench-expect-failure',exist=is_diagnostic)
                inquire(file='.fo-bench-unexpected-failure',exist=is_unexpected)
                if(is_diagnostic .and. trim(option)=='--json') then
                    write(*,'(a)') 'controlled diagnostic output'; stop 1
                end if
                if(is_unexpected) then
                    write(*,'(a)') 'controlled unexpected failure'; stop 7
                end if
                write(*,'(a)') 'controlled fo success'; stop 0
            end block
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
    found_output=.false.
    do
        read(u,'(a)',iostat=ios) arg
        if(ios/=0) exit
        if(index(arg,'known failure')>0) found_output=.true.
    end do
    close(u)
    call assert(found_output,'failed child output is preserved exactly')
    call assert(abs(median_value([1.0_real64,3.0_real64,2.0_real64])-2.0_real64)<1.0e-12_real64, &
        'odd median')
    call assert(abs(median_value([4.0_real64,1.0_real64,3.0_real64,2.0_real64])-2.5_real64)< &
        1.0e-12_real64,'even median')

    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"metric":"check","note":"quoted \"text\" and \uD83D\uDE00",'// &
        '"median_s":0.400,"expected_exit":0,"exit_codes":[0,0],"n":2,'// &
        '"times_s":[0.3,0.5],"output_paths":[null,null],"case":"many_tests"}'
    write(u,'(a)') '{"tool":"fake","exit_codes":[0],"expected_exit":0,'// &
        '"case":"bigmod","n":1,"median_s":0.150,'// &
        '"metric":"metadata_touch_leaf","times_s":[0.15],"output_paths":[null]}'
    close(u)
    call json_parse('{"note":"quote \" and \uD83D\uDE00"}',unicode_row,valid,unicode_message)
    call assert(valid,'escaped quote and valid surrogate pair parse')
    unicode_note=json_member(unicode_row,'note')
    unicode_text=json_string_value(unicode_note)
    call assert(index(unicode_text,'quote "')>0,'escaped quote decodes exactly')
    call assert(len(unicode_text)>=4,'decoded scalar has four UTF-8 bytes')
    if(len(unicode_text)>=4) then
        call assert(iachar(unicode_text(len(unicode_text)-3:len(unicode_text)-3))==240, &
            'surrogate pair UTF-8 lead byte')
        call assert(iachar(unicode_text(len(unicode_text)-2:len(unicode_text)-2))==159, &
            'surrogate pair UTF-8 second byte')
        call assert(iachar(unicode_text(len(unicode_text)-1:len(unicode_text)-1))==152, &
            'surrogate pair UTF-8 third byte')
        call assert(iachar(unicode_text(len(unicode_text):len(unicode_text)))==128, &
            'surrogate pair UTF-8 final byte')
    end if
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode==0,'reordered keys and escaped strings parse; target pass')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"case":"bigmod","metric":"metadata_touch_leaf",'// &
        '"median_s":0.250,"expected_exit":0,"n":1,"exit_codes":[0],'// &
        '"times_s":[0.25],"output_paths":[null]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode==0,'exceeded timing target is advisory when evidence is valid')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"case":"many_tests","metric":"check","median_s":null,'// &
        '"expected_exit":0,"n":1,"exit_codes":[7],"times_s":[0.1],'// &
        '"output_paths":["/var/tmp/fo-bench-oracle.log"]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'child failure record fails')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{malformed'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'malformed JSONL fails')
    invalid_numbers(1)='01'; invalid_numbers(2)='1.'
    invalid_numbers(3)='1e+'; invalid_numbers(4)='+1'
    do i=1,size(invalid_numbers)
        open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
        write(u,'(a)') '{"case":"many_tests","metric":"check","median_s":'// &
            trim(invalid_numbers(i))//',"expected_exit":0,"n":1,"exit_codes":[0],'// &
            '"times_s":[1],"output_paths":[null]}'
        close(u)
        call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
        call assert(exitcode/=0,'invalid JSON number grammar is rejected: '//trim(invalid_numbers(i)))
    end do
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    write(u,'(a)') '{"case":"many_tests","metric":"check",'// &
        '"note":"\uD83D\u0041","median_s":1,"expected_exit":0,'// &
        '"n":1,"exit_codes":[0],"times_s":[1],"output_paths":[null]}'
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'invalid surrogate pair is rejected')
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='replace',action='write')
    close(u)
    call report_jsonl('/var/tmp/fo-bench-oracle.jsonl',exitcode)
    call assert(exitcode/=0,'empty JSONL fails')

    status=create_temp('/var/tmp/fo-bench-fixture-',fixture_buffer, &
        int(size(fixture_buffer),c_size_t))
    call assert(status==0,'create unique generated-output fixture directory')
    fixture=c_string(fixture_buffer)
    call make_fixture(fixture)
    call run_benchmarks(path(:path_end-1),1,'/var/tmp/fo-bench-generated.jsonl',fixture,exitcode)
    call assert(exitcode==0,'controlled fo run produces all expected benchmark outcomes')
    call report_jsonl('/var/tmp/fo-bench-generated.jsonl',exitcode,.true.)
    call assert(exitcode==0,'generated JSONL contains the complete metric inventory')
    open(newunit=u,file='/var/tmp/fo-bench-generated.jsonl',status='old',action='read')
    line_count=0
    do
        read(u,'(a)',iostat=ios) arg
        if(ios/=0) exit
        line_count=line_count+1
    end do
    close(u)
    call assert(line_count==8,'generated JSONL has exactly eight benchmark rows')
    call get_environment_variable('FO_CACHE_DIR',cache_after)
    inquire(file=trim(cache_after),exist=valid)
    call assert(.not.valid,'unique benchmark cache is removed after successful run')
    cache_before=cache_after
    open(newunit=u,file=fixture//'/many_tests/.fo-bench-unexpected-failure',status='replace')
    write(u,'(a)') 'unexpected child failure'
    close(u)
    call run_benchmarks(path(:path_end-1),1,'/var/tmp/fo-bench-failed.jsonl',fixture,exitcode)
    call assert(exitcode/=0,'unexpected measured child failure makes runner fail')
    call report_jsonl('/var/tmp/fo-bench-failed.jsonl',exitcode,.true.)
    call assert(exitcode/=0,'failed generated rows retain explicit failure status')
    call get_environment_variable('FO_CACHE_DIR',cache_after)
    call assert(trim(cache_before)/=trim(cache_after),'cache path is unique after command failure')
    inquire(file=trim(cache_after),exist=valid)
    call assert(.not.valid,'unique benchmark cache is removed after measured failure')
    call run_benchmarks(path(:path_end-1),1,'/no/such/fo-bench/output.jsonl',fixture,exitcode)
    call assert(exitcode/=0,'output-open failure is reported')
    call get_environment_variable('FO_CACHE_DIR',cache_after)
    call assert(trim(cache_before)/=trim(cache_after),'cache path is unique across runs in one PID')
    inquire(file=trim(cache_after),exist=valid)
    call assert(.not.valid,'unique benchmark cache is removed after early failure')
    status=remove_owned_temp(fixture//c_null_char)
    call assert(status==0,'remove owned workload fixture')
    open(newunit=u,file='/var/tmp/fo-bench-generated.jsonl',status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
    open(newunit=u,file='/var/tmp/fo-bench-failed.jsonl',status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
    open(newunit=u,file=trim(cache_before),status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
    block
        character(512) :: diagnostic_log
        pid=get_process_id()
        write(diagnostic_log,'(a,i0,a)') '/var/tmp/fo-bench-',pid,'-diag_latency-1.log'
        call remove_log(diagnostic_log)
        write(diagnostic_log,'(a,i0,a)') '/var/tmp/fo-bench-',pid,'-check_json-1.log'
        call remove_log(diagnostic_log)
        write(diagnostic_log,'(a,i0,a)') '/var/tmp/fo-bench-',pid,'-check-1.log'
        call remove_log(diagnostic_log)
    end block
    open(newunit=u,file='/var/tmp/fo-bench-oracle.jsonl',status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
    open(newunit=u,file=trim(log),status='old',iostat=ios)
    if(ios==0) close(u,status='delete')
contains
    subroutine make_fixture(root)
        character(len=*), intent(in) :: root
        integer(c_int) :: rc
        integer :: unit
        rc=make_dir(root//'/many_tests'//c_null_char)
        call assert(rc==0,'create many_tests fixture')
        rc=make_dir(root//'/bigmod'//c_null_char)
        call assert(rc==0,'create bigmod fixture')
        rc=make_dir(root//'/bigmod/src'//c_null_char)
        call assert(rc==0,'create bigmod source fixture')
        rc=make_dir(root//'/diagnostics'//c_null_char)
        call assert(rc==0,'create diagnostics fixture')
        open(newunit=unit,file=root//'/bigmod/src/leaf_1.f90',status='replace')
        write(unit,'(a)') 'module leaf_1; end module'
        close(unit)
        open(newunit=unit,file=root//'/bigmod/src/core.f90',status='replace')
        write(unit,'(a)') 'module core; end module'
        close(unit)
        open(newunit=unit,file=root//'/diagnostics/.fo-bench-expect-failure',status='replace')
        write(unit,'(a)') 'expected failure'
        close(unit)
    end subroutine make_fixture

    function c_string(buffer) result(value)
        character(kind=c_char), intent(in) :: buffer(:)
        character(:), allocatable :: value
        integer :: i
        value=''
        do i=1,size(buffer)
            if(buffer(i)==c_null_char) exit
            value=value//buffer(i)
        end do
    end function c_string

    subroutine remove_log(path_name)
        character(len=*), intent(in) :: path_name
        integer :: unit,open_status
        open(newunit=unit,file=trim(path_name),status='old',iostat=open_status)
        if(open_status==0) close(unit,status='delete')
    end subroutine remove_log

    subroutine assert(condition,message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if(.not.condition) then
            write(*,'(a)') 'FAIL: '//message
            error stop 1
        end if
    end subroutine assert
end program test_bench_driver
