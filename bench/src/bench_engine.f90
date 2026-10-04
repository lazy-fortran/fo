module bench_engine
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_size_t, c_null_char
    use, intrinsic :: iso_fortran_env, only: real64, output_unit, error_unit
    use bench_json, only: json_value_t, json_object, json_array, json_number, json_string
    use bench_json, only: json_parse, json_member, json_number_value, json_string_value
    implicit none
    private
    public :: run_benchmarks, run_one, report_jsonl, median_value

    interface
        integer(c_int64_t) function monotonic_ns() bind(C,name='fo_bench_monotonic_ns')
            import :: c_int64_t
        end function
        integer(c_int) function set_env(name,value) bind(C,name='fo_bench_setenv')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: name(*),value(*)
        end function
        integer(c_int) function create_temp(prefix,path,capacity) bind(C,name='fo_bench_create_temp')
            import :: c_char,c_int,c_size_t
            character(kind=c_char), intent(in) :: prefix(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_size_t), value :: capacity
        end function
        integer(c_int) function remove_owned_temp(path) &
                bind(C,name='fo_bench_remove_owned_temp')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: path(*)
        end function
        integer(c_int) function current_pid() bind(C,name='fo_bench_getpid')
            import :: c_int
        end function
        integer(c_int) function touch_file(path) bind(C,name='fo_bench_touch')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: path(*)
        end function
        integer(c_int) function run_argv(cwd,blob,argc,logfile,timeout_s) &
                bind(C,name='fo_bench_run_argv')
            import :: c_char,c_int
            character(kind=c_char), intent(in) :: cwd(*),blob(*),logfile(*)
            integer(c_int), value :: argc,timeout_s
        end function
    end interface
contains
    subroutine run_benchmarks(fo,reps,output,workloads,exitcode)
        character(len=*), intent(in) :: fo,output,workloads
        integer, intent(in) :: reps
        integer, intent(out) :: exitcode
        character(:), allocatable :: cache
        character(kind=c_char) :: cache_buffer(256)
        character(len=4096) :: args(3)
        integer :: ios,u,failures
        integer(c_int) :: rc
        if(reps<1 .or. reps>1000) then
            write(error_unit,'(a)') 'fo bench: repetitions must be between 1 and 1000'
            exitcode=1; return
        end if
        rc=create_temp('/var/tmp/fo-bench-cache-'//c_null_char,cache_buffer, &
            int(size(cache_buffer),c_size_t))
        if(rc/=0) then
            write(error_unit,'(a)') 'fo bench: cannot create isolated cache directory'
            exitcode=1; return
        end if
        cache=c_string(cache_buffer)
        rc=set_env('FO_CACHE_DIR'//c_null_char,cache//c_null_char)
        if(rc==0) rc=set_env('FO_DISABLE_SELF_REFRESH'//c_null_char,'1'//c_null_char)
        if(rc==0) rc=set_env('FO_SELF_REFRESH'//c_null_char,'0'//c_null_char)
        if(rc==0) rc=set_env('TMPDIR'//c_null_char,'/var/tmp'//c_null_char)
        if(rc/=0) then
            write(error_unit,'(a)') 'fo bench: cannot set isolated fo environment'
            call discard_cache(cache,exitcode)
            return
        end if
        if(trim(output)=='/dev/stdout') then
            u=output_unit
        else
            open(newunit=u,file=output,status='replace',action='write',iostat=ios)
            if(ios/=0) then
                write(error_unit,'(a)') 'fo bench: cannot open JSONL output: '//trim(output)
                exitcode=1
                call discard_cache(cache,exitcode)
                return
            end if
        end if
        failures=0
        call warm('many_tests',workloads,fo,failures)
        args(1)=fo; args(2)='check'; args(3)='--json'
        call measure(u,'many_tests','fo','check_json',workloads//'/many_tests', &
            args(:3),reps,failures)
        args(2)='test'
        call measure(u,'many_tests','fo','test',workloads//'/many_tests', &
            args(:2),reps,failures)
        args(2)='check'
        call measure(u,'many_tests','fo','check',workloads//'/many_tests', &
            args(:2),reps,failures)
        call warm('bigmod',workloads,fo,failures)
        args(2)='check'; args(3)='--json'
        call measure(u,'bigmod','fo','check_json',workloads//'/bigmod', &
            args(:3),reps,failures)
        args(2)='build'
        call measure(u,'bigmod','fo','build',workloads//'/bigmod', &
            args(:2),reps,failures)
        call measured_touch(u,'bigmod','incremental_leaf',workloads//'/bigmod', &
            fo,'src/leaf_1.f90',reps,failures)
        call measured_touch(u,'bigmod','incremental_core',workloads//'/bigmod', &
            fo,'src/core.f90',reps,failures)
        args(2)='check'; args(3)='--json'
        call measure(u,'diagnostics','fo','diag_latency',workloads//'/diagnostics', &
            args(:3),reps,failures,expected_exit=1)
        if(trim(output)/='/dev/stdout') close(u)
        rc=remove_owned_temp(cache//c_null_char)
        if(rc/=0) then
            write(error_unit,'(a,i0,a)') 'fo bench: failed to remove owned cache (errno ',rc,')'
            failures=failures+1
        end if
        if(failures>0) then
            write(error_unit,'(a,i0,a)') 'fo bench: ',failures,' command(s) failed; see JSONL evidence'
            exitcode=1
        else
            exitcode=0
        end if
    end subroutine run_benchmarks

    subroutine discard_cache(cache,exitcode)
        character(len=*), intent(in) :: cache
        integer, intent(inout) :: exitcode
        integer(c_int) :: rc
        rc=remove_owned_temp(cache//c_null_char)
        if(rc/=0) then
            write(error_unit,'(a,i0,a)') 'fo bench: failed to remove owned cache (errno ',rc,')'
            exitcode=1
        end if
    end subroutine discard_cache

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

    subroutine warm(name,root,fo,failures)
        character(len=*), intent(in) :: name,root,fo
        integer, intent(inout) :: failures
        integer(c_int) :: status
        character(:), allocatable :: log
        log='/var/tmp/fo-bench-warm-'//integer_text(int(current_pid()))//'-'//name//'.log'
        status=run_argv(root//'/'//name//c_null_char,fo//achar(1)// &
            'build'//achar(1)//c_null_char,2_c_int,log//c_null_char,300_c_int)
        if(status/=0) then
            failures=failures+1
            write(error_unit,'(a,a,a,i0,a,a)') 'fo bench: warm build failed for ', &
                name,', exit=',status,', output=',trim(log)
        else
            call remove_file(log)
        end if
    end subroutine warm

    subroutine measure(u,case_name,tool,metric,cwd,args,reps,failures,expected_exit)
        integer, intent(in) :: u,reps
        character(len=*), intent(in) :: case_name,tool,metric,cwd
        character(len=*), intent(in) :: args(:)
        integer, intent(inout) :: failures
        integer, intent(in), optional :: expected_exit
        real(real64), allocatable :: times(:)
        integer, allocatable :: exits(:)
        character(512), allocatable :: logs(:)
        character(32) :: rep_text
        integer :: i,pid,expected
        allocate(times(reps),exits(reps),logs(reps))
        pid=int(current_pid())
        expected=0
        if(present(expected_exit)) expected=expected_exit
        do i=1,reps
            write(rep_text,'(i0)') i
            logs(i)='/var/tmp/fo-bench-'//integer_text(pid)//'-'//metric//'-'//trim(rep_text)//'.log'
            call run_one(cwd,args,trim(logs(i)),times(i),exits(i))
            if(exits(i)/=expected) failures=failures+1
            if(exits(i)==0) then
                call remove_file(trim(logs(i)))
                logs(i)=''
            end if
        end do
        call write_record(u,case_name,tool,metric,times,exits,logs,expected)
    end subroutine measure

    subroutine measured_touch(u,case_name,metric,cwd,fo,source,reps,failures)
        integer, intent(in) :: u,reps
        character(len=*), intent(in) :: case_name,metric,cwd,fo,source
        integer, intent(inout) :: failures
        real(real64), allocatable :: times(:)
        integer, allocatable :: exits(:)
        character(512), allocatable :: logs(:)
        character(32) :: rep_text
        character(len=len(fo)) :: args(2)
        integer :: i,pid
        integer(c_int) :: rc
        allocate(times(reps),exits(reps),logs(reps))
        pid=int(current_pid())
        args(1)=fo; args(2)='build'
        do i=1,reps
            write(rep_text,'(i0)') i
            logs(i)='/var/tmp/fo-bench-'//integer_text(pid)//'-'//metric//'-'//trim(rep_text)//'.log'
            rc=touch_file(cwd//'/'//source//c_null_char)
            if(rc/=0) then
                times(i)=0.0_real64; exits(i)=rc
            else
                call run_one(cwd,args,trim(logs(i)),times(i),exits(i))
            end if
            if(exits(i)/=0) failures=failures+1
            if(exits(i)==0) then
                call remove_file(trim(logs(i)))
                logs(i)=''
            end if
        end do
        call write_record(u,case_name,'fo',metric,times,exits,logs,0)
    end subroutine measured_touch

    subroutine run_one(cwd,args,log,seconds,exitcode)
        character(len=*), intent(in) :: cwd,args(:),log
        real(real64), intent(out) :: seconds
        integer, intent(out) :: exitcode
        character(:), allocatable :: blob
        integer(c_int64_t) :: start,finish
        integer(c_int) :: rc
        integer :: i
        blob=''
        do i=1,size(args)
            blob=blob//trim(args(i))//achar(1)
        end do
        start=monotonic_ns()
        rc=run_argv(cwd//c_null_char,blob//c_null_char,int(size(args),c_int), &
            log//c_null_char,300_c_int)
        finish=monotonic_ns()
        exitcode=int(rc)
        seconds=real(max(0_c_int64_t,finish-start),real64)/1.0e9_real64
        if(start<0 .or. finish<0) then
            exitcode=125; seconds=0.0_real64
        end if
    end subroutine run_one

    subroutine write_record(u,case_name,tool,metric,times,exits,logs,expected_exit)
        integer, intent(in) :: u
        character(len=*), intent(in) :: case_name,tool,metric
        real(real64), intent(in) :: times(:)
        integer, intent(in) :: exits(:)
        character(len=*), intent(in) :: logs(:)
        integer, intent(in) :: expected_exit
        character(32) :: median_text
        integer :: i
        logical :: ok
        ok=all(exits==expected_exit)
        if(ok) then
            write(median_text,'(f24.9)') median_value(times)
        else
            median_text='null'
        end if
        write(u,'(a)',advance='no') '{"case":"'//case_name//'","tool":"'//tool// &
            '","metric":"'//metric//'","median_s":'//trim(median_text)// &
            ',"n":'//integer_text(size(times))//',"expected_exit":'// &
            integer_text(expected_exit)//',"exit_codes":['
        do i=1,size(exits)
            if(i>1) write(u,'(a)',advance='no') ','
            write(u,'(i0)',advance='no') exits(i)
        end do
        write(u,'(a)',advance='no') '],"times_s":['
        do i=1,size(times)
            if(i>1) write(u,'(a)',advance='no') ','
            write(u,'(f24.9)',advance='no') times(i)
        end do
        write(u,'(a)',advance='no') '],"output_paths":['
        do i=1,size(logs)
            if(i>1) write(u,'(a)',advance='no') ','
            if(len_trim(logs(i))==0) then
                write(u,'(a)',advance='no') 'null'
            else
                write(u,'(a)',advance='no') '"'//trim(logs(i))//'"'
            end if
        end do
        write(u,'(a)') ']}'
    end subroutine write_record

    function integer_text(n) result(s)
        integer, intent(in) :: n
        character(:), allocatable :: s
        character(32) :: buffer
        write(buffer,'(i0)') n; s=trim(buffer)
    end function integer_text

    real(real64) function median_value(values) result(median)
        real(real64), intent(in) :: values(:)
        real(real64), allocatable :: sorted(:)
        real(real64) :: item
        integer :: i,j,n
        n=size(values); median=0.0_real64; if(n==0) return
        sorted=values
        do i=2,n
            item=sorted(i); j=i-1
            do while(j>=1)
                if(sorted(j)<=item) exit
                sorted(j+1)=sorted(j); j=j-1
            end do
            sorted(j+1)=item
        end do
        if(mod(n,2)==1) then
            median=sorted((n+1)/2)
        else
            median=(sorted(n/2)+sorted(n/2+1))/2.0_real64
        end if
    end function median_value

    subroutine report_jsonl(path,exitcode,require_complete)
        character(len=*), intent(in) :: path
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: require_complete
        character(1048576) :: line
        character(:), allocatable :: message,case_name,metric,status
        character(8) :: target_text
        type(json_value_t) :: row,case_field,metric_field,median_field,exit_field
        type(json_value_t) :: n_field,times_field,outputs_field,output_item,expected_field
        logical :: valid,all_pass,has_exit_failure,samples_valid,output_exists
        logical :: complete,seen(8)
        integer :: u,ios,count,i,n_value,expected_exit,inventory_id
        real(real64) :: median,target,derived_median,n_real,expected_real
        all_pass=.true.; count=0; seen=.false.; complete=.false.
        if(present(require_complete)) complete=require_complete
        open(newunit=u,file=path,status='old',action='read',iostat=ios)
        if(ios/=0) then
            write(*,'(a)') 'fo bench report: cannot open input: '//trim(path)
            exitcode=1; return
        end if
        write(*,'(a)') 'case             metric                median_s     target   status'
        write(*,'(a)') '----------------------------------------------------------------------'
        do
            read(u,'(a)',iostat=ios) line
            if(ios<0) exit
            if(ios/=0) then
                all_pass=.false.; exit
            end if
            if(len_trim(line)==0) cycle
            call json_parse(trim(line),row,valid,message)
            if(.not.valid) then
                write(*,'(a)') 'fo bench report: malformed JSONL row: '//message
                all_pass=.false.; exit
            end if
            if(row%kind/=json_object) then
                write(*,'(a)') 'fo bench report: each JSONL row must be an object'
                all_pass=.false.; exit
            end if
            case_field=json_member(row,'case'); metric_field=json_member(row,'metric')
            median_field=json_member(row,'median_s'); exit_field=json_member(row,'exit_codes')
            n_field=json_member(row,'n'); times_field=json_member(row,'times_s')
            outputs_field=json_member(row,'output_paths')
            expected_field=json_member(row,'expected_exit')
            case_name=json_string_value(case_field); metric=json_string_value(metric_field)
            if(len(case_name)==0 .or. len(metric)==0 .or. exit_field%kind/=json_array .or. &
                times_field%kind/=json_array .or. outputs_field%kind/=json_array .or. &
                n_field%kind/=json_number .or. expected_field%kind/=json_number) then
                write(*,'(a)') 'fo bench report: missing required fields'
                all_pass=.false.; exit
            end if
            if(complete) then
                inventory_id=inventory_index(case_name,metric)
                if(inventory_id==0) then
                    write(*,'(a)') 'fo bench report: unexpected benchmark metric in complete inventory'
                    all_pass=.false.; exit
                end if
                if(seen(inventory_id)) then
                    write(*,'(a)') 'fo bench report: duplicate benchmark metric in complete inventory'
                    all_pass=.false.; exit
                end if
                seen(inventory_id)=.true.
            end if
            n_real=json_number_value(n_field)
            expected_real=json_number_value(expected_field)
            if(n_real<1.0_real64 .or. n_real>1000.0_real64 .or. &
                expected_real<0.0_real64 .or. expected_real>255.0_real64) then
                write(*,'(a)') 'fo bench report: invalid repetition or expected exit count'
                all_pass=.false.; exit
            end if
            n_value=nint(n_real); expected_exit=nint(expected_real)
            if(abs(expected_real-real(expected_exit,real64))>1.0e-12_real64 .or. &
                abs(n_real-real(n_value,real64))>1.0e-12_real64 .or. &
                .not.allocated(times_field%children) .or. &
                .not.allocated(exit_field%children) .or. .not.allocated(outputs_field%children)) then
                write(*,'(a)') 'fo bench report: invalid repetition evidence'
                all_pass=.false.; exit
            end if
            if(size(times_field%children)/=n_value .or. size(exit_field%children)/=n_value .or. &
                size(outputs_field%children)/=n_value) then
                write(*,'(a)') 'fo bench report: repetition count does not match evidence arrays'
                all_pass=.false.; exit
            end if
            block
                real(real64), allocatable :: samples(:)
                allocate(samples(n_value))
                samples_valid=.true.
                do i=1,n_value
                    if(times_field%children(i)%kind/=json_number) then
                        samples_valid=.false.
                    else
                        samples(i)=json_number_value(times_field%children(i))
                        if(samples(i)<0.0_real64) samples_valid=.false.
                    end if
                end do
                if(samples_valid) derived_median=median_value(samples)
            end block
            if(.not.samples_valid) then
                write(*,'(a)') 'fo bench report: nonnumeric timing sample'
                all_pass=.false.; exit
            end if
            has_exit_failure=.false.
            if(allocated(exit_field%children)) then
                do i=1,size(exit_field%children)
                    if(exit_field%children(i)%kind/=json_number) then
                        has_exit_failure=.true.
                    end if
                    output_item=outputs_field%children(i)
                    if(exit_field%children(i)%kind==json_number) then
                        if(json_number_value(exit_field%children(i))/=real(expected_exit,real64)) then
                            has_exit_failure=.true.
                        end if
                        if(json_number_value(exit_field%children(i))/=0.0_real64) then
                            if(output_item%kind/=json_string) then
                                has_exit_failure=.true.
                            else
                                inquire(file=output_item%text,exist=output_exists)
                                if(.not.output_exists) has_exit_failure=.true.
                            end if
                        end if
                    end if
                end do
            end if
            target=target_for(case_name,metric)
            if(median_field%kind==json_number) then
                median=json_number_value(median_field)
                if(abs(median-derived_median)>0.00000051_real64) then
                    write(*,'(a)') 'fo bench report: recorded median does not match samples'
                    all_pass=.false.; exit
                end if
            else
                median=0.0_real64; has_exit_failure=.true.
            end if
            status='-'; target_text='-'
            if(target>=0.0_real64) then
                write(target_text,'(f8.3)') target
                if(has_exit_failure) then
                    status='FAIL'; all_pass=.false.
                else if(median<=target) then
                    status='PASS'
                else
                    status='FAIL'; all_pass=.false.
                end if
            else if(has_exit_failure) then
                status='FAIL'; all_pass=.false.
            end if
            write(*,'(a16,1x,a20,1x,f10.3,1x,a8,1x,a8)') case_name,metric,median, &
                trim(target_text),trim(status)
            count=count+1
        end do
        close(u)
        if(complete .and. .not.all(seen)) then
            write(*,'(a)') 'fo bench report: incomplete benchmark metric inventory'
            all_pass=.false.
        end if
        if(count==0) then
            write(*,'(a)') 'fo bench report: no results'
            exitcode=1
        else if(all_pass) then
            write(*,'(a)') 'All targets met.'
            exitcode=0
        else
            write(*,'(a)') 'Benchmark evidence failed validation or a target.'
            exitcode=1
        end if
    end subroutine report_jsonl

    integer function inventory_index(case_name,metric) result(index_value)
        character(len=*), intent(in) :: case_name,metric
        index_value=0
        select case(case_name//':'//metric)
        case('many_tests:check_json'); index_value=1
        case('many_tests:test'); index_value=2
        case('many_tests:check'); index_value=3
        case('bigmod:check_json'); index_value=4
        case('bigmod:build'); index_value=5
        case('bigmod:incremental_leaf'); index_value=6
        case('bigmod:incremental_core'); index_value=7
        case('diagnostics:diag_latency'); index_value=8
        end select
    end function inventory_index

    real(real64) function target_for(case_name,metric) result(target)
        character(len=*), intent(in) :: case_name,metric
        target=-1.0_real64
        select case(case_name//':'//metric)
        case('many_tests:check_json','bigmod:check_json'); target=0.100_real64
        case('many_tests:check'); target=0.500_real64
        case('bigmod:incremental_leaf'); target=0.200_real64
        case('diagnostics:diag_latency'); target=0.200_real64
        end select
    end function target_for

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: u,ios
        open(newunit=u,file=path,status='old',iostat=ios)
        if(ios==0) close(u,status='delete')
    end subroutine remove_file
end module bench_engine
