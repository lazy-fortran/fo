program fo_bench
    use bench_engine, only: run_benchmarks, report_jsonl
    implicit none
    character(32) :: command,reps_text
    character(4096) :: fo,output,workloads,arg
    integer :: argc,i,reps,ios,exitcode,env_status
    logical :: reps_on_cli,output_on_cli
    character(32) :: env_reps
    character(4096) :: env_output

    argc=command_argument_count()
    if(argc<1) call usage()
    call get_command_argument(1,command)
    select case(trim(command))
    case('run')
        fo=''; output='/dev/stdout'; workloads='workloads'; reps=7
        reps_on_cli=.false.; output_on_cli=.false.
        i=2
        do while(i<=argc)
            call get_command_argument(i,arg)
            select case(trim(arg))
            case('--fo')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,fo)
            case('--reps')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,reps_text)
                read(reps_text,*,iostat=ios) reps
                if(ios/=0) call usage()
                if(reps<1 .or. reps>1000) call usage()
                reps_on_cli=.true.
            case('--output')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,output)
                if(len_trim(output)==0) call usage()
                output_on_cli=.true.
            case('--workloads')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,workloads)
            case default
                call usage()
            end select
            i=i+1
        end do
        if(.not.reps_on_cli) then
            call get_environment_variable('BENCH_REPS',env_reps,status=env_status)
            if(env_status==0 .and. len_trim(env_reps)>0) then
                read(env_reps,*,iostat=ios) reps
                if(ios/=0) call usage()
                if(reps<1 .or. reps>1000) call usage()
            end if
        end if
        if(.not.output_on_cli) then
            call get_environment_variable('BENCH_OUTPUT',env_output,status=env_status)
            if(env_status==0 .and. len_trim(env_output)>0) output=env_output
        end if
        if(len_trim(fo)==0) call usage()
        call run_benchmarks(trim(fo),reps,trim(output),trim(workloads),exitcode)
    case('report')
        if(argc==2) then
            call get_command_argument(2,arg)
            call report_jsonl(trim(arg),exitcode)
        else if(argc==3) then
            call get_command_argument(2,arg)
            if(trim(arg)/='--complete') call usage()
            call get_command_argument(3,arg)
            call report_jsonl(trim(arg),exitcode,.true.)
        else
            call usage()
        end if
    case default
        call usage()
    end select
    if(exitcode/=0) stop 1
contains
    subroutine usage()
        write(*,'(a)') 'usage: fo_bench run --fo PATH [--reps N] [--output FILE] [--workloads DIR]'
        write(*,'(a)') '       fo_bench report [--complete] RESULTS.jsonl'
        stop 2
    end subroutine usage
end program fo_bench
