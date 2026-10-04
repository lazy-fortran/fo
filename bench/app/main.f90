program fo_bench
    use bench_engine, only: run_benchmarks, report_jsonl
    implicit none
    character(32) :: command,reps_text
    character(4096) :: fo,output,workloads,arg
    integer :: argc,i,reps,ios,exitcode

    argc=command_argument_count()
    if(argc<1) call usage()
    call get_command_argument(1,command)
    select case(trim(command))
    case('run')
        fo=''; output='/dev/stdout'; workloads='workloads'; reps=7
        call get_environment_variable('BENCH_REPS',reps_text,status=ios)
        if(ios==0) then
          if(len_trim(reps_text)>0) then
            read(reps_text,*,iostat=ios) reps
            if(ios/=0) call usage()
            if(reps<1 .or. reps>1000) call usage()
          end if
        end if
        call get_environment_variable('BENCH_OUTPUT',output,status=ios)
        if(ios/=0) output='/dev/stdout'
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
            case('--output')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,output)
            case('--workloads')
                i=i+1; if(i>argc) call usage()
                call get_command_argument(i,workloads)
            case default
                call usage()
            end select
            i=i+1
        end do
        if(len_trim(fo)==0) call usage()
        call run_benchmarks(trim(fo),reps,trim(output),trim(workloads),exitcode)
    case('report')
        if(argc/=2) call usage()
        call get_command_argument(2,arg)
        call report_jsonl(trim(arg),exitcode)
    case default
        call usage()
    end select
    if(exitcode/=0) stop 1
contains
    subroutine usage()
        write(*,'(a)') 'usage: fo_bench run --fo PATH [--reps N] [--output FILE] [--workloads DIR]'
        write(*,'(a)') '       fo_bench report RESULTS.jsonl'
        stop 2
    end subroutine usage
end program fo_bench
