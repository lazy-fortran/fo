program test_link_graph
    use fo_link_graph, only: link_graph_load, link_graph_record, link_graph_path
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_util, only: make_tmpfile
    implicit none

    character(len=1024) :: project
    character(len=512) :: outputs(1), inputs(2)
    character(len=512), allocatable :: recorded_outputs(:), recorded_inputs(:)
    integer :: unit, i, n

    call make_tmpfile('fo-link-graph', project)
    call fs_make_dir(trim(project)//'/build/fo')
    open (newunit=unit, file=link_graph_path(trim(project)), status='replace')
    do i = 1, 1000
        write (unit, '(a,i0,a)') 'old-output-', i, &
            achar(9)//repeat('x', 100)//'.a'
    end do
    close (unit)
    outputs(1) = 'new-output'
    inputs = [character(len=512) :: 'new-library.a', 'ignored-object.o']

    call link_graph_record(trim(project), outputs, 1, inputs, 2)
    call link_graph_load(trim(project), recorded_outputs, recorded_inputs, n)
    call require(n == 1001, 'a large prior graph survives recording a new output')
    do i = 1, 1000
        call require(trim(recorded_inputs(i)) == repeat('x', 100)//'.a', &
            'prior inputs survive unchanged')
    end do
    call require(trim(recorded_outputs(n)) == 'new-output', 'new output is appended')
    call require(trim(recorded_inputs(n)) == 'new-library.a', &
        'only library inputs are recorded')

    inputs(1) = 'replacement-library.so'
    call link_graph_record(trim(project), outputs, 1, inputs, 2)
    call link_graph_load(trim(project), recorded_outputs, recorded_inputs, n)
    call require(n == 1001, 'recording the output replaces its previous input')
    call require(trim(recorded_inputs(n)) == 'replacement-library.so', &
        'replacement input is recorded')
    call link_graph_record(trim(project), outputs, 1, inputs, 2)
    call link_graph_load(trim(project), recorded_outputs, recorded_inputs, n)
    call require(n == 1001, 'recording unchanged inputs is idempotent')
    call fs_remove_tree(trim(project))
    print '(a)', 'PASS: large link graph updates'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        print '(a)', 'FAIL: '//message
        error stop 1
    end subroutine require

end program test_link_graph
