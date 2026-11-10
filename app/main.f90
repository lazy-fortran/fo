program fo_main
    use fo_cli_entry, only: cli_entry
    implicit none

    ! Self-builds and linked fixtures share the same command entry.
    call cli_entry()
end program fo_main
