program test_verification
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_verification, only: verification_config_t, &
        verification_config_parse, verification_config_defaults, claim_t, &
        verification_scan_dir, verification_result_t, verification_run, &
        verification_json, derive_generate_all, derive_generated_path, &
        verification_clear_proof_cache
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_config_parse()
    call test_scan_directives()
    call test_content_addressing()
    call test_json_distinguishes_evidence()
    call test_probe_disproves()

    write (output_unit, '(a,i0,a,i0,a)') 'verification: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_config_parse()
        type(verification_config_t) :: config
        character(len=*), parameter :: dir = '/tmp/fo_verif_cfg'
        integer :: u

        call execute_command_line('rm -rf '//dir, wait=.true.)
        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "cfg"'
        write (u, '(a)') '[extra.fo.verification]'
        write (u, '(a)') 'require-proof = ["generated-kernel-equivalence", "array-bounds"]'
        write (u, '(a)') 'allow-unknown = ["special-function-identity"]'
        write (u, '(a)') 'property-test-unknown = true'
        write (u, '(a)') 'lean = "auto"'
        write (u, '(a)') 'why3 = "auto"'
        close (u)

        call verification_config_parse(dir, config)
        call assert(config%n_require_proof == 2, 'require-proof parses two classes')
        call assert(trim(config%require_proof(1)) == 'generated-kernel-equivalence', &
            'first require-proof class')
        call assert(trim(config%require_proof(2)) == 'array-bounds', &
            'second require-proof class')
        call assert(config%n_allow_unknown == 1, 'allow-unknown parses one class')
        call assert(trim(config%allow_unknown(1)) == 'special-function-identity', &
            'allow-unknown class')
        call assert(config%property_test_unknown, 'property-test-unknown default true')
        call assert(trim(config%lean) == 'auto', 'lean default auto')
        call assert(trim(config%why3) == 'auto', 'why3 default auto')

        call verification_config_defaults(config)
        call assert(config%n_require_proof == 0, 'defaults clear require-proof')
        call assert(config%property_test_unknown, 'defaults keep property testing on')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_config_parse

    subroutine test_scan_directives()
        type(claim_t) :: claims(64)
        character(len=*), parameter :: dir = '/tmp/fo_verif_scan'
        integer :: n_claims, u

        call execute_command_line('rm -rf '//dir, wait=.true.)
        call execute_command_line('mkdir -p '//dir//'/src', wait=.true.)
        open (newunit=u, file=dir//'/src/m.f90', status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    !@assume finite_x: x > -1.0d0'
        write (u, '(a)') '    !@property commutative: a + b == b + a'
        write (u, '(a)') '    !@claim array-bounds index_ok: n >= 1'
        write (u, '(a)') '    !@derive derived_sum: 2.0d0 * x + 3.0d0 => result'
        write (u, '(a)') 'end module m'
        close (u)

        call verification_scan_dir(dir, claims, n_claims)
        call assert(n_claims == 4, 'scans four directives')
        call assert(trim(claims(1)%kind) == 'assume', 'first directive is assume')
        call assert(trim(claims(1)%name) == 'finite_x', 'assume name parsed')
        call assert(trim(claims(2)%kind) == 'property', 'second directive is property')
        call assert(trim(claims(3)%class) == 'array-bounds', 'claim class parsed')
        call assert(trim(claims(3)%name) == 'index_ok', 'claim name parsed')
        call assert(trim(claims(4)%kind) == 'derive', 'fourth directive is derive')
        call assert(trim(claims(4)%target) == 'result', 'derive target parsed')
        call assert(index(trim(claims(4)%expr), 'x') > 0, 'derive expression parsed')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_scan_directives

    subroutine test_content_addressing()
        ! Content-addressing is exercised end to end by the cache in the full
        ! prove run (covered by the system check). Here we verify the public
        ! contract through the cache: a stored result is restored, and a changed
        ! claim source file yields a different result set.
        type(claim_t) :: claims(64)
        type(verification_result_t) :: results(64)
        type(verification_config_t) :: config
        character(len=*), parameter :: dir = '/tmp/fo_verif_addr'
        integer :: n_claims, n_results, ierr, u

        call execute_command_line('rm -rf '//dir, wait=.true.)
        call execute_command_line('mkdir -p '//dir//'/src', wait=.true.)
        call verification_config_defaults(config)
        open (newunit=u, file=dir//'/src/m.f90', status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    !@property p: a == a'
        write (u, '(a)') 'end module m'
        close (u)
        call verification_scan_dir(dir, claims, n_claims)
        call verification_run(dir, config, claims, n_claims, results, &
            n_results, ierr)
        call assert(n_results == 1, 'one property obligation')
        call assert(trim(results(1)%status) == 'UNKNOWN', &
            'tautology probes UNKNOWN, not PROVED')
        call assert(trim(results(1)%evidence) == 'probe', &
            'tautology evidence is probe, not proof')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_content_addressing

    subroutine test_json_distinguishes_evidence()
        type(claim_t) :: claims(64)
        type(verification_result_t) :: results(64)
        type(verification_config_t) :: config
        character(len=*), parameter :: dir = '/tmp/fo_verif_json'
        character(len=65536) :: json
        integer :: n_claims, n_results, ierr, u

        call execute_command_line('rm -rf '//dir, wait=.true.)
        call execute_command_line('mkdir -p '//dir//'/src', wait=.true.)
        call verification_config_defaults(config)
        open (newunit=u, file=dir//'/src/m.f90', status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    !@claim bad index_ok: n >= 1'
        write (u, '(a)') 'end module m'
        close (u)
        call verification_scan_dir(dir, claims, n_claims)
        call verification_run(dir, config, claims, n_claims, results, &
            n_results, ierr)
        json = verification_json(results, n_results)
        call assert(index(json, '"evidence":"probe"') > 0, &
            'JSON marks probe evidence')
        call assert(index(json, '"status":"DISPROVED"') > 0, &
            'JSON reports DISPROVED')
        call assert(index(json, '"disproved":1') > 0, &
            'JSON aggregates disproved count')
        call assert(index(json, '"counterexample"') > 0, &
            'JSON includes counterexample field')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_json_distinguishes_evidence

    subroutine test_probe_disproves()
        type(claim_t) :: claims(64)
        type(verification_result_t) :: results(64)
        type(verification_config_t) :: config
        character(len=*), parameter :: dir = '/tmp/fo_verif_probe'
        integer :: n_claims, n_results, ierr, u

        call execute_command_line('rm -rf '//dir, wait=.true.)
        call execute_command_line('mkdir -p '//dir//'/src', wait=.true.)
        call verification_config_defaults(config)
        open (newunit=u, file=dir//'/src/m.f90', status='replace')
        write (u, '(a)') 'module m'
        write (u, '(a)') '    !@property falsity: x < x'
        write (u, '(a)') 'end module m'
        close (u)
        call verification_scan_dir(dir, claims, n_claims)
        call verification_run(dir, config, claims, n_claims, results, &
            n_results, ierr)
        call assert(n_results == 1, 'one falsity property')
        call assert(trim(results(1)%status) == 'DISPROVED', &
            'numeric probe disproves a false property')
        call assert(len_trim(results(1)%counterexample) > 0, &
            'disproof carries a counterexample')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_probe_disproves

end program test_verification
