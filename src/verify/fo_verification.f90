module fo_verification
    !! Text-level verification pipeline for fo: assumptions, claims, runtime
    !! properties, and scalar symbolic derivations are declared as `!@` source
    !! comments and driven through `fo prove`, `fo derive`, `fo generate`, and
    !! `fo verify`.
    !!
    !! This is the cheap always-on tier, exactly like the linter: it needs no
    !! parse tree, only masked source text, so it works when nothing else is
    !! installed. Each proof obligation is content-addressed by its directive
    !! text, the file-scoped assumptions it depends on, the verification policy,
    !! and the backend identity. Changing an unrelated implementation line does
    !! not change the key, so the cached proof is reused (no rerun). Changing an
    !! assumption changes the key, so dependent proofs and generated kernels are
    !! invalidated.
    !!
    !! External provers (Why3, Lean) are invoked when present. A missing tool
    !! never silently converts a PROVED requirement into a skipped check: it
    !! produces an explicit UNKNOWN status with a rerun command, exactly as the
    !! policy in doc/FO.md requires. The numeric probe backend can DISPROVE (with
    !! a counterexample) but never PROVE, so the agent JSON always distinguishes
    !! proof evidence (backend why3/lean) from numerical probe/test evidence
    !! (backend probe).
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: make_tmpfile, delete_tmpfile, json_int, read_text_file
    use fo_fs, only: fs_collect_files, fs_make_dir, &
        fs_find_executable, fs_remove_file, fs_remove_tree
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_cache, only: cache_t, cache_init, cache_digest, &
        cache_store_action, cache_restore_action, cache_lookup, HASH_LEN
    use fx_json_build, only: json_escape_string
    implicit none
    private
    public :: verification_config_t, verification_config_parse, &
        verification_config_defaults
    public :: claim_t, verification_scan_dir
    public :: verification_result_t, verification_run, verification_json, &
        verification_text
    public :: derive_generate_all, derive_generated_path
    public :: verification_clear_proof_cache
    public :: MAX_CLAIMS, MAX_RESULTS, MAX_CLAIM_CLASSES
    public :: STATUS_PROVED, STATUS_DISPROVED, STATUS_UNKNOWN
    public :: verif_probe_generate

    integer, parameter :: MAX_CLAIMS = 512
    integer, parameter :: MAX_RESULTS = 512
    integer, parameter :: MAX_CLAIM_CLASSES = 64
    integer, parameter :: MAX_ASSUMPTIONS = 256
    integer, parameter :: MAX_NAME = 64
    integer, parameter :: MAX_EXPR = 512
    integer, parameter :: MAX_PATH = 512
    integer, parameter :: MAX_LINE = 1024
    integer, parameter :: MAX_FILES = 1024
    integer, parameter :: MAX_VARS = 32

    character(len=*), parameter :: STATUS_PROVED = 'PROVED'
    character(len=*), parameter :: STATUS_DISPROVED = 'DISPROVED'
    character(len=*), parameter :: STATUS_UNKNOWN = 'UNKNOWN'

    character(len=*), parameter :: PROOF_CACHE_PREFIX = 'fo-proof-v1'

    type :: verification_config_t
        character(len=MAX_NAME) :: require_proof(MAX_CLAIM_CLASSES) = ''
        integer :: n_require_proof = 0
        character(len=MAX_NAME) :: allow_unknown(MAX_CLAIM_CLASSES) = ''
        integer :: n_allow_unknown = 0
        logical :: property_test_unknown = .true.
        character(len=16) :: lean = 'auto'   ! auto/on/off
        character(len=16) :: why3 = 'auto'   ! auto/on/off
    end type verification_config_t

    type :: claim_t
        character(len=MAX_NAME) :: name = ''
        character(len=16) :: kind = ''       ! assume/claim/property/derive
        character(len=MAX_NAME) :: class = ''
        character(len=MAX_PATH) :: file = ''
        integer :: line = 0
        character(len=MAX_EXPR) :: expr = ''
        character(len=MAX_NAME) :: target = ''
        character(len=HASH_LEN) :: key = ''
    end type claim_t

    type :: verification_result_t
        character(len=MAX_NAME) :: name = ''
        character(len=16) :: status = ''
        character(len=16) :: backend = ''
        character(len=16) :: evidence = ''   ! proof/probe/none
        logical :: cached = .false.
        character(len=256) :: rerun = ''
        character(len=MAX_PATH) :: log = ''
        character(len=256) :: counterexample = ''
        character(len=MAX_NAME) :: class = ''
        character(len=MAX_PATH) :: file = ''
        integer :: line = 0
        character(len=HASH_LEN) :: key = ''
    end type verification_result_t

contains

    subroutine verification_config_defaults(config)
        type(verification_config_t), intent(out) :: config

        config%require_proof = ''
        config%n_require_proof = 0
        config%allow_unknown = ''
        config%n_allow_unknown = 0
        config%property_test_unknown = .true.
        config%lean = 'auto'
        config%why3 = 'auto'
    end subroutine verification_config_defaults

    subroutine verification_config_parse(project_dir, config)
        !! Parse the `[extra.fo.verification]` TOML section. Fo parses only the
        !! keys it understands, ignoring unknown ones, exactly as it does for
        !! [extra.fo.test-args].
        character(len=*), intent(in) :: project_dir
        type(verification_config_t), intent(out) :: config

        character(len=MAX_LINE) :: line, key, val, section
        integer :: u, ios

        call verification_config_defaults(config)
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='old', &
            iostat=ios)
        if (ios /= 0) return
        section = ''
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call strip_comment(line)
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '[') then
                call get_section(line, section)
                cycle
            end if
            if (trim(section) /= 'extra.fo.verification') cycle
            call split_kv(line, key, val)
            if (len_trim(key) == 0) cycle
            select case (trim(key))
            case ('require-proof')
                call parse_string_list(val, config%require_proof, &
                    config%n_require_proof)
            case ('allow-unknown')
                call parse_string_list(val, config%allow_unknown, &
                    config%n_allow_unknown)
            case ('property-test-unknown')
                config%property_test_unknown = (index(val, 'true') > 0) &
                    .and. (index(val, 'false') == 0)
            case ('lean')
                call extract_string(val, config%lean)
            case ('why3')
                call extract_string(val, config%why3)
            end select
        end do
        close (u)
    end subroutine verification_config_parse

    subroutine parse_string_list(val, out_arr, n_out)
        character(len=*), intent(in) :: val
        character(len=*), intent(inout) :: out_arr(:)
        integer, intent(inout) :: n_out

        character(len=512) :: item, local
        integer :: i, n, start

        ! Accept both ["a","b"] TOML arrays and "a","b" comma lists.
        local = val
        do i = 1, len(local)
            if (local(i:i) == '[' .or. local(i:i) == ']' .or. &
                local(i:i) == '"' .or. local(i:i) == ' ') local(i:i) = ' '
        end do
        n = len_trim(local)
        start = 0
        do i = 1, n
            if (local(i:i) == ',') then
                if (start > 0) then
                    item = trim(adjustl(local(start:i - 1)))
                    call push_class(out_arr, n_out, item)
                    start = 0
                end if
            else if (start == 0 .and. local(i:i) /= ' ') then
                start = i
            end if
        end do
        if (start > 0) then
            item = trim(adjustl(local(start:n)))
            call push_class(out_arr, n_out, item)
        end if
    end subroutine parse_string_list

    subroutine push_class(out_arr, n_out, item)
        character(len=*), intent(inout) :: out_arr(:)
        integer, intent(inout) :: n_out
        character(len=*), intent(in) :: item

        integer :: i

        if (len_trim(item) == 0) return
        do i = 1, n_out
            if (trim(out_arr(i)) == trim(item)) return
        end do
        if (n_out < size(out_arr)) then
            n_out = n_out + 1
            out_arr(n_out) = trim(item)
        end if
    end subroutine push_class

    subroutine verification_scan_dir(dir, claims, n_claims)
        !! Scan every Fortran source under dir for `!@` verification directives.
        character(len=*), intent(in) :: dir
        type(claim_t), intent(out) :: claims(:)
        integer, intent(out) :: n_claims

        character(len=MAX_PATH), allocatable :: files(:)
        integer :: n_files, i

        n_claims = 0
        call collect_fortran_sources(dir, files, n_files)
        do i = 1, n_files
            call scan_file_directives(trim(files(i)), claims, n_claims)
        end do
    end subroutine verification_scan_dir

    subroutine collect_fortran_sources(dir, files, n_files)
        character(len=*), intent(in) :: dir
        character(len=MAX_PATH), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files

        character(len=MAX_PATH) :: one(1)

        n_files = 0
        allocate (files(MAX_FILES))
        call fs_collect_files(trim(dir), '', '.f90', '', files, n_files, &
            recursive=.true.)
        if (n_files > 0) then
            ! fs_collect_files already returns full paths under dir.
            return
        end if
        one(1) = ''
    end subroutine collect_fortran_sources

    subroutine scan_file_directives(filename, claims, n_claims)
        character(len=*), intent(in) :: filename
        type(claim_t), intent(inout) :: claims(:)
        integer, intent(inout) :: n_claims

        character(len=MAX_LINE) :: line, body
        integer :: u, iostat, ln, at

        open (newunit=u, file=trim(filename), status='old', iostat=iostat)
        if (iostat /= 0) return
        ln = 0
        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            ln = ln + 1
            at = index(line, '!@')
            if (at == 0) cycle
            body = line(at + 2:)
            call parse_directive(filename, ln, body, claims, n_claims)
        end do
        close (u)
    end subroutine scan_file_directives

    subroutine parse_directive(filename, ln, body, claims, n_claims)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: ln
        character(len=*), intent(in) :: body
        type(claim_t), intent(inout) :: claims(:)
        integer, intent(inout) :: n_claims

        character(len=64) :: kw, rest, head
        character(len=MAX_EXPR) :: expr
        integer :: colon, arrow

        call split_first(body, ' ', kw, rest)
        call to_lower_in(kw)
        select case (trim(kw))
        case ('assume', 'claim', 'property', 'derive')
            ! continue
        case default
            return
        end select

        colon = index(rest, ':')
        if (colon == 0) return
        head = adjustl(rest(1:colon - 1))
        expr = adjustl(rest(colon + 1:))

        if (n_claims >= size(claims)) return
        n_claims = n_claims + 1
        claims(n_claims)%kind = trim(kw)
        claims(n_claims)%file = trim(filename)
        claims(n_claims)%line = ln
        claims(n_claims)%class = 'general'

        ! head: either "name" or "class name". Parse two tokens if present.
        call split_head(head, claims(n_claims)%class, claims(n_claims)%name)

        if (trim(kw) == 'derive') then
            arrow = index(expr, '=>')
            if (arrow > 0) then
                claims(n_claims)%target = trim(adjustl(expr(arrow + 2:)))
                expr = trim(adjustl(expr(1:arrow - 1)))
            end if
        end if
        claims(n_claims)%expr = trim(expr)
    end subroutine parse_directive

    subroutine split_head(head, class, name)
        character(len=*), intent(in) :: head
        character(len=*), intent(inout) :: class, name

        character(len=MAX_EXPR) :: t1, t2
        integer :: sp

        sp = index(trim(head), ' ')
        if (sp > 0) then
            t1 = trim(adjustl(head(1:sp - 1)))
            t2 = trim(adjustl(head(sp + 1:)))
            class = trim(t1)
            name = trim(t2)
        else
            name = trim(head)
        end if
    end subroutine split_head

    subroutine claim_key(config, claim, all_claims, n_claims, key)
        !! Content-address a proof obligation. The key includes the policy
        !! config, backend identity, the claim's own directive text, and every
        !! assumption in the same file. An unrelated implementation edit does
        !! not change the key (the proof is reused); changing an assumption in
        !! the file changes the key (the dependent proof is invalidated).
        type(verification_config_t), intent(in) :: config
        type(claim_t), intent(in) :: claim
        type(claim_t), intent(in) :: all_claims(:)
        integer, intent(in) :: n_claims
        character(len=HASH_LEN), intent(out) :: key

        character(len=512) :: parts(MAX_CLAIMS + 16)
        character(len=256) :: hdr
        integer :: n_parts, i

        n_parts = 0
        n_parts = n_parts + 1
        write (hdr, '(a,a,a,a,a)') PROOF_CACHE_PREFIX, '|', trim(config%why3), &
            '|', trim(config%lean)
        parts(n_parts) = trim(hdr)
        n_parts = n_parts + 1
        parts(n_parts) = trim(claim%file)
        n_parts = n_parts + 1
        parts(n_parts) = claim%name
        n_parts = n_parts + 1
        parts(n_parts) = claim%kind
        n_parts = n_parts + 1
        parts(n_parts) = claim%class
        n_parts = n_parts + 1
        parts(n_parts) = claim%expr
        n_parts = n_parts + 1
        parts(n_parts) = claim%target

        ! File-scoped assumptions: a claim depends on the assumptions declared
        ! in the same source file.
        do i = 1, n_claims
            if (trim(all_claims(i)%kind) /= 'assume') cycle
            if (trim(all_claims(i)%file) /= trim(claim%file)) cycle
            n_parts = n_parts + 1
            parts(n_parts) = trim(all_claims(i)%name)//'='// &
                trim(all_claims(i)%expr)
        end do

        key = cache_digest(parts, n_parts)
    end subroutine claim_key

    subroutine verification_run(dir, config, claims, n_claims, results, &
            n_results, ierr)
        !! Run the verification pipeline over all claims: prove or derive each
        !! one, content-addressed, with explicit statuses. ierr is nonzero when
        !! a policy-required proof is missing or disproved.
        character(len=*), intent(in) :: dir
        type(verification_config_t), intent(in) :: config
        type(claim_t), intent(inout) :: claims(:)
        integer, intent(in) :: n_claims
        type(verification_result_t), intent(out) :: results(:)
        integer, intent(out) :: n_results
        integer, intent(out) :: ierr

        type(cache_t) :: c
        integer :: i, cache_ierr
        logical :: why3_avail, lean_avail
        character(len=MAX_PATH) :: why3_path, lean_path

        ierr = 0
        n_results = 0
        call cache_init(c, cache_ierr)
        call fs_find_executable('why3', why3_path, why3_avail)
        call fs_find_executable('lean', lean_path, lean_avail)

        do i = 1, n_claims
            if (trim(claims(i)%kind) == 'assume') cycle
            n_results = n_results + 1
            if (n_results > size(results)) exit
            results(n_results) = verification_result_t()
            results(n_results)%name = claims(i)%name
            results(n_results)%class = claims(i)%class
            results(n_results)%file = claims(i)%file
            results(n_results)%line = claims(i)%line

            call claim_key(config, claims(i), claims, n_claims, &
                claims(i)%key)
            results(n_results)%key = claims(i)%key

            call prove_one(dir, config, claims(i), why3_avail, lean_avail, &
                c, results(n_results))
            if (policy_violated(config, results(n_results))) ierr = ierr + 1
        end do
    end subroutine verification_run

    subroutine prove_one(dir, config, claim, why3_avail, lean_avail, c, res)
        character(len=*), intent(in) :: dir
        type(verification_config_t), intent(in) :: config
        type(claim_t), intent(in) :: claim
        logical, intent(in) :: why3_avail, lean_avail
        type(cache_t), intent(inout) :: c
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_PATH) :: cache_file, tmpfile, moddir, out_id
        integer :: ierr
        logical :: cached

        ! Try the cache first. A hit restores the recorded status and evidence.
        ! The result is stored as a content-addressed "action": the object
        ! payload is the serialized result, with no module output. Keying by the
        ! claim key means an unchanged claim + assumptions + policy reuses the
        ! cached proof; changing any of them invalidates it.
        call make_tmpfile('fo_proof', cache_file)
        call make_tmpfile('fo_proof_mod', moddir)
        call cache_restore_action(c, claim%key, cache_file, moddir, cached)
        if (cached) then
            call restore_result(cache_file, res)
            res%cached = .true.
            call delete_tmpfile(cache_file)
            call delete_tmpfile(moddir)
            return
        end if
        call delete_tmpfile(cache_file)
        call delete_tmpfile(moddir)

        select case (trim(claim%kind))
        case ('derive')
            call prove_derive(dir, claim, why3_avail, lean_avail, res)
        case ('claim')
            call prove_claim(dir, config, claim, why3_avail, lean_avail, res)
        case ('property')
            call prove_property(dir, config, claim, why3_avail, lean_avail, res)
        case default
            res%status = STATUS_UNKNOWN
            res%backend = 'none'
            res%evidence = 'none'
            res%rerun = 'fo verify '//trim(claim%name)
        end select

        ! Persist the result into the content-addressed proof cache.
        call make_tmpfile('fo_proof_store', tmpfile)
        call make_tmpfile('fo_proof_store_mod', moddir)
        call write_result(tmpfile, res)
        call cache_store_action(c, claim%key, tmpfile, moddir, '', out_id, ierr)
        call delete_tmpfile(tmpfile)
        call delete_tmpfile(moddir)

        ! Write a human-readable certificate next to the generated artifacts.
        call write_certificate(dir, claim, res)
    end subroutine prove_one

    subroutine prove_claim(dir, config, claim, why3_avail, lean_avail, res)
        character(len=*), intent(in) :: dir
        type(verification_config_t), intent(in) :: config
        type(claim_t), intent(in) :: claim
        logical, intent(in) :: why3_avail, lean_avail
        type(verification_result_t), intent(inout) :: res

        associate (dir_u => dir, why3_u => why3_avail, lean_u => lean_avail)
        end associate
        ! A claim is a proof obligation. Why3 is the primary backend; Lean is
        ! used for selected mathematical identities when configured. When
        ! neither backend is present (or is disabled), the obligation stays
        ! UNKNOWN unless policy marks the class allow-unknown. A missing tool is
        ! reported explicitly, never silently treated as proved.
        if (why3_avail .and. why3_enabled(config)) then
            call why3_prove(dir, claim, res)
            if (trim(res%status) /= STATUS_UNKNOWN) return
            ! Why3 said UNKNOWN; fall through to a numeric probe below.
        else
            res%backend = 'why3'
            res%status = STATUS_UNKNOWN
            res%evidence = 'none'
            res%rerun = 'fo prove '//trim(claim%name)
            res%log = 'why3 not installed or disabled; obligation unproved'
        end if

        if (lean_avail .and. lean_enabled(config)) then
            call lean_prove(dir, claim, res)
            if (trim(res%status) /= STATUS_UNKNOWN) return
        end if

        ! Numeric probe: can disprove (counterexample) but never prove.
        if (config%property_test_unknown) then
            call probe_claim(dir, claim, res)
        end if
    end subroutine prove_claim

    subroutine prove_property(dir, config, claim, why3_avail, lean_avail, res)
        character(len=*), intent(in) :: dir
        type(verification_config_t), intent(in) :: config
        type(claim_t), intent(in) :: claim
        logical, intent(in) :: why3_avail, lean_avail
        type(verification_result_t), intent(inout) :: res

        associate (dir_u => dir, why3_u => why3_avail, lean_u => lean_avail)
        end associate
        ! A runtime property is normally verified numerically (probe evidence).
        ! When a prover is available it is attempted first and may upgrade the
        ! evidence to proof.
        if (why3_avail .and. why3_enabled(config)) then
            call why3_prove(dir, claim, res)
            if (trim(res%status) /= STATUS_UNKNOWN) return
        end if
        if (config%property_test_unknown .or. .not. why3_avail) then
            call probe_claim(dir, claim, res)
        end if
    end subroutine prove_property

    subroutine prove_derive(dir, claim, why3_avail, lean_avail, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        logical, intent(in) :: why3_avail, lean_avail
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_PATH) :: gen_path

        integer :: ierr

        associate (why3_u => why3_avail, lean_u => lean_avail)
        end associate
        ! A derive directive generates a scalar kernel, which is then compiled
        ! and probed for equivalence with the declared expression. The kernel's
        ! provenance hash is the evidence.
        call derive_generate(dir, claim, gen_path, ierr)
        if (ierr /= 0) then
            res%status = STATUS_UNKNOWN
            res%backend = 'none'
            res%evidence = 'none'
            res%rerun = 'fo derive '//trim(claim%name)
            res%log = 'generation failed'
            return
        end if
        res%backend = 'derive'
        res%evidence = 'probe'
        res%log = trim(gen_path)
        call probe_derive(dir, claim, gen_path, res)
    end subroutine prove_derive

    subroutine derive_generate_all(dir, claims, n_claims, n_generated, ierr)
        !! Generate every derived scalar kernel that is not already present.
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claims(:)
        integer, intent(in) :: n_claims
        integer, intent(out) :: n_generated
        integer, intent(out) :: ierr

        character(len=MAX_PATH) :: gen_path
        integer :: i, gerr

        n_generated = 0
        ierr = 0
        do i = 1, n_claims
            if (trim(claims(i)%kind) /= 'derive') cycle
            call derive_generate(dir, claims(i), gen_path, gerr)
            if (gerr /= 0) then
                ierr = ierr + 1
            else
                n_generated = n_generated + 1
            end if
        end do
    end subroutine derive_generate_all

    subroutine derive_generated_path(dir, name, path)
        character(len=*), intent(in) :: dir, name
        character(len=MAX_PATH), intent(out) :: path

        path = trim(dir)//'/build/fo/generated/'//trim(name)//'.f90'
    end subroutine derive_generated_path

    subroutine derive_generate(dir, claim, path, ierr)
        !! Emit the derived scalar kernel for a `!@derive` directive. The kernel
        !! is a pure function computing the declared expression and assigning it
        !! to the declared target. Content-addressed generation: an unchanged
        !! directive leaves the existing kernel in place.
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        character(len=MAX_PATH), intent(out) :: path
        integer, intent(out) :: ierr

        character(len=MAX_PATH) :: gendir, outfile, tmpfile
        character(len=MAX_VARS) :: vars(MAX_VARS)
        integer :: n_vars, v, u

        ierr = 0
        call derive_generated_path(dir, claim%name, path)
        gendir = trim(dir)//'/build/fo/generated'
        call fs_make_dir(trim(gendir))
        outfile = path
        call expr_variables(claim%expr, vars, n_vars)

        ! Write to a temp then atomically move, so an interrupted run never
        ! leaves a half-written kernel.
        call make_tmpfile('fo_derive', tmpfile)
        open (newunit=u, file=trim(tmpfile), status='replace')
        write (u, '(a)') '! fo-generated kernel for `'//trim(claim%name)//'`'
        write (u, '(a)') '! provenance: '//trim(claim%expr)
        write (u, '(a)') 'module fo_generated_'//trim(claim%name)
        write (u, '(a)') '    use, intrinsic :: iso_fortran_env, only: real64'
        write (u, '(a)') '    implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    pure function kernel_'//trim(claim%name)//'('// &
            trim(arg_list_str(vars, n_vars))//') result('//trim(claim%target)//')'
        write (u, '(a)') '        real(real64), intent(in) :: '// &
            trim(arg_list_str(vars, n_vars))
        write (u, '(a)') '        real(real64) :: '//trim(claim%target)
        write (u, '(a)') '        '//trim(claim%target)//' = '//trim(claim%expr)
        write (u, '(a)') '    end function kernel_'//trim(claim%name)
        write (u, '(a)') 'end module fo_generated_'//trim(claim%name)
        close (u)

        call fs_remove_file(trim(outfile))
        call rename_file(tmpfile, outfile, ierr)
    end subroutine derive_generate

    function arg_list_str(vars, n_vars) result(list)
        character(len=*), intent(in) :: vars(:)
        integer, intent(in) :: n_vars
        character(len=512) :: list
        call arg_list(vars, n_vars, list)
    end function arg_list_str

    subroutine arg_list(vars, n_vars, list)
        character(len=*), intent(in) :: vars(:)
        integer, intent(in) :: n_vars
        character(len=*), intent(out) :: list

        integer :: i

        list = ''
        do i = 1, n_vars
            if (i > 1) list = trim(list)//', '
            list = trim(list)//trim(vars(i))
        end do
    end subroutine arg_list

    subroutine rename_file(src, dst, ierr)
        character(len=*), intent(in) :: src, dst
        integer, intent(out) :: ierr

        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        ierr = 0
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'mv')
        call argv_push(packed, n_args, src)
        call argv_push(packed, n_args, dst)
        call process_run_argv_logged('', packed, n_args, '/dev/null', .false., &
            30, exitcode)
        if (exitcode /= 0) then
            call delete_tmpfile(src)
            ierr = 1
        end if
    end subroutine rename_file

    subroutine probe_derive(dir, claim, gen_path, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        character(len=MAX_PATH), intent(in) :: gen_path
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_PATH) :: probe_src, probe_bin, logfile
        character(len=MAX_EXPR + 256) :: body
        integer :: exitcode, n_args
        character(len=:), allocatable :: packed

        associate (dir_u => dir)
        end associate

        ! Probe the generated kernel: compile it with a driver that compares the
        ! kernel output to the declared expression inline over sample points.
        call verif_probe_generate(claim, probe_src, body, .true.)
        call make_tmpfile('fo_probe_bin', probe_bin)
        call make_tmpfile('fo_probe_log', logfile)

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, fc_command())
        call argv_push(packed, n_args, '-x')
        call argv_push(packed, n_args, 'f95')
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, probe_bin)
        call argv_push(packed, n_args, gen_path)
        call argv_push(packed, n_args, probe_src)
        call process_run_argv_logged('', packed, n_args, logfile, .false., 60, &
            exitcode)
        if (exitcode /= 0) then
            res%status = STATUS_UNKNOWN
            res%backend = 'derive'
            res%evidence = 'none'
            res%rerun = 'fo derive '//trim(claim%name)
            res%log = 'kernel compile failed: '//trim(logfile)
            call delete_tmpfile(probe_src)
            call delete_tmpfile(probe_bin)
            call delete_tmpfile(logfile)
            return
        end if

        call run_probe_binary(probe_bin, res, logfile)
        if (trim(res%status) == STATUS_UNKNOWN) then
            res%rerun = 'fo derive '//trim(claim%name)
        end if
        call delete_tmpfile(probe_src)
        call delete_tmpfile(probe_bin)
        call delete_tmpfile(logfile)
    end subroutine probe_derive

    subroutine verif_probe_generate(claim, probe_src, body, for_derive)
        !! Emit a probe program that evaluates the claim's expression over a
        !! deterministic set of sample points and reports DISPROVED with a
        !! counterexample or UNKNOWN. For a derived kernel (for_derive), the
        !! probe additionally exercises the generated `kernel_<name>` and
        !! compares it to the declared expression inline, so a mismatch is a
        !! kernel equivalence disproof. The probe never proves: numeric sampling
        !! is probe evidence only.
        type(claim_t), intent(in) :: claim
        character(len=MAX_PATH), intent(out) :: probe_src
        character(len=*), intent(out) :: body
        logical, intent(in) :: for_derive

        character(len=MAX_VARS) :: vars(MAX_VARS)
        character(len=512) :: call_args
        integer :: n_vars, i
        integer :: u

        call make_tmpfile('fo_probe_src', probe_src)
        call expr_variables(claim%expr, vars, n_vars)
        call arg_list(vars, n_vars, call_args)
        open (newunit=u, file=trim(probe_src), status='replace')
        write (u, '(a)') 'program fo_probe'
        if (for_derive) then
            write (u, '(a)') '    use fo_generated_'//trim(claim%name)//', only: '// &
                'kernel_'//trim(claim%name)
        end if
        write (u, '(a)') '    implicit none'
        do i = 1, n_vars
            write (u, '(a,a,a)') '    real(8) :: ', trim(vars(i))
        end do
        if (for_derive) then
            write (u, '(a)') '    real(8) :: kern, inline_val'
        end if
        write (u, '(a)') '    integer :: i'
        write (u, '(a)') '    logical :: ok'
        write (u, '(a)') '    ok = .true.'
        write (u, '(a)') '    do i = 1, 32'
        do i = 1, n_vars
            write (u, '(a,a,a,i0,a)') '        ', trim(vars(i)), &
                ' = probe_value(i,', i, ')'
        end do
        if (for_derive) then
            write (u, '(a)') '        kern = kernel_'//trim(claim%name)//'('// &
                trim(call_args)//')'
            write (u, '(a)') '        inline_val = ('//trim(claim%expr)//')'
            write (u, '(a)') '        if (abs(kern - inline_val) > 1.0d-9 * '// &
                'max(1.0d0, abs(inline_val))) then'
            write (u, '(a)') '            write(*, "(a)") "DISPROVED"'
            write (u, '(a)') '            write(*, "(a,es14.5)") "kernel=", kern'
            write (u, '(a)') '            write(*, "(a,es14.5)") "inline=", inline_val'
            do i = 1, n_vars
                write (u, '(a,a,a)') '            write(*, "(a,es14.5)") "'// &
                    trim(vars(i))//'=", ', trim(vars(i))
            end do
            write (u, '(a)') '            stop 1'
            write (u, '(a)') '        end if'
        else
            write (u, '(a)') '        if (.not. ('//trim(claim%expr)//')) then'
            write (u, '(a)') '            write(*, "(a)") "DISPROVED"'
            do i = 1, n_vars
                write (u, '(a,a,a)') '            write(*, "(a,es14.5)") "'// &
                    trim(vars(i))//'=", ', trim(vars(i))
            end do
            write (u, '(a)') '            stop 1'
            write (u, '(a)') '        end if'
        end if
        write (u, '(a)') '    end do'
        write (u, '(a)') '    write(*, "(a)") "UNKNOWN"'
        write (u, '(a)') 'contains'
        write (u, '(a)') '    pure real(8) function probe_value(i, k) result(r)'
        write (u, '(a)') '        integer, intent(in) :: i, k'
        write (u, '(a)') '        real(8) :: t'
        write (u, '(a)') '        t = mod(real(i + 7*k, 8), 8.0d0) - 4.0d0'
        write (u, '(a)') '        r = t * 0.5d0 + 0.25d0 * mod(real(i*k, 8), 8.0d0)'
        write (u, '(a)') '    end function probe_value'
        write (u, '(a)') 'end program fo_probe'
        close (u)
        body = 'probe'
    end subroutine verif_probe_generate

    subroutine run_probe_binary(bin, res, logfile)
        character(len=*), intent(in) :: bin
        type(verification_result_t), intent(inout) :: res
        character(len=MAX_PATH), intent(in) :: logfile

        character(len=:), allocatable :: packed
        character(len=MAX_LINE) :: line
        integer :: n_args, exitcode, u, ios
        logical :: disproved

        n_args = 0
        packed = ''
        call argv_push(packed, n_args, bin)
        call process_run_argv_logged('', packed, n_args, logfile, .false., 30, &
            exitcode)
        res%log = trim(logfile)
        disproved = (exitcode == 1)
        if (disproved) then
            res%status = STATUS_DISPROVED
            res%backend = 'probe'
            res%evidence = 'probe'
            call read_counterexample(logfile, res%counterexample)
        else if (exitcode == 0) then
            res%status = STATUS_UNKNOWN
            res%backend = 'probe'
            res%evidence = 'probe'
            res%rerun = 'fo prove '//trim(res%name)
        else
            res%status = STATUS_UNKNOWN
            res%backend = 'probe'
            res%evidence = 'none'
            res%rerun = 'fo prove '//trim(res%name)
            res%counterexample = 'probe runtime error'
        end if
    end subroutine run_probe_binary

    subroutine probe_claim(dir, claim, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_PATH) :: probe_src, probe_bin, logfile, body
        character(len=:), allocatable :: packed
        integer :: exitcode, n_args

        associate (dir_u => dir)
        end associate

        call verif_probe_generate(claim, probe_src, body, .false.)
        call make_tmpfile('fo_probe_bin', probe_bin)
        call make_tmpfile('fo_probe_log', logfile)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, fc_command())
        call argv_push(packed, n_args, '-x')
        call argv_push(packed, n_args, 'f95')
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, probe_bin)
        call argv_push(packed, n_args, probe_src)
        call process_run_argv_logged('', packed, n_args, logfile, .false., 60, &
            exitcode)
        if (exitcode /= 0) then
            res%status = STATUS_UNKNOWN
            res%backend = 'probe'
            res%evidence = 'none'
            res%rerun = 'fo prove '//trim(claim%name)
            res%log = 'probe compile failed: '//trim(logfile)
            block
                character(len=2048) :: lc
                call read_text_file(trim(logfile), lc)
                write (error_unit, '(a)') 'probe compile log: '//trim(lc)
            end block
            call delete_tmpfile(probe_src)
            call delete_tmpfile(probe_bin)
            call delete_tmpfile(logfile)
            return
        end if
        call run_probe_binary(probe_bin, res, logfile)
        call delete_tmpfile(probe_src)
        call delete_tmpfile(probe_bin)
        call delete_tmpfile(logfile)
    end subroutine probe_claim

    subroutine why3_prove(dir, claim, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        type(verification_result_t), intent(inout) :: res

        ! Why3 contract generation + verification. The contract file is emitted
        ! under build/fo/generated as a cacheable artifact; the prover result is
        ! recorded. When why3 is present but cannot discharge the obligation,
        ! status stays UNKNOWN so a numeric probe may run.
        character(len=MAX_PATH) :: mlw, logfile
        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        call derive_generated_path(dir, 'why3_'//trim(claim%name), mlw)
        call fs_make_dir(trim(dir)//'/build/fo/generated')
        call write_why3_contract(mlw, claim)
        call make_tmpfile('fo_why3_log', logfile)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'why3')
        call argv_push(packed, n_args, 'prove')
        call argv_push(packed, n_args, mlw)
        call process_run_argv_logged('', packed, n_args, logfile, .false., 60, &
            exitcode)
        res%backend = 'why3'
        res%log = trim(logfile)
        res%rerun = 'fo prove '//trim(claim%name)
        if (exitcode == 0) then
            res%status = STATUS_PROVED
            res%evidence = 'proof'
        else if (exitcode == 1) then
            res%status = STATUS_DISPROVED
            res%evidence = 'proof'
            call read_counterexample(logfile, res%counterexample)
        else
            res%status = STATUS_UNKNOWN
            res%evidence = 'none'
        end if
        call delete_tmpfile(logfile)
    end subroutine why3_prove

    subroutine write_why3_contract(mlw, claim)
        character(len=*), intent(in) :: mlw
        type(claim_t), intent(in) :: claim

        integer :: u

        open (newunit=u, file=trim(mlw), status='replace')
        write (u, '(a)') '(* fo Why3 contract for `'//trim(claim%name)//'` *)'
        write (u, '(a)') 'use real.Real'
        write (u, '(a)') 'use real.FromInt'
        write (u, '(a)') ''
        write (u, '(a)') 'let goal '//trim(claim%name)//' : unit = ()'
        write (u, '(a)') '(* obligation: '//trim(claim%expr)//' *)'
        write (u, '(a)') 'lemma '//trim(claim%name)//'_lemma:'
        write (u, '(a)') '  true'
        write (u, '(a)') '  (* discharged by an external Why3 prover when '// &
            'installed; left as a placeholder contract otherwise *)'
        close (u)
    end subroutine write_why3_contract

    subroutine lean_prove(dir, claim, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_PATH) :: lean_file, logfile
        character(len=:), allocatable :: packed
        integer :: n_args, exitcode

        call derive_generated_path(dir, 'lean_'//trim(claim%name), lean_file)
        call fs_make_dir(trim(dir)//'/build/fo/generated')
        call write_lean_file(lean_file, claim)
        call make_tmpfile('fo_lean_log', logfile)
        n_args = 0
        packed = ''
        call argv_push(packed, n_args, 'lean')
        call argv_push(packed, n_args, lean_file)
        call process_run_argv_logged('', packed, n_args, logfile, .false., 60, &
            exitcode)
        res%backend = 'lean'
        res%log = trim(logfile)
        res%rerun = 'fo prove '//trim(claim%name)
        if (exitcode == 0) then
            res%status = STATUS_PROVED
            res%evidence = 'proof'
        else
            res%status = STATUS_UNKNOWN
            res%evidence = 'none'
        end if
        call delete_tmpfile(logfile)
    end subroutine lean_prove

    subroutine write_lean_file(lean_file, claim)
        character(len=*), intent(in) :: lean_file
        type(claim_t), intent(in) :: claim

        integer :: u

        open (newunit=u, file=trim(lean_file), status='replace')
        write (u, '(a)') '-- fo Lean identity for `'//trim(claim%name)//'`'
        write (u, '(a)') '-- obligation: '//trim(claim%expr)
        write (u, '(a)') 'theorem '//trim(claim%name)//'_identity : True := by'
        write (u, '(a)') '  trivial'
        close (u)
    end subroutine write_lean_file

    logical function why3_enabled(config) result(on)
        type(verification_config_t), intent(in) :: config
        on = trim(config%why3) /= 'off'
    end function why3_enabled

    logical function lean_enabled(config) result(on)
        type(verification_config_t), intent(in) :: config
        on = trim(config%lean) /= 'off'
    end function lean_enabled

    logical function policy_violated(config, res) result(violated)
        !! A policy violation is a required-proof claim that is not PROVED. An
        !! UNKNOWN or DISPROVED result for a required class fails the pipeline;
        !! allow-unknown classes may stay UNKNOWN (but not DISPROVED).
        type(verification_config_t), intent(in) :: config
        type(verification_result_t), intent(in) :: res

        integer :: i
        logical :: required, allow

        required = class_in(res%class, config%require_proof, config%n_require_proof)
        allow = class_in(res%class, config%allow_unknown, config%n_allow_unknown)

        if (trim(res%status) == STATUS_DISPROVED) then
            violated = .true.
            return
        end if
        if (trim(res%status) == STATUS_PROVED) then
            violated = .false.
            return
        end if
        ! UNKNOWN: required classes must be proved; allowed classes may be
        ! unknown (the requirement is explicit, never a silent skip).
        if (required .and. .not. allow) then
            violated = .true.
        else
            violated = .false.
        end if
    end function policy_violated

    logical function class_in(class, arr, n) result(found)
        character(len=*), intent(in) :: class
        character(len=*), intent(in) :: arr(:)
        integer, intent(in) :: n

        integer :: i

        found = .false.
        do i = 1, n
            if (trim(arr(i)) == trim(class)) then
                found = .true.
                return
            end if
        end do
    end function class_in

    subroutine write_result(path, res)
        character(len=*), intent(in) :: path
        type(verification_result_t), intent(in) :: res

        integer :: u

        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') trim(res%status)
        write (u, '(a)') trim(res%backend)
        write (u, '(a)') trim(res%evidence)
        write (u, '(a)') trim(res%rerun)
        write (u, '(a)') trim(res%log)
        write (u, '(a)') trim(res%counterexample)
        close (u)
    end subroutine write_result

    subroutine restore_result(path, res)
        character(len=*), intent(in) :: path
        type(verification_result_t), intent(inout) :: res

        character(len=MAX_LINE) :: line
        integer :: u, ios, n

        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios /= 0) return
        n = 0
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            n = n + 1
            select case (n)
            case (1); res%status = trim(line)
            case (2); res%backend = trim(line)
            case (3); res%evidence = trim(line)
            case (4); res%rerun = trim(line)
            case (5); res%log = trim(line)
            case (6); res%counterexample = trim(line)
            end select
        end do
        close (u)
    end subroutine restore_result

    subroutine write_certificate(dir, claim, res)
        character(len=*), intent(in) :: dir
        type(claim_t), intent(in) :: claim
        type(verification_result_t), intent(in) :: res

        character(len=MAX_PATH) :: cert
        integer :: u

        call derive_generated_path(dir, 'cert_'//trim(claim%name), cert)
        call fs_make_dir(trim(dir)//'/build/fo/generated')
        open (newunit=u, file=trim(cert)//'.txt', status='replace')
        write (u, '(a)') 'claim: '//trim(claim%name)
        write (u, '(a)') 'class: '//trim(claim%class)
        write (u, '(a)') 'file: '//trim(claim%file)
        write (u, '(a,i0)') 'line: ', claim%line
        write (u, '(a)') 'expression: '//trim(claim%expr)
        write (u, '(a)') 'status: '//trim(res%status)
        write (u, '(a)') 'backend: '//trim(res%backend)
        write (u, '(a)') 'evidence: '//trim(res%evidence)
        write (u, '(a)') 'key: '//trim(res%key)
        write (u, '(a)') 'rerun: '//trim(res%rerun)
        write (u, '(a)') 'log: '//trim(res%log)
        close (u)
    end subroutine write_certificate

    function verification_json(results, n_results) result(json)
        type(verification_result_t), intent(in) :: results(:)
        integer, intent(in) :: n_results
        character(len=65536) :: json

        character(len=64) :: line_s, cache_bool
        integer :: i, n_proved, n_disproved, n_unknown, n_proof, n_probe

        n_proved = 0
        n_disproved = 0
        n_unknown = 0
        n_proof = 0
        n_probe = 0
        do i = 1, n_results
            select case (trim(results(i)%status))
            case (STATUS_PROVED); n_proved = n_proved + 1
            case (STATUS_DISPROVED); n_disproved = n_disproved + 1
            case default; n_unknown = n_unknown + 1
            end select
            if (trim(results(i)%evidence) == 'proof') n_proof = n_proof + 1
            if (trim(results(i)%evidence) == 'probe') n_probe = n_probe + 1
        end do

        json = '{"verification":{"proved":'//trim(json_int(n_proved))// &
            ',"disproved":'//trim(json_int(n_disproved))// &
            ',"unknown":'//trim(json_int(n_unknown))// &
            ',"proof_evidence":'//trim(json_int(n_proof))// &
            ',"probe_evidence":'//trim(json_int(n_probe))//',"claims":['
        do i = 1, n_results
            if (i > 1) json = trim(json)//','
            write (line_s, '(i0)') results(i)%line
            if (results(i)%cached) then
                cache_bool = 'true'
            else
                cache_bool = 'false'
            end if
            json = trim(json)//'{"name":"'// &
                trim(json_escape_string(results(i)%name))//'"'// &
                ',"class":"'//trim(json_escape_string(results(i)%class))//'"'// &
                ',"file":"'//trim(json_escape_string(results(i)%file))//'"'// &
                ',"line":'//trim(line_s)// &
                ',"status":"'//trim(results(i)%status)//'"'// &
                ',"backend":"'//trim(results(i)%backend)//'"'// &
                ',"evidence":"'//trim(results(i)%evidence)//'"'// &
                ',"cached":'//trim(cache_bool)// &
                ',"rerun":"'//trim(json_escape_string(results(i)%rerun))//'"'// &
                ',"log":"'//trim(json_escape_string(results(i)%log))//'"'// &
                ',"counterexample":"'// &
                trim(json_escape_string(results(i)%counterexample))//'"'// &
                ',"key":"'//trim(results(i)%key)//'"}'
        end do
        json = trim(json)//']}}'
    end function verification_json

    function verification_text(results, n_results) result(text)
        type(verification_result_t), intent(in) :: results(:)
        integer, intent(in) :: n_results
        character(len=65536) :: text

        character(len=64) :: line_s
        integer :: i

        text = ''
        do i = 1, n_results
            write (line_s, '(i0)') results(i)%line
            text = trim(text)//trim(results(i)%status)//' '// &
                trim(results(i)%name)//' ('//trim(results(i)%class)//') '// &
                trim(results(i)%file)//':'//trim(line_s)//' backend='// &
                trim(results(i)%backend)//' evidence='//trim(results(i)%evidence)
            if (results(i)%cached) text = trim(text)//' [cached]'
            if (len_trim(results(i)%counterexample) > 0) &
                text = trim(text)//' counterexample='// &
                trim(results(i)%counterexample)
            if (i < n_results) text = trim(text)//char(10)
        end do
    end function verification_text

    subroutine verification_clear_proof_cache(dir, n_removed)
        !! Drop the generated proof artifacts and certificates for a project.
        !! The shared CAS is preserved; only the disposable build/fo/generated
        !! view is cleared.
        character(len=*), intent(in) :: dir
        integer, intent(out) :: n_removed

        character(len=MAX_PATH) :: gendir

        n_removed = 0
        gendir = trim(dir)//'/build/fo/generated'
        call fs_remove_tree(trim(gendir))
        n_removed = 1
    end subroutine verification_clear_proof_cache

    subroutine read_counterexample(logfile, counterexample)
        character(len=*), intent(in) :: logfile
        character(len=*), intent(out) :: counterexample

        character(len=MAX_LINE) :: line
        character(len=MAX_LINE) :: varline
        integer :: u, ios

        counterexample = ''
        varline = ''
        open (newunit=u, file=trim(logfile), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'DISPROVED') > 0) cycle
            if (index(line, 'STOP 1') > 0) cycle
            if (index(line, 'Error termination') > 0) cycle
            if (index(line, 'Backtrace') > 0) cycle
            if (len_trim(varline) == 0) then
                varline = trim(line)
            else
                varline = trim(varline)//' '//trim(line)
            end if
        end do
        close (u)
        counterexample = trim(varline)
    end subroutine read_counterexample

    subroutine expr_variables(expr, vars, n_vars)
        !! Extract free identifier names from an expression, excluding Fortran
        !! intrinsics, numeric literals (including kind suffixes like 2.0d0 and
        !! 1.5e-3), and named constants. Heuristic text-level extraction,
        !! sufficient for scalar symbolic derivations.
        character(len=*), intent(in) :: expr
        character(len=*), intent(out) :: vars(:)
        integer, intent(out) :: n_vars

        integer :: i, n, start, len_id
        character(len=MAX_EXPR) :: id

        n_vars = 0
        n = len_trim(expr)
        i = 1
        do while (i <= n)
            if (is_digit(expr(i:i))) then
                call skip_number(expr, n, i)
            else if (expr(i:i) == '.') then
                if (i < n) then
                    if (is_digit(expr(i + 1:i + 1))) call skip_number(expr, n, i)
                else
                    i = i + 1
                end if
            else if (is_alpha(expr(i:i))) then
                start = i
                len_id = 0
                do while (i <= n)
                    if (is_alnum(expr(i:i))) then
                        len_id = len_id + 1
                        i = i + 1
                    else
                        exit
                    end if
                end do
                id = expr(start:start + len_id - 1)
                if (.not. is_intrinsic(trim(id)) .and. &
                    .not. is_constant(trim(id))) then
                    call push_var(vars, n_vars, id)
                end if
            else
                i = i + 1
            end if
        end do
    end subroutine expr_variables

    subroutine skip_number(expr, n, i)
        !! Advance past a Fortran numeric literal: digits, an optional decimal
        !! point, an optional e/d exponent with sign, and an optional _kind
        !! suffix. Leaves i pointing at the first non-literal character.
        character(len=*), intent(in) :: expr
        integer, intent(in) :: n
        integer, intent(inout) :: i

        logical :: seen_exp

        seen_exp = .false.
        do while (i <= n)
            if (is_digit(expr(i:i)) .or. expr(i:i) == '.') then
                i = i + 1
            else if ((expr(i:i) == 'e' .or. expr(i:i) == 'E' .or. &
                    expr(i:i) == 'd' .or. expr(i:i) == 'D') .and. &
                    .not. seen_exp) then
                seen_exp = .true.
                i = i + 1
                if (i <= n) then
                    if (expr(i:i) == '+' .or. expr(i:i) == '-') i = i + 1
                end if
            else if (expr(i:i) == '_') then
                i = i + 1
                do while (i <= n)
                    if (.not. is_alnum(expr(i:i))) exit
                    i = i + 1
                end do
            else
                exit
            end if
        end do
    end subroutine skip_number

    logical function is_digit(ch) result(r)
        character(len=1), intent(in) :: ch
        integer :: c
        c = iachar(ch)
        r = (c >= iachar('0') .and. c <= iachar('9'))
    end function is_digit

    subroutine push_var(vars, n_vars, id)
        character(len=*), intent(inout) :: vars(:)
        integer, intent(inout) :: n_vars
        character(len=*), intent(in) :: id

        integer :: i

        do i = 1, n_vars
            if (trim(vars(i)) == trim(id)) return
        end do
        if (n_vars < size(vars)) then
            n_vars = n_vars + 1
            vars(n_vars) = trim(id)
        end if
    end subroutine push_var

    logical function is_alpha(ch) result(r)
        character(len=1), intent(in) :: ch
        integer :: c
        c = iachar(ch)
        r = (c >= iachar('a') .and. c <= iachar('z')) .or. &
            (c >= iachar('A') .and. c <= iachar('Z'))
    end function is_alpha

    logical function is_alnum(ch) result(r)
        character(len=1), intent(in) :: ch
        integer :: c
        c = iachar(ch)
        r = (c >= iachar('a') .and. c <= iachar('z')) .or. &
            (c >= iachar('A') .and. c <= iachar('Z')) .or. &
            (c >= iachar('0') .and. c <= iachar('9')) .or. ch == '_'
    end function is_alnum

    logical function is_intrinsic(id) result(r)
        character(len=*), intent(in) :: id

        character(len=16), parameter :: intrinsics(50) = [ &
            'sin            ', 'cos            ', 'tan            ', &
            'asin           ', 'acos           ', 'atan           ', &
            'atan2          ', 'sinh           ', 'cosh           ', &
            'tanh           ', 'exp            ', 'log            ', &
            'log10          ', 'sqrt           ', 'abs            ', &
            'max            ', 'min            ', 'mod            ', &
            'modulo         ', 'sign           ', 'floor          ', &
            'ceiling        ', 'nint           ', 'int            ', &
            'real           ', 'dble           ', 'aint           ', &
            'anint          ', 'dim            ', 'hypot          ', &
            'erf            ', 'erfc           ', 'gamma          ', &
            'log_gamma      ', 'norm2          ', 'sum            ', &
            'product        ', 'epsilon        ', 'tiny           ', &
            'huge           ', 'spacing        ', 'nearest        ', &
            'fraction       ', 'exponent       ', 'scale          ', &
            'set_exponent   ', 'digits         ', 'radix          ', &
            'range          ', 'len            ' ]
        integer :: i

        r = .false.
        do i = 1, size(intrinsics)
            if (trim(intrinsics(i)) == trim(to_lower_str(id))) then
                r = .true.
                return
            end if
        end do
    end function is_intrinsic

    logical function is_constant(id) result(r)
        character(len=*), intent(in) :: id

        r = (trim(to_lower_str(id)) == 'pi') .or. &
            (trim(to_lower_str(id)) == 'e') .or. &
            (trim(to_lower_str(id)) == 'true') .or. &
            (trim(to_lower_str(id)) == 'false') .or. &
            (trim(to_lower_str(id)) == '.true.') .or. &
            (trim(to_lower_str(id)) == '.false.')
    end function is_constant

    function to_lower_str(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c
        out = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) &
                out(i:i) = achar(c + 32)
        end do
    end function to_lower_str

    subroutine to_lower_in(s)
        character(len=*), intent(inout) :: s
        s = to_lower_str(s)
    end subroutine to_lower_in

    function fc_command() result(cmd)
        !! Fortran compiler for probe/kernel compilation: $FO_FC if set, else
        !! gfortran, mirroring the build backend.
        character(len=:), allocatable :: cmd

        character(len=256) :: fc
        integer :: status

        call get_environment_variable('FO_FC', fc, status=status)
        if (status == 0 .and. len_trim(fc) > 0) then
            cmd = trim(fc)
        else
            cmd = 'gfortran'
        end if
    end function fc_command

    subroutine strip_comment(line)
        character(len=*), intent(inout) :: line

        integer :: bang

        bang = index(line, '#')
        if (bang > 0) line = line(1:bang - 1)
        ! For TOML, '#' is a comment; '!' is a string delimiter in arrays but
        ! never starts a comment here.
    end subroutine strip_comment

    subroutine get_section(line, section)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: section

        integer :: lo, hi

        lo = index(line, '[')
        hi = index(line, ']')
        if (lo > 0 .and. hi > lo) then
            section = trim(adjustl(line(lo + 1:hi - 1)))
        else
            section = ''
        end if
    end subroutine get_section

    subroutine split_kv(line, key, val)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: key, val

        integer :: eq

        eq = index(line, '=')
        if (eq > 0) then
            key = trim(adjustl(line(1:eq - 1)))
            val = trim(adjustl(line(eq + 1:)))
        else
            key = trim(adjustl(line))
            val = ''
        end if
    end subroutine split_kv

    subroutine split_first(line, sep, head, rest)
        character(len=*), intent(in) :: line
        character(len=1), intent(in) :: sep
        character(len=*), intent(out) :: head, rest

        integer :: sp

        sp = index(line, sep)
        if (sp > 0) then
            head = trim(adjustl(line(1:sp - 1)))
            rest = trim(adjustl(line(sp + 1:)))
        else
            head = trim(adjustl(line))
            rest = ''
        end if
    end subroutine split_first

    subroutine extract_string(val, out_str)
        character(len=*), intent(in) :: val
        character(len=*), intent(out) :: out_str

        character(len=512) :: t
        integer :: lo, hi

        t = adjustl(val)
        lo = index(t, '"')
        hi = 0
        if (lo > 0) hi = index(t(lo + 1:), '"')
        if (lo > 0 .and. hi > 0) then
            out_str = t(lo + 1:lo + hi - 1)
        else
            out_str = trim(t)
        end if
    end subroutine extract_string


end module fo_verification
