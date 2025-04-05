" Vim syntax file
" Language:     NGS
" Maintainer:   Ilya Sher
" URL:			
" Release Coordinator:	
" ----------------------------------------------------------------------------
"
" if exists("b:current_syntax")
"   finish
" endif

setf ngs
syn match   ngsOperator "[-~!^&|*/%+=<>`]\+"
syn match   ngsComment " #.*" contains=ngsTodo
syn match   ngsComment "^\s*#.*" contains=ngsTodo
syn match   ngsComment "^[ \t]*doc .*" contains=ngsTodo
"syn match   ngsTest "^TEST .*"
syn keyword ngsKeyword A B C and block break breaks catch collector collect cond continue continues econd ematch eswitch do F global guard local match ns or return returns section switch TEST throw throws tor try type upvar while with X Y Z .. ...
syn keyword ngsConditional if then else
syn keyword ngsRepeat for

" Generated types - start
syn keyword ngsType Absent AllOf Any AnyOf ArgsMismatch Arr ArrDiff ArrIter ArrLike ArrSplatMethodParam AssertFail AtPath AwsElb AwsElbRes AwsImage AwsImageRes AwsInstance AwsInstanceRes AwsRecordSet AwsRecordSetRes AwsRes AwsResDef AwsSecGroup AwsSecGroupRes AwsSubnet AwsSubnetRes AwsVpc AwsVpcRes
syn keyword ngsType Backtrace BasenameArgumentFail BasicType BasicTypeInstance Block BlockDevice Bool Box BoxFail Branch
syn keyword ngsType CError CLib CSym C_DIR CallFail Capture CdFail CharDevice CleanupPolicy CollectingPipeFromChildToParentProcess Command CommandRedir CommandsPipe CommandsPipeline CompileFail ConstIter
syn keyword ngsType DecodeFail DecodeHints DelimStr DeprecatedTopLevelHashlike Diff Dir DirFail DivisionByZero DlopenFail DontKnowHowToCall Duration
syn keyword ngsType Eachable Eachable1 Eachable2 ElementNotFound EmptyArrayFail EmptyBox EmptyEachableFail Error ExactPresence Exception Exit ExitCodeFail ExitException
syn keyword ngsType Failure FailuresException FatalError FieldNotFound FifoFile File FileIOFail FilterIter FullBox Fun FunIter
syn keyword ngsType GlobalNotFound
syn keyword ngsType Hash HashDiff HashIter HashLike HashSplatMethodParam Hook
syn keyword ngsType IfExists Ifx IndexNotFound InstantiatingAbstractType Int InternalError InvalidArgument InvalidCommandLineArguments Iter
syn keyword ngsType JWT JsonDataViaGet JsonDataViaHash JsonDecodeFail
syn keyword ngsType KeepCleanupPolicy KeepOnErrorCleanupPolicy KeyNotFound KillFail
syn keyword ngsType LLHashEntry Lines Lit Lock LockFail LookupFail
syn keyword ngsType MainFail MapIter MatchContext MatchFailure MatchResult MatchSuccess MaybeIfx MaybePfx MaybeSfx MethodNotFound MethodParam MethodParams MultiMethod MustIfx MustPfx MustSfx
syn keyword ngsType NamedInstances Namespace NativeMethod NgsStrComp NgsStrCompExp NgsStrCompImm NgsStrCompSplatExp NoData NoEmptyInit NoNext NormalExit NormalType NormalTypeConstructor NormalTypeInstance Not NotImplemented Null Num NumRange
syn keyword ngsType OptionalMethodParam
syn keyword ngsType ParamsMatchN ParamsMatchY PartialPresence Path PatternAction Pfx Pipe PipeCreateFail PipeFromChildProcess PipeFromChildToParentProcess PipeFromParentToChildProcess PipeToChildProcess PredRange Presence Present Process ProcessRedir ProcessesPipeline Program ProgramNotFound
syn keyword ngsType Range RangeIter ReadFail ReadingPipeBetweenChildren Real ReentrantLock RegExp RegExpCompileFail RemoveCleanupPolicy Repeat RequireFail RequiredMethodParam Res ResDef ResetPath Result ResultFail Results ResultsException RetryAssertFail RetryBodyMissing RetryFail ReturnFromBlock
syn keyword ngsType Seq Set Sfx SkipError SocketFile SplatMethodParam StackDepthFail StackOverflow Stat StatFail Stats Str SubSeq Success SwitchFail SwitchParseFail Symlink
syn keyword ngsType Table TestFail TestMessage TestsResults Thread ThreadFail Threads Time TimeFail TmpDir TmpFile TmpFsObj Transformed TtyCheckFail Type
syn keyword ngsType UndefinedLocalVar UndefinedUpVar UserDefinedMethod
syn keyword ngsType WriteFail WritingPipeBetweenChildren
syn keyword ngsType c_ffi_cif c_ffi_type c_pthread_attr_t c_pthread_cond_t c_pthread_mutex_t c_pthread_mutexattr_t c_pthread_t c_sockaddr c_sockaddr_in c_sockaddr_un c_tm
" Generated types - end

" Generated methods - start
syn keyword ngsFunction Arg Argv ArgvMatcher
syn keyword ngsFunction C_WEXITSTATUS C_WTERMSIG
syn keyword ngsFunction ExitCode
syn keyword ngsFunction JsonData
syn keyword ngsFunction SafeStr StrForTable Strs
syn keyword ngsFunction TODO
syn keyword ngsFunction Value
syn keyword ngsFunction _inspect_path_to_str
syn keyword ngsFunction abs access acquire after_last all any apply args arr_splat assert assert_array assert_base assert_bool assert_eq assert_exit_code assert_has assert_hash assert_hash_keys assert_hash_keys_values assert_in assert_match assert_min_len assert_output_has assert_path_exists assert_resolvable assert_string attrs avg
syn keyword ngsFunction band basename before_first bootstrap bootstrap_exception_catch_wrapper bootstrap_invoke bootstrap_print_compilation_warnings bootstrap_try_method bor bxor
syn keyword ngsFunction c_accept c_access c_bind c_chdir c_close c_closedir c_connect c_dlopen c_errno c_execve c_exit c_ffi_call c_ffi_prep_cif c_fork c_fstat c_getpid c_getppid c_gettimeofday c_gmtime c_isatty c_kill c_listen c_localtime c_lseek c_lstat c_mktime c_open c_opendir c_pcre_compile c_pcre_exec c_pipe c_poll c_pow c_pthread_attr_init c_pthread_cond_broadcast c_pthread_cond_destroy c_pthread_cond_init c_pthread_cond_signal c_pthread_cond_wait c_pthread_create c_pthread_join c_pthread_mutex_init c_pthread_mutex_lock c_pthread_mutex_unlock c_pthread_mutexattr_init c_pthread_mutexattr_settype c_pthread_self c_read c_readdir c_realpath c_recvfrom c_send c_socket c_stat c_strcasecmp c_strcmp c_strerror c_strftime c_strptime c_sysconf c_time c_waitpid c_write cached calculate_num_cols_to_show call ceil cell_display_width chdir child_fd chr close close_reading_end close_writing_end code column columns compile config converge copy count create created
syn keyword ngsFunction debug decode decode_hex decode_json decode_uri_component deeper del delete dflt die digest dir drop dump duplicates
syn keyword ngsFunction each each_chunk each_group_test each_idx_key_val each_idx_val eachk eachv echo echo_cli encode encode_hex encode_html encode_html_attr encode_json encode_uri_component ends_with ensure ensure_array error exception_specific_message exec exit expect
syn keyword ngsFunction fetch fields filter filterk filtero filterv finally find find_if_needed find_in_path finished finished_ok first flat_map flatten floor fork framed
syn keyword ngsFunction get glob global_not_found_handler globals gmtime group
syn keyword ngsFunction has has_index has_no hash hash_splat human_type_name
syn keyword ngsFunction id identity ids in index indexes inherit init inspect intersperse ip is is_blocked_group is_subtype isatty
syn keyword ngsFunction join
syn keyword ngsFunction keys kill
syn keyword ngsFunction last latest len limit lines ll_hash_entry_key ll_hash_entry_next ll_hash_entry_val ll_hash_head ll_hash_tail ll_is_global_variable_defined ll_maybe_wrap ll_resolve_global_variable ll_set_global_variable ll_thread_local load localtime log lstat lte
syn keyword ngsFunction main map map_base_idx map_idx_key_val map_idx_val mapk mapkv mapo mapv max maybe_print_stacktrace merge_sorted meta method_not_found_handler min myip
syn keyword ngsFunction next none nop normalize_presence_list not
syn keyword ngsFunction only open opt_prop ord
syn keyword ngsFunction params parent_fd partial partial_tail partition partitionk partitionv peek pfilter pid pmap pop pos pow ppid print_exception progress ptimes push push_all
syn keyword ngsFunction rand rand_uniq read realpath reduce register_column reject rejectk rejectv release replace report req_prop require resolve_instruction_pointer retry retry_assert reverse round run
syn keyword ngsFunction set set_last_path_element shift skip sort sortk sortv specific split srand starts_with stat status stdlib_aws_straighten_tags store strftime subset sum sysconf
syn keyword ngsFunction take tap test the_one throw_if_no_next time times trim trunc typeof
syn keyword ngsFunction uniq unshift update users_ids
syn keyword ngsFunction values
syn keyword ngsFunction wait warn when width without words write
syn keyword ngsFunction zip
" Generated methods - end


" Special methods
syn keyword ngsPredefinedVariable init call args true false null
syn keyword ngsKeyword super
" Namespaces
syn keyword ngsNamespace AWS CHARS Doc OS

" strings
syn match   ngsSpecial contained #\$#
syn region  ngsString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=ngsSpecial
syn region  ngsString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=ngsSpecial
"syn region  ngsString start=+/+ end=+/+ skip=+\\\\\|\\"+ contains=ngsSpecial

syn match   ngsNumber "\<\d\+\>"

hi def link ngsComment Comment
hi def link ngsConditional Conditional
hi def link ngsConstant Constant
hi def link ngsFunction Function
hi def link ngsKeyword Keyword
hi def link ngsNumber Number
hi def link ngsOperator Operator
hi def link ngsPredefinedVariable Identifier
hi def link ngsRepeat Repeat
hi def link ngsSpecial Special
hi def link ngsString String
hi def link ngsTest PreProc
hi def link ngsTodo Todo
hi def link ngsType Type
hi def link ngsNamespace Keyword

let b:current_syntax = "ngs"

" vim: nowrap sw=4 sts=4 ts=4 et:
