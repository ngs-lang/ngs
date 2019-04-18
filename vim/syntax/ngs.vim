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
syn keyword ngsKeyword A B C and break breaks catch collector collect cond continue continues econd ematch eswitch do F global guard local match ns or return returns switch TEST throw throws tor try type upvar while with X Y Z .. ...
syn keyword ngsConditional if then else
syn keyword ngsRepeat for

" Generated types - start
syn keyword ngsType Absent Any ArgsMismatch Arr ArrDiff ArrIter ArrLike ArrSplatMethodParam AssertFail AwsElb AwsElbRes AwsImage AwsImageRes AwsInstance AwsInstanceRes AwsRecordSet AwsRecordSetRes AwsRes AwsResDef AwsSecGroup AwsSecGroupRes AwsSubnet AwsSubnetRes AwsVpc AwsVpcRes
syn keyword ngsType Backtrace BasenameArgumentFail BasicType BasicTypeInstance BlockDevice Bool BootstrapFailedMatchMain BootstrapNoMatchingMain Box BoxFail
syn keyword ngsType CException CLib CSym C_DIR CallFail CdFail CharDevice CollectingPipeFromChildToParentProcess Command CommandRedir CommandsPipe CommandsPipeline CompileFail ConstIter
syn keyword ngsType DecodeFail DelimStr Diff Dir DirFail DivisionByZero DlopenFail DontKnowHowToCall
syn keyword ngsType Eachable Eachable1 Eachable2 ElementNotFound EmptyArrayFail EmptyBox EmptyEachableFail Error ExactPresence Exception ExecutableNotFound Exit ExitCodeFail ExitException
syn keyword ngsType Failure FatalError FieldNotFound FifoFile File FileIOFail FilterIter FullBox Fun FunIter
syn keyword ngsType GlobalNotFound
syn keyword ngsType Hash HashDiff HashIter HashLike HashSplatMethodParam Hook
syn keyword ngsType Ifx IndexNotFound InstantiatingAbstractType Int InternalError InvalidArgument Iter
syn keyword ngsType JsonDecodeFail
syn keyword ngsType KeyNotFound KillFail
syn keyword ngsType LLHashEntry Lines Lock LockFail LookupFail
syn keyword ngsType MainFail MapIter MatchFailure MatchResult MatchSuccess MaybeIfx MaybePfx MaybeSfx MethodNotFound MethodParam MethodParams MultiMethod MustIfx MustPfx MustSfx
syn keyword ngsType NativeMethod NgsStrComp NgsStrCompExp NgsStrCompImm NgsStrCompSplatExp NoData NoNext NormalExit NormalType NormalTypeConstructor NormalTypeInstance NotImplemented Null Num NumRange
syn keyword ngsType OptionalMethodParam
syn keyword ngsType ParamsMatchN ParamsMatchY PartialPresence Path Pfx Pipe PipeCreateFail PipeFromChildProcess PipeFromChildToParentProcess PipeFromParentToChildProcess PipeToChildProcess PredRange Presence Present Process ProcessRedir ProcessesPipeline
syn keyword ngsType Range RangeIter ReadFail ReadingPipeBetweenChildren Real ReentrantLock RegExp RegExpCompileFail RequireFail RequiredMethodParam Res ResDef Result ResultFail RetryFail Return
syn keyword ngsType Seq Set Sfx SocketFile SplatMethodParam StackDepthFail Stat StatFail Stats Str SubSeq Success SwitchFail SwitchParseFail Symlink
syn keyword ngsType Table TestFail TestMessage TestsResults Thread ThreadFail Threads Time TimeFail TtyCheckFail Type
syn keyword ngsType UndefinedLocalVar UserDefinedMethod
syn keyword ngsType WritingPipeBetweenChildren
syn keyword ngsType c_ffi_cif c_ffi_type c_pthread_attr_t c_pthread_mutex_t c_pthread_mutexattr_t c_pthread_t c_tm
" Generated types - end

" Generated methods - start
syn keyword ngsFunction Arg Argv ArgvMatcher
syn keyword ngsFunction C_WEXITSTATUS C_WTERMSIG
syn keyword ngsFunction ExitCode
syn keyword ngsFunction Pred
syn keyword ngsFunction SafeStr StrForTable StrParams Strs
syn keyword ngsFunction abs access acquire all any args arr_splat assert assert_array assert_base assert_bool assert_eq assert_exit_code assert_has assert_hash assert_hash_keys assert_hash_keys_values assert_in assert_match assert_min_len assert_output_has assert_path_exists assert_resolvable assert_string attempt attrs
syn keyword ngsFunction band basename body_missing_in_retry bootstrap bootstrap_debug bootstrap_exception_catch_wrapper bootstrap_find_ngs_dir bootstrap_invoke_main bootstrap_try_main bor bxor
syn keyword ngsFunction c_access c_chdir c_close c_closedir c_dlopen c_errno c_execve c_exit c_ffi_call c_ffi_prep_cif c_fork c_fstat c_getpid c_getppid c_gettimeofday c_gmtime c_isatty c_kill c_localtime c_lseek c_lstat c_mktime c_open c_opendir c_pcre_compile c_pcre_exec c_pipe c_poll c_pthread_attr_init c_pthread_create c_pthread_join c_pthread_mutex_init c_pthread_mutex_lock c_pthread_mutex_unlock c_pthread_mutexattr_init c_pthread_mutexattr_settype c_pthread_self c_read c_readdir c_stat c_strcasecmp c_strcmp c_strerror c_strftime c_strptime c_time c_waitpid c_write cached calculate_num_cols_to_show call ceil cell_display_width chdir child_fd chr close close_reading_end close_writing_end code collector column columns compile config converge copy count create created
syn keyword ngsFunction debug decode decode_hex decode_json decode_uri_component del delete dflt die digest dir drop dump
syn keyword ngsFunction each each_chunk each_group_test each_idx_key_val each_idx_val eachk eachv echo encode encode_hex encode_html encode_html_attr encode_json encode_uri_component ends_with ensure ensure_array error exception_specific_message exit expect
syn keyword ngsFunction fetch filter filterk filterv finally find find_if_needed find_in_path finished_ok first flatten floor framed
syn keyword ngsFunction get glob global_not_found_handler globals gmtime group
syn keyword ngsFunction has has_no hash hash_splat
syn keyword ngsFunction id identity ids in index indexes inherit init inspect intersperse ip is is_blocked_group is_subtype isatty
syn keyword ngsFunction join
syn keyword ngsFunction keys kill
syn keyword ngsFunction latest len limit lines ll_hash_entry_key ll_hash_entry_next ll_hash_entry_val ll_hash_head ll_hash_tail ll_is_global_variable_defined ll_resolve_global_variable ll_set_global_variable ll_thread_local load localtime log lstat lte
syn keyword ngsFunction main map map_base_idx map_idx_key_val map_idx_val mapk mapkv mapo mapv match max maybe_print_stacktrace merge_sorted method_not_found_handler min myip
syn keyword ngsFunction n next none nop normalize_presence_list not
syn keyword ngsFunction only open opt_prop ord
syn keyword ngsFunction params parent_fd partial partial_tail partition peek pid pmap pop pos ppid print_exception ptimes push push_all
syn keyword ngsFunction rand rand_uniq read reduce register_column reject rejectk rejectv release replace report req_prop require resolve_instruction_pointer retry reverse round run
syn keyword ngsFunction set shift sort sortk specific split srand starts_with stat status stdlib_aws_straighten_tags store strftime subset sum
syn keyword ngsFunction table take tap test the_one throw_if_no_next time times to_exit_code trunc typeof
syn keyword ngsFunction uniq unshift update users_ids
syn keyword ngsFunction values
syn keyword ngsFunction wait warn width without write
syn keyword ngsFunction zip
" Generated methods - end


" Special methods
syn keyword ngsPredefinedVariable init call args
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
