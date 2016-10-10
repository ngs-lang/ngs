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
syn match   ngsComment "#.*" contains=ngsTodo
syn match   ngsComment "^[ \t]*doc .*" contains=ngsTodo
"syn match   ngsTest "^TEST .*"
syn keyword ngsKeyword A B C and break breaks catch collector collect cond continue continues econd ematch eswitch F global guard local match or return returns switch TEST throw throws tor try type upvar while with X Y Z .. ...
syn keyword ngsConditional if then else
syn keyword ngsRepeat for
" bootstrap types
syn keyword ngsType NotImplemented FetchFail RequreFail MainFail
" other types
syn keyword ngsType Any ArgvMatcher Arr ArrIter BasicType Bool Box CLib Closure Command Counter CSym EmptyBox ExclusiveRange ExecutableNotFound FFI FullBox Fun Hash InclusiveRange Int Iter KV NormalType NormalTypeInstance Null Path Pipe Process ProcessFail Range Real Seq Stats Str Table TableMeta TableMetaNotIfCol Type
syn keyword ngsType AssertFail ArgsMismatch Error Exception CompileFail DontKnowHowToCall GlobalNotFound InvalidArgument ImplNotFound InternalError LookupFail KeyNotFound StackDepthFail
syn keyword ngsType IndexNotFound AttrNotFound InvalidParameter NoNext
syn keyword ngsType Lock Pthread PthreadAttr Thread
syn keyword ngsType LockFail
syn keyword ngsType RegExp RegExpCompileFail
syn keyword ngsType Return
syn keyword ngsType ScreenRenderer ScreenItemsContainer ScreenItemsVerticalContainer ScreenItemsHorizontalContainer
syn keyword ngsType Match MatchY MatchN MatchFail SubSeq Pfx MaybePfx MustPfx Sfx MaybeSfx MustSfx
syn keyword ngsType Props
syn keyword ngsType PubSub
syn keyword ngsTodo TODO FIXME XXX NOTE
syn keyword ngsConstant true false null
syn keyword ngsPredefinedVariable ARGV ARGV0 ENV ORIG_ARGV
" Special methods
syn keyword ngsPredefinedVariable init call args
syn keyword ngsKeyword super

" strings
syn match   ngsSpecial contained #\$#
syn region  ngsString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=ngsSpecial
syn region  ngsString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=ngsSpecial

syn match   ngsNumber "\<\d\+\>"

" builtin functions
syn keyword ngsFunction c_close c_execve c_exit c_fork c_lseek copy c_open c_pcre_compile c_pcre_exec c_read c_waitpid C_WEXITSTATUS C_WTERMSIG compile del dump echo encode_json get get_c_errno globals hash impl_not_found_hook in inherit is keys len load not decode_json push shift typeof values
" bootstrap functions (only the ones that are relevant for later usage)
syn keyword ngsFunction fetch main require to_exit_code
" stdlib functions
syn keyword ngsFunction acquire all any assert basename close_reading_end close_writing_end cmp code compare count dflt dup2 dup2_reading_end dup2_writing_end each each_idx_key_val each_idx_val ends_with error expose debug die filter find_in_path first flatten group has identity in index join len limit log map mapk mapv mapkv max merge_sorted min n next none partial partial_tail peek pmap pos ptimes publish read release reverse set sort split starts_with status Strs subscribe sum uniq update wait without write zip

" resources types
syn keyword ngsType Res ResDef ResNotFound

syn keyword ngsType AwsRes AwsResDef
syn keyword ngsType AwsAncor

syn keyword ngsType AwsElb AwsElbRes
syn keyword ngsType AwsImage AwsImageRes
syn keyword ngsType AwsInstance AwsInstanceRes
syn keyword ngsType AwsRecordSet AwsRecordSetRes
syn keyword ngsType AwsSecGroup AwsSecGroupRes
syn keyword ngsType AwsVpc AwsVpcRes

" stdlib resources functions
syn keyword ngsFunction converge create delete expect find validate

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

let b:current_syntax = "ngs"

" vim: nowrap sw=4 sts=4 ts=4 et:

