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
syn match   ngsOperator "[-~!^&|*/%+=<>]\+"
syn match   ngsComment "#.*" contains=ngsTodo
syn match   ngsComment "^[ \t]*doc .*" contains=ngsTodo
"syn match   ngsTest "^TEST .*"
syn keyword ngsKeyword and break breaks catch collector collect cond continue continues econd ematch eswitch F global guard local match or return returns switch TEST throw throws try type upvar while with .. ...
syn keyword ngsConditional if then else
syn keyword ngsRepeat for
syn keyword ngsType Any Arr ArrConsumer BasicType Bool CLib Closure Command Counter CSym ExclusiveRange ExecutableNotFound FFI Fun Hash InclusiveRange Int NormalType NormalTypeInstance Null Path Pipe Process ProcessFail Range Real Seq Stats Str Table TableMeta TableMetaNotIfCol Type
syn keyword ngsType Error Exception CompileFail DontKnowHowToCall GlobalNotFound InvalidArgument ImplNotFound LookupFail KeyNotFound StackDepthFail
syn keyword ngsType IndexNotFound AttrNotFound InvalidParameter
syn keyword ngsType Lock Pthread PthreadAttr Thread
syn keyword ngsType LockFail
syn keyword ngsType ScreenRenderer ScreenItemsContainer ScreenItemsVerticalContainer ScreenItemsHorizontalContainer
syn keyword ngsTodo TODO FIXME XXX NOTE
syn keyword ngsConstant true false null
syn keyword ngsPredefinedVariable ARGV ENV
" Special methods
syn keyword ngsPredefinedVariable init call

" strings
syn match   ngsSpecial contained #\$#
syn region  ngsString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=ngsSpecial
syn region  ngsString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=ngsSpecial

syn match   ngsNumber "\<\d\+\>"

" builtin functions
syn keyword ngsFunction c_close c_execve c_exit c_fork c_lseek c_open c_read c_waitpid C_WEXITSTATUS C_WTERMSIG compile del dump echo get get_c_errno globals hash impl_not_found_hook in inherit is keys len load not decode_json push shift values
" bootstrap functions (only the ones that are relevant for later usage)
syn keyword ngsFunction fetch main require to_exit_code
" stdlib functions
syn keyword ngsFunction acquire all any close_reading_end close_writing_end count dup2 dup2_reading_end dup2_writing_end each each_idx_val error expose debug filter find_in_path first flatten has identity in index join len log map max min none partial partial_tail pmap pos ptimes read release reverse split status Strs uniq update wait without write zip

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

