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
"syn match   ngsTest "^TEST .*"
syn keyword ngsKeyword and break breaks catch collector collect cond continue continues econd ematch eswitch F guard local match or return returns switch TEST throw throws try type while with .. ...
syn keyword ngsConditional if then else
syn keyword ngsRepeat for
syn keyword ngsType Any Arr BasicType Bool CLib Command CSym ExclusiveRange Fun Hash InclusiveRange Int NormalType Null Path Pipe Process ProcessFailed Range Seq Stats Str Type
syn keyword ngsType Error Exception CompileFail DontKnowHowToCall GlobalNotFound InvalidArgument ImplNotFound LookupFail KeyNotFound IndexNotFound AttrNotFound InvalidParameter
syn keyword ngsTodo TODO FIXME XXX NOTE
syn keyword ngsConstant true false null
syn keyword ngsPredefinedVariable ARGV ENV
" Special methods
syn keyword ngsPredefinedVariable init

" strings
syn match   ngsSpecial contained #\$#
syn region  ngsString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=ngsSpecial
syn region  ngsString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=ngsSpecial

syn match   ngsNumber "\<\d\+\>"

" builtin functions
syn keyword ngsFunction c_close c_execve c_exit c_fork c_lseek c_open c_read c_waitpid C_WEXITSTATUS C_WTERMSIG compile del dump echo get get_c_errno globals hash impl_not_found_hook in inherit is keys len load not parse_json push shift values
" bootstrap functions (only the ones that are relevant for later usage)
syn keyword ngsFunction fetch require to_exit_code
" stdlib functions
syn keyword ngsFunction all any close_reading_end close_writing_end count dup2 dup2_reading_end dup2_writing_end each error expose debug filter find_in_path first flatten has identity in len log map max min partial partial_tail pos read split status Strs uniq update wait without write zip

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

