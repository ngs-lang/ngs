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
syn keyword ngsKeyword catch collector collect F guard local return returns TEST throw try type with
syn keyword ngsConditional if then else
syn keyword ngsRepeat for
syn keyword ngsType Any Arr Bool CLib Command CSym Fun Hash Int Null Process Seq Str Type
syn keyword ngsTodo TODO FIXME XXX NOTE
syn keyword ngsConstant true false null
syn keyword ngsPredefinedVariable ARGV ENV
syn keyword ngsPredefinedVariable Error Exception LookupFail KeyNotFound IndexNotFound AttrNotFound InvalidParameter

" strings
syn match   ngsSpecial contained #\$#
syn region  ngsString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=ngsSpecial
syn region  ngsString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=ngsSpecial

syn match   ngsNumber "\<\d\+\>"

" builtin functions
syn keyword ngsFunction c_close c_exit c_fork c_lseek c_open c_read c_waitpid compile del dump echo get hash in is keys len load not push shift values
" bootstrap functions (only the ones that are relevant for later usage)
syn keyword ngsFunction fetch require to_exit_code
" stdlib functions
syn keyword ngsFunction all any count each error debug filter flatten has identity in len log map max min partial partial_tail status uniq update without zip

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

