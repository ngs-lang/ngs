# Location tracking
# YY_LOCAL(void) yyDo(yycontext *yy, yyaction action, int begin, int end, int location[4])
#                                                                       +++++++++++++++++
/^YY_LOCAL/ {
	s/\(yyDo\|yyPush\|yyPop\|yySet\)(\(.\+\))/\1(\2, int location[4])/
	t
}
# YY_ACTION(void) yy...(yycontext *yy, char *yytext, int yyleng, int location[4])
#                                                              +++++++++++++++++
/^YY_ACTION/ {
	s/\(yy.*\)(\(.\+\))/\1(\2, int location[4])/
	t
}
# typedef void (*yyaction)(yycontext *yy, char *yytext, int yyleng, int location[4]);
#                                                                 +++++++++++++++++
/^typedef.*yyaction/ {
	s/);/, int location[4]);/
	t
}
# typedef struct _yythunk { int begin, end;  yyaction  action;  struct _yythunk *next; int location[4]; } yythunk;
#                                                                                      +++++++++++++++++
/^typedef.*_yythunk/ {
	s/}/int location[4]; }/
	t
}

s/yyDo(\(yy, yy_[^)]\+\))/{ int loc[4]; position_to_line_col(yy, yypos0, loc); position_to_line_col(yy, yy->__pos, \&loc[2]); yyDo%%(\1, loc); };/g;

# Non actions do not need location to be set anyway
s/yyDo(\([^)]\+\))/yyDo(\1, (int []){0, 0, 0 ,0})/g

s/yyDo%%/yyDo/g

# thunk->action(yy, yy->__text, yyleng, thunk->location);
#                                     +++++++++++++++++
s/thunk->action(\([^)]\+\))/thunk->action(\1, thunk->location)/

# End of "static char *preamble" in peg's src/compile.c
/#define\s\+YYACCEPT/a void position_to_line_col(yycontext *yy, int pos, int result[]);
