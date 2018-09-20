#!/usr/bin/env bash

set -eu

sed -n '/^typedef/p; /^struct/,/^$/p; /define YY_MAX_LINES/p;'
echo 'int yyparse(yycontext *yyctx);'
echo 'yycontext * yyrelease(yycontext *yyctx);'
