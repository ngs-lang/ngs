#!/usr/bin/env bash

set -eu

SED=$1

if ! $SED --version >/dev/null 2>/dev/null;then
	echo "Failed to assert GNU sed: failed running sed --version" >&2
	exit 1
fi

if ! $SED --version | grep -iq 'GNU sed';then
	echo "Failed to assert GNU sed: 'sed --version' output had no 'GNU sed'" >&2
	exit 2
fi

$SED -n '/^typedef/p; /^struct/,/^$/p; /define YY_MAX_LINES/p;'
echo 'int yyparse(yycontext *yyctx);'
echo 'yycontext * yyrelease(yycontext *yyctx);'
