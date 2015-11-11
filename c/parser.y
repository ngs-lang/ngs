%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ngs.h"
#include "compile.h"

// handle warnings - start
int yylex();
int yyerror();
// handle warnings - end

#define ALLOC_NODE(dst, type_) (dst) = NGS_MALLOC(sizeof(ast_node)); dst->type=type_
#define MAKE_NODE(name, type_) ast_node *name = NGS_MALLOC(sizeof(*name)); name->type = type_; DEBUG_PARSER("ast_node %p has type %d\n", name, name->type)
// TODO: check whether it's appropriate to use Boehm's "atomic" allocation.
#define COPY_NODE(dst, src) (dst) = NGS_MALLOC(sizeof(ast_node)); memcpy((dst), (src), sizeof(ast_node))
#define SET_LOC(src) \
	yyloc.first_line   = src.first_line; \
	yyloc.first_column = src.first_column; \
	yyloc.last_line    = src.first_column; \
	yyloc.last_column  = src.last_column;
%}

%define api.pure full
/* cheat a little: supposed to be scanner_t scanner */
%parse-param { void * scanner }
%parse-param { ast_node ** result }
%lex-param   { void * scanner }
%locations

// Symbols.
%union
{
	int      number;
	char     *name;
	ast_node *ast_node;
};

%token DEF
%token FOR
%token WHILE

%token <name> BINOP
%token <name> IDENTIFIER
%token <number> NUMBER

%type <ast_node> argument
%type <ast_node> array_items
%type <ast_node> array_literal
%type <ast_node> assignment
%type <ast_node> call
%type <ast_node> curly_expressions
%type <ast_node> def
%type <ast_node> expression
%type <ast_node> expressions
%type <ast_node> f
%type <ast_node> for
%type <ast_node> binop
%type <ast_node> identifier
%type <ast_node> number
%type <ast_node> optional_arguments
%type <ast_node> top_level
%type <ast_node> top_level2
%type <ast_node> top_level_item

%right '='
%left BINOP
%left '('
/* %precedence xx */

/*TODO: intern symbols*/

%%

top_level: top_level2 {
	 DEBUG_PARSER("top_level $1 %p\n", $top_level2);
	 *result = $top_level2;
}

top_level2:
		top_level2 expressions_delimiter top_level_item {
			DEBUG_PARSER("top_level2 $1 %p $3 %p\n", $1, $3);
			$1->last_child->next_sibling = $3;
			$1->last_child = $3;
			$$ = $1;
		}
		| top_level2 expressions_delimiter {
			$$ = $1;
		}
		| top_level_item {
			MAKE_NODE(ret, EXPRESSIONS_NODE);
			ret->first_child = $1;
			ret->last_child = $1;
			$$ = ret;
		}
		| expressions_delimiter {
			MAKE_NODE(ret, EMPTY_NODE);
			$$ = ret;
		};

top_level_item: curly_expressions

assignment: identifier '=' expression {
		 DEBUG_PARSER("assignment $identifier %p $5 %p\n", $identifier, $expression);
		 MAKE_NODE(ret, ASSIGNMENT_NODE);
		 $identifier->next_sibling = $expression;
		 ret->first_child = $identifier;
		 $$ = ret;
}


identifier: IDENTIFIER {
		 DEBUG_PARSER("identifier $1 %p name=%s\n", $1, $1);
		 MAKE_NODE(ret, IDENTIFIER_NODE);
		 ret->name = $IDENTIFIER;
		 SET_LOC(@IDENTIFIER);
		 $$ = ret;
}

curly_expressions:
		'{' expressions_delimiter_zero_or_more expressions expressions_delimiter_zero_or_more '}' { $$ = $expressions; }
		| expression;

/* TODO: straighten this */
expressions:
		expressions expressions_delimiter_one_or_more expression {
			DEBUG_PARSER("expressions $1 %p $3 %p\n", $1, $3);
			$1->last_child->next_sibling = $3;
			$1->last_child = $3;
			$$ = $1;
		}
		| expression {
			MAKE_NODE(ret, EXPRESSIONS_NODE);
			ret->first_child = $1;
			ret->last_child = $1;
			$$ = ret;
		};

expressions_delimiter: ';' | '\n';

expressions_delimiter_one_or_more: expressions_delimiter_one_or_more expressions_delimiter | expressions_delimiter;

expressions_delimiter_zero_or_more: expressions_delimiter_one_or_more | /* nothing */;

expression: assignment | binop | number | identifier | call | for | array_literal | f | def;

binop: expression[e1] BINOP expression[e2] {
		DEBUG_PARSER("expression $e1 %p $e2 %p\n", $e1, $e2);
		MAKE_NODE(ret, CALL_NODE);
			MAKE_NODE(id, IDENTIFIER_NODE);
			ret->first_child = id;
			id->name = $BINOP;
			id->next_sibling = $e1;
			$e1->next_sibling = $e2;
			SET_LOC(@BINOP);
		$$ = ret;
}

call: expression '(' expression ')' {
		MAKE_NODE(ret, CALL_NODE);
		ret->first_child = $1;
		ret->first_child->next_sibling = $3;
		$$ = ret;
}

for:
		/* for(i;expr) => for(i=0;i<expr;i=i+1) */
		FOR '(' identifier[id] ';' expression[expr] ')' curly_expressions[body] {
			MAKE_NODE(ret, FOR_NODE);

				MAKE_NODE(init_node, ASSIGNMENT_NODE);
				ret->first_child = init_node;
					COPY_NODE(init_node->first_child, $id);
					ALLOC_NODE(init_node->first_child->next_sibling, NUMBER_NODE);
					init_node->first_child->next_sibling->number = 0;

				MAKE_NODE(cond_node, CALL_NODE);
				init_node->next_sibling = cond_node;
					ALLOC_NODE(cond_node->first_child, IDENTIFIER_NODE);
					cond_node->first_child->name = "<";
					COPY_NODE(cond_node->first_child->next_sibling, $id);
					cond_node->first_child->next_sibling->next_sibling = $expr;

				MAKE_NODE(incr_node, ASSIGNMENT_NODE);
				cond_node->next_sibling = incr_node;

					COPY_NODE(incr_node->first_child, $id);

					MAKE_NODE(incr_plus_node, CALL_NODE);
					incr_node->first_child->next_sibling = incr_plus_node;
						ALLOC_NODE(incr_plus_node->first_child, IDENTIFIER_NODE);
						incr_plus_node->first_child->name = "+";
						COPY_NODE(incr_plus_node->first_child->next_sibling, $id);
						ALLOC_NODE(incr_plus_node->first_child->next_sibling->next_sibling, NUMBER_NODE);
						incr_plus_node->first_child->next_sibling->next_sibling->number = 1;

				incr_node->next_sibling = $body;

			$$ = ret;
		}
		| FOR '(' expression[init] ';' expression[cond] ';' expression[incr] ')' curly_expressions[body] {
			MAKE_NODE(ret, FOR_NODE);
			$init->next_sibling = $cond;
			$cond->next_sibling = $incr;
			$incr->next_sibling = $body;
			ret->first_child = $init;
			$$ = ret;
		}

array_literal:
		'[' ']' {
				MAKE_NODE(ret, ARR_LIT_NODE);
				ret->first_child = NULL; // not needed because of calloc but want to be explicit
				$$ = ret;
		}
		| '[' array_items ']' {
				MAKE_NODE(ret, ARR_LIT_NODE);
				ret->first_child = $array_items->first_child;
				$$ = ret;
		}

array_items:
		array_items ',' expression {
			DEBUG_PARSER("array_items $1 %p $3 %p\n", $1, $3);
			$1->last_child->next_sibling = $3;
			$1->last_child = $3;
			$$ = $1;
		}
		| expression {
			MAKE_NODE(ret, EXPRESSIONS_NODE);
			ret->first_child = $1;
			ret->last_child = $1;
			$$ = ret;
		};

f:
 		'F' '(' optional_arguments ')' curly_expressions[body] {
			/* Work in progress */
			MAKE_NODE(ret, FUNC_NODE);
			// MAKE_NODE(args, EXPRESSIONS_NODE); /* wrong type */
			ret->first_child = $optional_arguments;
			ret->first_child->next_sibling = $body;
			$$ = ret;
		}
def:
		DEF identifier '(' optional_arguments ')' curly_expressions[body] {
			/* Work in progress */
			MAKE_NODE(ret, EXPRESSIONS_NODE);
			MAKE_NODE(args, EXPRESSIONS_NODE); /* wrong type */
			ret->first_child = args;
			args->next_sibling = $body;
			$$ = ret;
		}

optional_arguments:
	optional_arguments[arguments] ',' argument {
		printf("ARGS MULTI\n");
		$arguments->last_child->next_sibling = $argument;
		$arguments->last_child = $argument;
		$$ = $arguments;
	}
	| argument {
		printf("ARGS ONE\n");
		MAKE_NODE(ret, ARGS_NODE);
		ret->first_child = $argument;
		ret->last_child = $argument;
		$$ = ret;
	}
	|
	/* nothing */ {
		MAKE_NODE(ret, ARGS_NODE);
		printf("ARGS NONE\n");
		ret->first_child = NULL;
		ret->last_child = NULL;
		$$ = ret;
	};

argument:
	identifier;

number: NUMBER { MAKE_NODE(ret, NUMBER_NODE); ret->number = $NUMBER; $$ = ret; }

%%
