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

%type <ast_node> array_items
%type <ast_node> array_literal
%type <ast_node> assignment
%type <ast_node> call
%type <ast_node> curly_expressions
%type <ast_node> def
%type <ast_node> expression
%type <ast_node> expressions
%type <ast_node> for
%type <ast_node> binop
%type <ast_node> identifier
%type <ast_node> number
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
	 DEBUG_PARSER("top_level $1 %p\n", $1);
	 *result = $1;
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
		 DEBUG_PARSER("assignment $1 %p $5 %p\n", $1, $3);
		 MAKE_NODE(ret, ASSIGNMENT_NODE);
		 $1->next_sibling = $3;
		 ret->first_child = $1;
		 $$ = ret;
}


identifier: IDENTIFIER {
		 DEBUG_PARSER("identifier $1 %p name=%s\n", $1, $1);
		 MAKE_NODE(ret, IDENTIFIER_NODE);
		 ret->name = $1;
		 SET_LOC(@1);
		 $$ = ret;
}

curly_expressions:
		'{' expressions_delimiter_zero_or_more expressions expressions_delimiter_zero_or_more '}' { $$ = $3; }
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

expression: assignment | binop | number | identifier | call | for | array_literal | def;

binop: expression BINOP expression {
		DEBUG_PARSER("expression $1 %p $3 %p\n", $1, $3);
		MAKE_NODE(ret, CALL_NODE);
		MAKE_NODE(id, IDENTIFIER_NODE);
		$1->next_sibling = $3;
		id->next_sibling = $1;
		id->name = $2;
		ret->first_child = id;
		SET_LOC(@2);
		$$ = ret;
}

call: expression '(' expression ')' {
		MAKE_NODE(ret, CALL_NODE);
		ret->first_child = $1;
		ret->first_child->next_sibling = $3;
		$$ = ret;
}

for:
		/* for(i;n) => for(i=0;i<n;i=i+1) */
		FOR '(' identifier ';' expression ')' curly_expressions {
			/* Work in progress */
			MAKE_NODE(ret, FOR_NODE);

				MAKE_NODE(init_node, ASSIGNMENT_NODE);
				ret->first_child = init_node;
					COPY_NODE(init_node->first_child, $3);
					ALLOC_NODE(init_node->first_child->next_sibling, NUMBER_NODE);
					init_node->first_child->next_sibling->number = 0;

				MAKE_NODE(cond_node, CALL_NODE);
				init_node->next_sibling = cond_node;
					ALLOC_NODE(cond_node->first_child, IDENTIFIER_NODE);
					cond_node->first_child->name = strdup("<");
					COPY_NODE(cond_node->first_child->next_sibling, $3);
					cond_node->first_child->next_sibling->next_sibling = $5;

				MAKE_NODE(incr_node, ASSIGNMENT_NODE);
				cond_node->next_sibling = incr_node;

					COPY_NODE(incr_node->first_child, $3);

					MAKE_NODE(incr_plus_node, CALL_NODE);
					incr_node->first_child->next_sibling = incr_plus_node;
						ALLOC_NODE(incr_plus_node->first_child, IDENTIFIER_NODE);
						incr_plus_node->first_child->name = strdup("+");
						COPY_NODE(incr_plus_node->first_child->next_sibling, $3);
						ALLOC_NODE(incr_plus_node->first_child->next_sibling->next_sibling, NUMBER_NODE);
						incr_plus_node->first_child->next_sibling->next_sibling->number = 1;

				incr_node->next_sibling = $7;

			$$ = ret;
		}
		| FOR '(' expression ';' expression ';' expression ')' curly_expressions {
		MAKE_NODE(ret, FOR_NODE);
		$3->next_sibling = $5;
		$5->next_sibling = $7;
		$7->next_sibling = $9;
		ret->first_child = $3;
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
				ret->first_child = $2->first_child;
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

def:
		DEF identifier '(' optional_arguments ')' curly_expressions {
			/* Work in progress */

		}

optional_arguments: /* nothing */;

number: NUMBER { MAKE_NODE(ret, NUMBER_NODE); ret->number = $1; $$ = ret; }

%%
