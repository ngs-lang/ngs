%{
#define YYERROR_VERBOSE (1)
#include <string.h>
#include "ngs.h"
#include "ast.h"

#define YYMALLOC(size)        NGS_MALLOC(size)
#define YYREALLOC(ptr, size)  NGS_REALLOC(ptr, size)
#define YYCALLOC(nelem, size) assert(0=="YYCALLOC is not implemented")
#define YYFREE(ptr)           (void)(ptr)

// Without the following line you get message that struct YYLTYPE will not be visible outside the function yyerror
struct YYLTYPE;
void yyerror(struct YYLTYPE * yylloc_param, void *scanner, ast_node **result, const char *s);

// handle warnings - start
// int yylex (union YYSTYPE * yyval_param, struct YYLTYPE * yylloc_param, void * yyscanner);
int yylex();
// handle warnings - end

#define ALLOC_NODE(dst, type_) (dst) = NGS_MALLOC(sizeof(ast_node)); dst->type=type_
#define MAKE_NODE_LOC(name, type_, fl, fc, ll, lc, is_gen) \
	ast_node *name = NGS_MALLOC(sizeof(*name)); name->type = type_; \
	DEBUG_PARSER("[ast_node] at %p type %3d (%-12s) starts %d:%d ends %d:%d is_generated %d\n", name, name->type, NGS_AST_NODE_TYPES_NAMES[name->type], fl, fc, ll, lc, is_gen); \
	name->location.first_line = fl; \
	name->location.first_column = fc; \
	name->location.last_line = ll; \
	name->location.last_column = lc
#define MAKE_NODE(name, type_) MAKE_NODE_LOC(name, type_, yyloc.first_line, yyloc.first_column, yyloc.last_line, yyloc.last_column, 0)
// TODO: check whether it's appropriate to use Boehm's "atomic" allocation.
#define COPY_NODE(dst, src) (dst) = NGS_MALLOC(sizeof(ast_node)); memcpy((dst), (src), sizeof(ast_node))
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

%token DEFINED
%token FOR
%token STR_BEGIN
%token <name> STR_COMP_IMM
%token STR_END
%token NULL_TOK TRUE_TOK FALSE_TOK
%token WHILE

%token <name> BINOP
%token <name> IDENTIFIER
%token <number> NUMBER

%type <ast_node> array_items
%type <ast_node> array_literal
%type <ast_node> assignment
%type <ast_node> call
%type <ast_node> curly_expressions
%type <ast_node> curly_expressions_only
%type <ast_node> defined
%type <ast_node> expression
%type <ast_node> expressions
%type <ast_node> f
%type <ast_node> false
%type <ast_node> for
%type <ast_node> binop
%type <ast_node> identifier
%type <ast_node> null
%type <ast_node> number
%type <ast_node> optional_func_name
%type <ast_node> optional_parameters
%type <ast_node> optional_string_components
%type <ast_node> parameter
%type <ast_node> quoted_identifier
%type <ast_node> string
%type <ast_node> string_component
%type <ast_node> top_level
%type <ast_node> top_level2
%type <ast_node> top_level_item
%type <ast_node> true

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
		 $$ = ret;
}

quoted_identifier: STR_BEGIN STR_COMP_IMM STR_END {
		 DEBUG_PARSER("quoted identifier $2 %p name=%s\n", $2, $2);
		 MAKE_NODE(ret, IDENTIFIER_NODE);
		 ret->name = $STR_COMP_IMM;
		 $$ = ret;
}

curly_expressions_only:
		'{' expressions_delimiter_zero_or_more expressions expressions_delimiter_zero_or_more '}' { $$ = $expressions; }

curly_expressions:
		curly_expressions_only
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
			DEBUG_PARSER("expression $1 %p\n", $1);
			MAKE_NODE(ret, EXPRESSIONS_NODE);
			ret->first_child = $1;
			ret->last_child = $1;
			$$ = ret;
		};

expressions_delimiter: ';' | '\n';

expressions_delimiter_one_or_more: expressions_delimiter_one_or_more expressions_delimiter | expressions_delimiter;

expressions_delimiter_zero_or_more: expressions_delimiter_one_or_more | /* nothing */;

expression: assignment | binop | number | identifier | call | for | array_literal | f | string | null | true | false | defined;

binop: expression[e1] BINOP expression[e2] {
		DEBUG_PARSER("binop $e1 %p $e2 %p\n", $e1, $e2);
		MAKE_NODE(ret, CALL_NODE);
			MAKE_NODE(id, IDENTIFIER_NODE);
			ret->first_child = id;
			id->name = $BINOP;
			id->next_sibling = $e1;
			$e1->next_sibling = $e2;
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
		'F' optional_func_name[name] '(' optional_parameters ')' curly_expressions[body] {
			MAKE_NODE(ret, FUNC_NODE);
			ret->first_child = $optional_parameters;
			ret->first_child->next_sibling = $body;
			$$ = ret;
			if($name) {
				// TODO: check that source locations are correct
				MAKE_NODE_LOC(ret_assign, ASSIGNMENT_NODE, @name.first_line, @name.first_column, @name.last_line, @name.last_column, 1);
				ret_assign->first_child = $name;
				$name->next_sibling = ret;
				$$ = ret_assign;
			}
		}

optional_func_name:
	identifier
	|
	quoted_identifier
	| /* nothing */ { $$=NULL; };

optional_parameters:
	optional_parameters[parameters] ',' parameter {
		// printf("PARAMS MULTI\n");
		$parameters->last_child->next_sibling = $parameter;
		$parameters->last_child = $parameter;
		$$ = $parameters;
	}
	| parameter {
		// printf("PARAMS ONE\n");
		MAKE_NODE(ret, PARAMS_NODE);
		ret->first_child = $parameter;
		ret->last_child = $parameter;
		$$ = ret;
	}
	|
	/* nothing */ {
		MAKE_NODE(ret, PARAMS_NODE);
		printf("PARAMS NONE\n");
		ret->first_child = NULL;
		ret->last_child = NULL;
		$$ = ret;
	};

parameter:
	identifier ':' curly_expressions {
		MAKE_NODE(ret, PARAM_NODE);
		ret->first_child = $identifier;
		$identifier->next_sibling = $curly_expressions;
		$$ = ret;
	}
	/* f(arg) is same as f(x:Any) */
	| identifier {
		MAKE_NODE(ret, PARAM_NODE);
		ret->first_child = $identifier;
			MAKE_NODE(any_type, IDENTIFIER_NODE);
			any_type->name = "Any";
			$identifier->next_sibling = any_type;
		$$ = ret;
	}

string: STR_BEGIN optional_string_components STR_END {
	  $$ = $optional_string_components;
	}

optional_string_components:
	optional_string_components[opt_comps] string_component {
		// printf("STR COMPS MULTI\n");
		$opt_comps->last_child->next_sibling = $string_component;
		$opt_comps->last_child = $string_component;
		$$ = $opt_comps;
	}
	| string_component {
		// printf("STR COMPS ONE\n");
		MAKE_NODE(ret, STR_COMPS_NODE);
		ret->first_child = $string_component;
		ret->last_child = $string_component;
		$$ = ret;
	}
	|
	/* nothing */ {
		MAKE_NODE(ret, STR_COMPS_NODE);
		// printf("STR COMPS NONE\n");
		ret->first_child = NULL;
		ret->last_child = NULL;
		$$ = ret;
	};

string_component:
  STR_COMP_IMM { MAKE_NODE(ret, STR_COMP_IMM_NODE); ret->name = $STR_COMP_IMM; $$ = ret; }
  |
  identifier
  |
  curly_expressions_only;

number:
  NUMBER { MAKE_NODE(ret, NUMBER_NODE); ret->number = $NUMBER; $$ = ret; }

null:  NULL_TOK  { MAKE_NODE(ret, NULL_NODE); $$ = ret; }
true:  TRUE_TOK  { MAKE_NODE(ret, TRUE_NODE); $$ = ret; }
false: FALSE_TOK { MAKE_NODE(ret, FALSE_NODE); $$ = ret; }

defined: DEFINED identifier { MAKE_NODE(ret, DEFINED_NODE); ret->first_child = $identifier; $$ = ret; }


%%
