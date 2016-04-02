% NGSSYN(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngssyn - Next Generation Shell language syntax

# DESCRIPTION

NGS has two syntaxes. The **command syntax** covers the tasks of running programs and i/o redirection, mainly for good interactive (and simple scripting) experience. The **code syntax** is for everything else, giving access to a complete programming language.

## Command syntax

	ls
	ls $my_file
	ls $*my_files

This is the syntax at top level of the file or when you start an interactive shell. When in **code syntax** you can embed **command syntax** within `$(...)` and serveral other constructs.

## Code syntax

	{
		type T
		# unrelated
		for(i;10) dump(i)
	}


This is the syntax inside `{...}`.

## Commands separators

In both syntaxes, commands are separated by a new line or by `;` (semicolon).

## Command syntax - `{...}`

Embed **code syntax**.

	{ my_code }
	ls ${ name = "${prefix}${main}${suffix}"; name }

## Code syntax - `{...}`

Where code block is expected - code block (`{ code }` in this manual).

	if a == b {
		x = x + 1
		y = y + x
	}
	# Single expression does not need {...} where code block is expected.
	z = if a 7 8

Literal hash anywhere else (`Hash` type):

	inner_hash = {"b": 2}
	h = {"a": 1, **inner_hash, "c": 3}
	# h is now {"a": 1, "b": 2, "c": 3}

## Code syntax - `[...]`

Literal array (`Arr` type):

	inner_arr = [2,3]
	arr = [1, *inner_arr, 4]
	# arr is now [1, 2, 3, 4]

## Command and code syntax - assignment

	var_name=expression

## Command and code syntax - `F`

	F myfunc(required_arg, optional_arg=default_value, *rest_args, **rest_kw_args) {
		...
	}

## Code syntax - try ... catch ...

	try {
		code
	}
	catch(e:E1) { result1 }
	catch(e:E2) { result2 }
	...

Try to execute the *code*. If no excepion occurs, return the value of *code*. If an error of type `E1` occurs, execute `result1`. If an error of type `E2` occurs, execute `result2` and so on.

	try {
		code
	}

If there are no `catch` clauses following the `try`, if any exception occurs in the *code*, the result of the `try` expression is `null`. Example:

	v = try myhash['maybe-such-key-exists']['and-then-some']

## Code syntax - for(i;n) syntactic sugar

	for(i;n) { code }

is same as

	for(i=0; i<n; i=i+1) { code }

## Code syntax - collector syntactic sugar

	collector
		[1,2,3,4,5].each(F(elt) {
			collect(elt)
			if elt > 2 collect(elt * 2)
		})

The expression evaluates to the array **[1,2,3,6,4,8,5,10]**. See stdlib.ngs for more `collector` usage examples.

* `collector ... collect(x) ...` is equivalent to `collector([], code)`. The expression after `collector` is wrapped as `F(collect) { code }`
* `collector/expr ... collect(x) ...` is equivalent to `collector(expr, code)`. The expression after `collector` is wrapped as `F(collect) { code }`

## Code syntax - throws syntactic sugar

	expr1 throws expr2
	# fd <= 0 throws Exception("fetch(): failed to open file ${fname}")

is

	if expr1 throw expr2

## Code syntax - returns syntactic sugar

	expr1 returns expr2
	# a.len() != b.len() returns false

is

	if expr1 return expr2

## Code syntax - continues syntactic sugar

	expr continues
	# for(i;5) { i == 3 continues; echo(i) }
	# Outputs one per line: 0, 1, 2, 4

is

	if expr continue


## Code syntax - breaks syntactic sugar

	expr breaks
	# for(i;5) { i == 3 breaks; echo(i) }
	# Outputs one per line: 0, 1, 2

is

	if expr break
