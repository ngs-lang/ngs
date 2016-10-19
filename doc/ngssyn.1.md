% NGSSYN(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngssyn - Next Generation Shell language syntax

# DESCRIPTION

NGS has two syntaxes.

The **command syntax** covers the tasks of running programs and i/o redirection, mainly for good interactive (and simple scripting) experience. It resembles closesly the syntax of `bash`. Example: `ls a.txt`

The **code syntax** is for everything else, giving access to a complete, "normal" programming language. Example: `1 + 2 * 3`.

See **command syntax** and **code syntax** descriptions below.

## Command syntax

	ls
	ls $my_file
	ls $*my_files

This is the syntax at the top level of your file or when you start an interactive shell. Additionally, when in **code syntax** you can embed **command syntax** within `$(...)` and several other constructs.

## Code syntax

	{
		type T
		# unrelated
		for(i;10) dump(i)
	}


This is the syntax inside `{...}`. Additionally, when in **command syntax** you can switch to **code syntax** using keywords such as `if`, for example in `if e1 e2` both `e1` and `e2` are expcted to be in **code syntax**.

## Commands separators

In both syntaxes, commands are separated by a new line or by `;` (semicolon).

## Comments

	# comment at the beginning of line
	some code # comments after code are not implemented yet

## Truth / Falsehood

In `if`, `while`, etc. conditions if given expression that is not a `Bool` (i.e. not `true` or `false`), it is converted to `Bool` by applying the `Bool()` method to it. Built in conversion is as follows:

False: null, 0, empty string, empty array, empty hash.
True: everything else.
Not implemented yet: truth of real numbers

Defining truth for your types:

	{
		type T
		F init(t:T) t.wheels_count = 0
		F Bool(t:T) t.wheels_count >= 4
	}



## Command syntax - `{...}`

	{ my_code }
	ls ${ name = "${prefix}${main}${suffix}"; name }

Embed / switch to **code syntax**.

## Code syntax - `{...}`

`{ ... }` groups expressions. This is called a **code block**. Usually used in constructs such as `if`, `while`, etc.

	if a == b {
		x = x + 1
		y = y + x
	}
	# Single expression does not need {...} where a code block is expected.
	z = if a 7 8

`{ ... }` syntax is used for literal hash (see `Hash` type):

	inner_hash = {"b": 2}
	h = {"a": 1, **inner_hash, "c": 3}
	# h is now {"a": 1, "b": 2, "c": 3}

`{ ... }` creates anonymous function with three optional parameters `A`, `B` and `C` all defaulting to `null`.

	[1, 2, 3].map({ A * 2 + 1})
	# 3, 5, 7

	# Same as
	[1, 2, 3].map(F(A=null, B=null, C=null) { A * 2 + 1 })
	# 3, 5, 7

## Code syntax - `[...]`

Literal array (`Arr` type):

	inner_arr = [2,3]
	arr = [1, *inner_arr, 4]
	# arr is now [1, 2, 3, 4]

## Command and code syntax - assignment

	var_name=expression
	var_name = expression

## Command and code syntax - `F`

	F myfunc(required_arg, optional_arg=default_value, *rest_args, **rest_kw_args) {
		...

		if e1 { return e2 }

		# syntactic sugar, same as above
		e1 returns e2

		...
	}

`*rest_args` gets any excessive positional arguments (or `[]`, the empty array if there are none)
`*rest_kw_args` gets all passed keyword arguments (or `{}`, the empty hash if there are none)

## Code syntax - `... X ...` (or `Y` or `Z`)

	[1, 2, 3].map(X*3)
	# 3, 6, 9

	# Same as
	[1, 2, 3].map(F(x=null, y=null, z=null) x * 3)
	# 3, 6, 9

	F mymath(a,b,c) a*b+c
	[1, 2, 3].map(mymath(X, 2, 3))
	# 5, 7, 9

Any mention of `X`, `Y` or `Z` wraps the closest function call (to which the `X`, `Y` or `Z` is an argument) with anonymous function with three optional parameters `X`, `Y` and `Z` all defaulting to `null`. Since operators are essentailly functions with two arguments so the example above works.

## Command and code syntax - function call

	myfunc(required_arg, optional_arg=myval, *rest_args, **rest_kw_args)

`*rest_args` - pass additional positional arguments
`*rest_kw_args` - pass additional keyword arguments

## Code syntax - try/throw/catch

	try {
		# Try to execude this code - start
		...

		if e1 { throw e2 }

		# syntactic sugar, same as above
		e1 throws e2
		...
		# Try to execude this code - end
	}
	catch(e:E1) { result1 }
	catch(e:E2) { result2 }
	...

Try to execute the code. If no exception occurs, return the value of the code. If an error of type `E1` occurs, execute `result1`. If an error of type `E2` occurs, execute `result2` and so on.

	try {
		code
	}

If there are no `catch` clauses following the `try`, if any exception occurs in the *code*, the result of the `try` expression is `null`. Example:

	v = try myhash['maybe-such-key-exists']['and-then-some']

## Command and code syntax - loops

	for(i=0; i<n; i=i+1) { code }

	# This is syntax sugar and works exactly as above
	for(i;n) { code }

	while cond_expr {
		# following loop controls work in for too

		if e1 { continue }

		# syntactic sugar, same as above
		e1 continues

		if e2 { break }
		# syntactic sugar, same as above
		e2 breaks

	}

## Command and code syntax - switch, match, cond

	switch value_expr {
		expr1 code1
		expr2 code2
		...
	}

	# Example:
	r = switch 1 {
		1  2
		10 20
	}
	# r is now 2

Evaluates code1 if `value_expr == expr1`, code2 if `value_expr==expr2` and so on. `value_expr` is computed only once. 

	match value_expr {
		expr1 code1
		expr2 code2
		...
	}

Similar to `switch` but the code is evaluated when `match(value_expr, exprN)` is true.

	cond {
		expr1 code1
		expr2 code2
		...

	}

	# Example:
	r = cond {
		a == b  1
		b == c  2
	}


Similar to `switch` but the code is evaluated when `exprN` is true.

`switch`, `match` and `cond` do *not* have C-style fall-through semantics. They behave like there is a "break" after each `codeN`.

`eswitch`, `ematch` and `econd` work the same but if none of the clauses is matched (and no code is evaluated) the `SwitchFail` exception is thrown.

## Code syntax - collector syntactic sugar

	# Example 1
	collector
		[1,2,3,4,5].each(F(elt) {
			collect(elt)
			if elt > 2 collect(elt * 2)
		})

The expression evaluates to the array **[1,2,3,6,4,8,5,10]**.

	# Example 2, from stdlib
	F flatten(arr:Arr)
		collector
			arr.each(F(subarr) {
				subarr.each(collect)
			})

See **stdlib.ngs** for more `collector` usage examples.

* `collector ... collect(x) ...` is equivalent to `collector([], F(collect) { ... collect(x) ... })`.
* `collector/expr ... collect(x) ...` is equivalent to `collector(expr, F(collect) { ... collect(x) ... })`.

Sample definition for `Arr` (array) collector from **stdlib.ngs**:

	F collector(a:Arr, code:Fun) {
		code(F(elt) a.push(elt))
		a
	}
