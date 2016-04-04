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

This is the syntax at top level of the file or when you start an interactive shell. When in **code syntax** you can embed **command syntax** within `$(...)` and several other constructs.

## Code syntax

	{
		type T
		# unrelated
		for(i;10) dump(i)
	}


This is the syntax inside `{...}`.

## Commands separators

In both syntaxes, commands are separated by a new line or by `;` (semicolon).

## Comments

	# comment at the beginning of line
	some code # comments after code are not implemented yet

## Truth / Falsehood

In `if`, `while`, etc. conditions if given expression is not `Bool` (i.e. not `true` or `false`). It is converted to `Bool` by applying the `Bool()` method to it. Built in conversion is as follows:

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
	var_name = expression

## Command and code syntax - `F`

	F myfunc(required_arg, optional_arg=default_value, *rest_args, **rest_kw_args) {
		...

		if e1 { return e2 }

		# syntactic sugar, same as above
		e1 returns e2

		...
	}

`*rest_args` gets any excessive positional arguments (or `[]` if there are none)
`*rest_kw_args` gets all passed keyword arguments (or `{}` if there are none)

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

	# This is syntax sugar and is exactly as above
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
