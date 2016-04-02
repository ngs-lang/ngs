% NGS(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngs - Next Generation Shell.

# SYNOPSIS

**ngs** *script_name* \
**ngs** [**-e**|**-E**] *expression*

# OPTIONS

Given *script_name* runs the script.

Using *expression* is equivalent to running a script that consists of `{` *expression* `}`. **-e** loads stdlib.ngs before evaluating the expression, **-E** skips the loading of stdlib.ngs.

# DESCRIPTION

**ngs** is a Next Generation Shell. It has two main parts: the language and the interactive shell.

This project contains the language part which is under development. The interactive part will be dealt with later. This manual covers both implemented and unimplemented parts.

## MOTIVATION

NGS tries to fill the void between classical shells such as **bash** and general-purpose programming languages such as **Ruby**, **Python**, **Perl**, **Go**. The shells are domain-specific languages but the domain has changed so classical shells are not optimal for today's tasks. General-purpose languages on the other hand are not domain-specific so they are not good as shells and too verbose for system tasks scripting, not a good fit.

# EXIT CODE

The exit code is whatever the running code (*script_name* or *expression*) returns using the following conversions:

* `Int` (integer) - used directly as exit code (modulo 256). For example, `ngs -E 10` returns exit code 10.
* `Bool` (boolean) - `true` results 0, `false` results 1.
* `T` (a user-defined type) - The exit code can be customized by `to_exit_code(x:T)` method.
* Anything else, including user-defined types without `to_exit_code(x:T)` method will result 0.

In case of an uncaught exception, the exit code is 1.

# ENVIRONMENT

* `DEBUG` - when non-empty string, switches on `debug` method output (default is off). It's recommended to use `debug("my debug info")` in your scripts for easily be turning on/off.
* `NGS_BOOTSTRAP` points to the bootstrap NGS file. On NGS startup this file is always run. Defaults to first of:
	* `$HOME/.bootstrap.ngs`
	* `/etc/ngs/bootstrap.ngs`
	* `/var/lib/ngs/bootstrap.ngs`
	* `/usr/share/ngs/bootstrap.ngs`
* `NGS_BOOTSTRAP_DEBUG` - if defined, show **bootstrap.ngs** debugging information.
* `NGS_DIR` (defaults to `/usr/share/ngs`) - location of **stdlib.ngs** file and the **autoload** directory. Files are automatically loaded from this directory when an undefined global variable is used.


# FILES

## bootstrap.ngs

Typically located in `NGS_DIR`. Responsible for bootstrapping NGS.

* Loads **stdlib.ngs** if needed
* Handles **-e** and **-E** switches 
* Runs the script specified in the command line.

## stdlib.ngs

Located in `NGS_DIR`. Standard library. Defines many methods and autoloading behaviour.


# LANGUAGE GOTCHAS - READ FIRST

## Keyword arguments gotchas

	# Keyword arguments are silently ignored if corresponding positional argument is passed.
	kwargs = {"a": 10}
	F f(a, **kw) a; f(1, **kwargs) == 1

	# Keyword arguments for existing named attributes cause parameters not to match
	kwargs = {"a": 10}
	F f(a) a; f(1, **kwargs) == 1

	# **kwargs silently override (consistent with literal hash) previous named arguments
	kwargs = {"a": 10}
	F f(a=1) a; f(a=10, **kwargs) == 1

	# For speed and implementation simplicity reasons, the **kw parameter has all keys,
	# even if some of them matched and used for parameters. 
	# This is somewhat likely to change in the future.
	kwargs = {"a": 10}
	F f(a=1, **kw) [a, kw]; f(**kwargs) == [10, {"a": 10}]

# LANGUAGE PRINCIPLES OVERVIEW

* Do the most practical thing
	* `read('myfile.json')` will parse the JSON and return the data structure.
	* The ```` ``my_command`` ```` will parse the command output (JSON for example) and return the data structure.
	* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

* Simple methods naming for less guess work. `1+2` adds the numbers (method name `+`), `arr1+arr2` adds (concatenates) arrays and `hash1+hash2` produces merged hash.

* Extensibility
	* `read('your_file.super-format')` can be extended to parse your format.
	* `fetch`, which reads from a file, can be extended to support HTTP or S3.
	* Define any operator on your custom types or for existing types.

* Simplicity
	* No classes. Only types, methods and multiple dispatch (picking the right method implementation by matching types of parameters and arguments)
	* Simple type system.

# MAIN LANGUAGE CONCEPTS

## Types

In NGS, each value is of some type. `1` for example is an `Int`, `"xyz"` is a `Str` and `[1,2,3]` is an `Arr`.

Define `Counter` type and a few methods for it (code syntax):

	type Counter

	F init(c:Counter) c.counter = 0

	F incr(c:Counter) {
		c.counter = c.counter + 1
		c
	}

	F get(c:Counter) c.counter

Using the `Counter` type:

	c = Counter()
	c.incr()
	echo(c.get())

`c.incr()` and `incr(c)` are syntactically equivalent.

## Methods, method implementations and calling

A method in NGS is an `Arr` of functions. Each function is called **method implementation**.

	F bigger(x:Int) x+1
	F bigger(s:Str) "+${s}+"

`bigger` is now an `Arr` with two elements, the functions defined by `F`. When calling an `Arr`, NGS scans the array backwards and invokes the **method implementation** that matches the given arguments (this matching is called multiple dispatch). Example:

	echo(bigger("x"))
	# Output: +x+

	echo(bigger(1))
	# Output: 2


No matching **method implementation** causes `ImplNotFound` exception.

	echo(bigger(true))
	# Causes ImplNotFound exception

## Customizaion using methods

Most language syntax constructs are actually a method calls. For example `1 + 2` is `+(1,2)` . The method named `+` is called with 1 and 2 as arguments. This means you can customize what a `+` does when applied to your types. Since types are "open", you can also define what a `+` means for existing types too.

Backwards array scanning allows later code (your code as opposed to previously loaded library for example) to override the behaviour. This override can be narrowed to specific cases using the `guard` clause. Continuing the example above:

	F bigger(x:Int) {
		guard x >= 100
		x + 10
	}
	echo(bigger(1))
	echo(bigger(100))

	# Outputs one per line: 2, 110

When the condition following the `guard` clause is true, the execution continues. When the condition is false, method implementation execution is terminated and the search in the array of method implementations continues as if this implementation did not match the types of the arguments. I recommend not to cause any side effects inside the method implementation in statements above `guard`, if you have any.

# LANGUAGE SYNTAX

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


# LANGUAGE DATA TYPES

## Basic types

Basic types are implemented in C. User-defined types *can not* inherit from a basic type.

* Null
* Bool
* Int
* Str
* Arr
* Fun
* Any
	* BasicTypeInstance
	* NormalTypeInstance
* Seq
* Type
	* BasicType
	* NormalType
* Hash
* CLib
* CSym


## Normal types

User-defined types *can* inherit from a normal type.

* Exception
	* Error
		* LookupFail
			* KeyNotFound
			* IndexNotFound
			* AttrNotFound
			* GlobalNotFound
		* InvalidArgument
		* CompileFail
		* CallFail
			* DontKnowHowToCall
			* ImplNotFound
* Backtrace
* Command
* Range
	* InclusiveRange
	* ExclusiveRange

... and all types defined with `type`.

## Defining your types

TODO, WIP

	type T1
	type T2
	T2.inherit(T1)
	t2 = T2()
	echo(t2 is T2)
	echo(t2 is T1)
	# Outputs one per line: true, true

# LANGUAGE HOOKS

## `impl_not_found_hook`
... is called when a method was called but no **method implementation** matched the arguments. Use `F impl_not_found_hook(callable:Fun, *args) ...` to add your behaviours.

## `global_not_found_hook`
... is called on attempt to read from an undefined global variable. Sample usage from **stdlib.ngs**

	F global_not_found_hook(name:Str) {
		require("${NGS_DIR}/autoload/${name}.ngs")
	}

# THANKS

Thanks to Guy Egozy, Avishai Ish-Shalom and other friends for ideas and feedback.
