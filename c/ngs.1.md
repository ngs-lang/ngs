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

# MOTIVATION AND BACKGROUND

NGS tries to fill the void between outdated shells such as **bash** and generic programming languages such as **Ruby**, **Python**, **Perl**, **Go**. The shells are domain-specific languages but the domain has changed so they are not a good fit for the job. Generic languages on the other hand are not domain-specific so they are not good as shells and too verbose for system tasks scripting, also not a good fit.

## Motivation - fix current shell syntax

Since conventional shells were first designed just to run commands and only later extended to be a programming languages while keeping syntax compatibility, the syntax is inconvenient.
Examples:

* bash - `while some command;do ... done`. NGS - `while $(some command) ...`
* bash - while running `my_command $var`, the number of arguments passed to `my_command` is not known from syntax due to expansion rules.
* bash - `my_command "$var"`, ngs - `my_command $var`.
* bash - `my_command $var`, ngs - `my_command $*var` (zero or more arguments, equals to the number of elements in the `$var` array).

## Motivation - fix current language design

* For example, one can not simply set a variable defined outside while with code inside the while (because of sub-shell): `a=1; cat /dev/null | while true;do a=2;break; done; echo $a` - prints **1**.

## Motivation - Cloud support

Bash was meant to manage one machine.

* There is no built-in support for managing multiple servers.
* There is no convenient way to work with cloud services APIs. This is mostly due to unsupported nested data structures.

## Inspired by

The language was inspired by Python, JavaScript, Ruby. Also lifted good parts from bash.

# EXIT CODE

The exit code is whatever the running code (*script_name* or *expression*) returns using the following conversions:

* `Int` (integer) - used directly as exit code (modulo 256). For example, `ngs -E 10` returns exit code 10.
* `Bool` (boolean) - `true` results 0, `false` results 1.
* `T` (a user-defined type) - The exit code can be customized by `to_exit_code(x:T)` method.
* Anything else, including user-defined types without `to_exit_code(x:T)` method will result 0.

# ENVIRONMENT

TODO

* `DEBUG`
* `HOME`
* `NGS_BOOTSTRAP`
* `NGS_BOOTSTRAP_DEBUG`


# FILES


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
	* `read('myfile.json')` will parse the JSON and return the data structure. If you read a JSON file, most probably you want to look at the data, not handle it as a binary or a string.
	* If you run a command with ```` ``my_command`` ````, (which is a syntax for run and parse output) and it returns JSON, the expression evaluates to the parsed data structure.
	* If you `map` or `each` a hash, the callback function will be called with two arguments: key and value.
	* `my_array.my_prop` does `array.map(F(elem) elem.my_prop)` because that's probably what you would like to get when using `.my_prop` on array of hashes, get that property of each element of the array. Kind of `jq .[].my_prop`.

* Have types x methods matrix.
	* Less guess work about the names of methods (functions). Same method should work on maximum number of types. Example: if you have `1+2` which adds the numbers, you will have `arr1+arr2` which will add (concatenate) the arrays, rather than `arr1.concat(arr2)` and `hash1+hash2` will produce merged hash rather than `hash1.merge(hash2)`.
	* If you have a type and a method that can do remotely expected thing on that type, it should work. Example of "remotely" expected is: `number.map(mapper)` runs the `mapper` function for each number in range of 0 to the *number* - 1.

* Extensibility.
	* Have custom data format? There is no reason that `read('your_file.super-format')` will not return the parsed data structure.
	* Have your custom software that returns non-zero exit code but it's OK? Extend NGS so that running that command will not produce a fatal error. NGS always runs in a mode that is equivalent to `bash -e`, saving many tears.
	* Want `read('http://example.com/my-resource')` to work? Extend NGS to be able to fetch HTTP or S3.
	* Define any operator on your custom types.

* Consistent syntax.
	* No more `do ... done` and `if ... fi` in the same language. Blocks are `{ ... }`.
	* If you can call a function, `my_func(*args)` and have "splat" effect (`args` array becomes several arguments when calling the function), there is no reason why it would not work when you create an array: `[1, 2, *args, 3, 4]`, same for `**kwargs` and `{my_hash_default: 7, **user_supplied_options, my_override: 8}`.

* Short syntax for common cases. `F (a,b) a+b` is equivalent to JavaScript's `function(a,b) { return a+b; }` (last expression's value is returned, like in Ruby).

* Syntactic sugar for common cases.
	* `for(i;n) ...` is equivalent to `for(i=0;i<n;i=i+1) ...`
	* `collector ... collect(x) ...` - for collecting items

* Simplicity
	* No classes. Only types, methods and multi-dispatch.
	* Simple type system.

# MAIN LANGUAGE CONCEPTS

## Types

In NGS, each value is of some type. Example of such types are: `Int`, `Str`, `Arr`. `1` for examples is of type `Int`eger, `"xyz"` is an `Str`ing and `[1,2,3]` is an `Arr`ay.

Types in NGS plus methods that work with these types are rough equivalent of classes and their methods in languages such as Python and Ruby. Let's start with an example of roughly equivalent code:

Python:

	class Counter:

		def __init__(self):
			self.counter = 0

		def incr(self):
			self.counter += 1
			return self.counter

		def get(self):
			return self.counter

	c = Counter()
	c.incr()
	print(c.get())

NGS (code syntax):

	type Counter

	F init(c:Counter) c.counter = 0

	F incr(c:Counter) {
		c.counter = c.counter + 1
		c
	}

	F get(c:Counter) c.counter

	c = Counter()
	c.incr()
	echo(c.get())

`c.incr()` and `incr(c)` are syntactically equivalent.

Note that in NGS, the class is "open" so you can add methods that work with the `Counter` type any time after the definition of the type (`type Counter`).


## Methods, method implementations and calling

A method in NGS is a collection (techincally `Arr`ay) of functions. Each such function implements the intended operation for some type. Example (NGS code syntax):

	F bigger(x:Int) x+1
	F bigger(s:Str) "+${s}+"

Here `bigger` is a method. Each `F` definition defines a **method implementation**. When calling an `Arr`ay, NGS scans from the last element to first element and invokes the **method implementation** that matches the given arguments. Example:

	echo(bigger("x"))
	# Output: +x+

	echo(bigger(1))
	# Output: 2


When the array search is finished and no matching **method implementation** is found, `ImplNotFound` exception occurs.

	echo(bigger(true))
	# Causes ImplNotFound exception

## Customizaion using methods

Most language syntax constructs are actually a method calls. For example `1 + 2` is `+(1,2)` . The method named `+` is called with 1 and 2 as arguments. This means you can customize what a `+` does when applied to your types. Since types are "open", you can also define what a `+` means for existing types too.

Note that the array is specifically scanned from last element to first to allow later code (your code as opposed to previously loaded library for example) to override the behaviour. This override can be narrowed to specific cases using the `guard` clause. Continuing the example above:

	F bigger(x:Int) {
		guard x >= 100
		x + 10
	}
	echo(bigger(1))
	echo(bigger(100))

	# Outputs one per line: 2, 110

When the condition following the `guard` clause is true, the execution continues. When the condition is false, method implementation execution is terminated and the search in the array of method implementations continues as if this implementation did not match the types of the arguments. I recommend not to cause any side effects inside the method implementation in statements above `guard`, if you have any.

Such scanning behaviour allows for example to extend native `fetch` method, which can read a file to be able to fetch a URL.

Customizaion using methods is also used for the command syntax. When a command is mentioned in your program, NGS just parses it into a `Command` type instance. After such parsing, NGS passes it to one of appropriate methods, depending on the context where the command was mentioned: ```` `` ````, `````` ```` `````` (other methods will probably be added later). Since these methods can be customized, one can control how external programs are run (or replaced by built-ins for example). Note that the implementations of the ```` `` ```` and similar methods in stdlib.ngs are calling other methods. Each such call is a potential extension point, opportunity for more customization.


# LANGUAGE SYNTAX

A notable difference between say JavaScript, Ruby, Perl, Python and NGS is that there are two syntaxes in the language. The **command syntax** and the **code syntax**. The **command syntax** covers the tasks of running programs and i/o redirection. The **code syntax** is a full blown language. The decision to have two syntaxes was made because I did not see another way to provide both good interactive experience and a complete, normal programming language. What I mean is that typing `system('ls')` or even `` `ls` `` is not a good interactive experience. On the other hand having syntax for full blown language based on interactive syntax (extending the **command syntax** till it becomes a real language) is not a good thing either, it will look bad. See control structures in bash. It's horrible. `if ... ;then ...; fi`.

## Command syntax

This is the syntax at top level of the file or when you start an interactive shell. When in **code syntax** you can embed **command syntax** within `$(...)`, which returns the `Command` object (or a list of these, if `$(...)` contains pipes).

**Command syntax examples**

* `ls`
* `ls $my_file`
* `ls $*my_files`

## Code syntax

This is the syntax inside `{...}`.

**Code syntax examples**

* `{ for(i;10) dump(i) }`
* `{ for(i;10) { j=i*2; dump(j); } }`

## Code syntax - try ... catch ...

	try {
		code
	}
	catch(e:E1) { result1 }
	catch(e:E2) { result2 }
	...

Try to execute the *code*. If no excepion occurs, return the value of *code*. If an error of type `E1` occurs, execute `result1`. If an error of type `E2` occurs, execute `result2` and so on. `catch` clauses are converted to a method which is then called. When the method is constructed, the implementations are reversed in order so the scan is from first to last `catch` clause. 

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

## Code syntax - throws, returns, continues, breaks syntactic sugar

### Throws

	expr1 throws expr2
	# fd <= 0 throws Exception("fetch(): failed to open file ${fname}")

is

	if expr1 throw expr2

### Returns

	expr1 returns expr2
	# a.len() != b.len() returns false

is

	if expr1 return expr2

### Continues

	expr continues
	# for(i;5) { i == 3 continues; echo(i) }
	# Outputs one per line: 0, 1, 2, 4

is

	if expr continue


### Breaks

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

## Defining your types

TODO, WIP

	type T1
	type T2
	T2.inherit(T1)
	t2 = T2()
	echo(t2 is t2)
	echo(t2 is t1)

# THANKS

Thanks to Guy Egozy, Avishai Ish-Shalom and other friends for ideas and feedback.
