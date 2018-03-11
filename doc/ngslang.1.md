% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngslang - Next Generation Shell language tutorial.

# WHAT IS NGS?

NGS is an alternative shell. At it's core is a domain-specific language that was specifically designed to be a shell language.

NGS is under development. The language part is already good enough to write some useful scripts. CLI does not exist yet. It will be written using the same language.

# RUNNING NGS

**ngs** *script_name.ngs*

You can put the following line as first line of your script:

	#!/usr/bin/env ngs

If you do, you can run the script as `./script_name.ngs` or `/full/path/to/script_name.ngs` (you must make your script executable, `chmod 755 script_name.ngs`.

See more about running NGS in [ngs(1)](ngs.1.html).

# WHY NGS?

If your attitude towards system tools is like mine, NGS would resonate better with how you think than bash, Python, Ruby, Perl, Go or any other language for systems administration tasks.

## NGS was built for systems administration tasks

NGS is optimized by design to perform easily typical systems administration tasks. The following tasks are common so NGS either has a syntax or features to make these tasks easy:

* Running external commands
* Manipulating data
* Simple remote execution (not implemented yet)

Example of running external command + data manipulation (detailed explanation later):

	ngs -pi '``aws ec2 describe-instances``.InstanceId'

	# Output:
	Array of size 37
	[0] = i-0a0xxxxxxxxxxxxxx
	[1] = i-04dxxxxxxxxxxxxxx
	...

You might want to process the output above with an external tool but it's in a human-readable and not-machine-parseble format. No problem, use the **-pj** (print JSON) switch:

	ngs -pj '``aws ec2 describe-instances``.InstanceId'

	# Output:
	[ "i-0a0xxxxxxxxxxxxxx", "i-04dxxxxxxxxxxxxxx", ... ]

We all know that life is not that simple so here is a bit more complex situation where the field might or might not appear in the data (also outputting space delimited items):

	ngs -p '``aws ec2 describe-instances``.map({A.PublicIpAddress tor "-"}).join(" ")'

	# Output:
	52.58.XXX.XX 52.59.XX.XX 52.29.XXX.XX 52.57.XXX.XXX - 52.57.XX.XXX ...



## Your current situation with languages sucks

Currently, as a systems engineer you are probably using bash combined with of one or more high-level languages.

**bash**

You are using bash because it's domain-specific and allows you to get some tasks done easily. On the other hand, bash can not manipulate nested data structures in a sane way. So you need an external tool:

	aws ec2 describe-instances | jq -r '.Reservations[].Instances[].PublicIpAddress'

	# Output:
	52.58.XXX.XX
	52.59.XX.XX
	52.29.XXX.XX
	52.57.XXX.XXX
	null
	...

`jq` is fine till you need to work with tags (thanks AWS for list instead of hash!) or do something more complex. It starts looking bad really fast. It probably can be done with `jq` but why get yourself into this instead of using a normal programming language. You can do chess in `sed` too (actually done) but it doesn't mean you should unless it's for fun and not to get the job done quickly.

Yes, there is also built-in `jmespath` in `awscli`. It won't be much better than `jq` - OK for simple cases. Why bother then? I've read the specs once and decided it was not worth the trouble if you already know how to use `jq`.

**other languages**

You are probably using Python/Ruby/Perl/Go . You use one of the above languages because bash is not powerful enough / not convenient enough to do the tasks that these languages do. On the other hand something as simple as `echo mystring >myfile` or running an external program is not as convenient to do in these languages. Yes all of the languages above support system tasks to some degree. None of these languages can support system tasks as a language that was built ground-up for system tasks. See the double-backtick examples above... for example.

# Terminology

* type - Built-in or user-defined data type, similar to Python, Ruby and other languages.
* object - Instance of a type, similar to Python, Ruby and other languages. The phrase "MyType object" refers to an Instance of "MyType".
* field - A named slot of an object, similar to field in Python, Java, etc.
* attributes - Slot for auxiliary data on most types of objects (the ones that are references, i.e. not Int,Bool,Null), typically a `Hash` or `null`.
* method - Built-in or user-defined function. User defined methods can be closures.
* multimethod - A MultiMethod object containing ordered list of methods. When called, the appropriate method is selected from the list to perform the computation.

# Language principles overview

This section is about principles behind NGS language design.

## Systems engineers' language

NGS is a domain-specific language. It is aimed to solve common system tasks in a convenient manner.

## Do the most practical thing

* `fetch('myfile.json')` will parse the JSON and return the data structure. (Use `read()` to get raw contents).
* The ```` ``my_command`` ```` will parse the command output (JSON for example) and return the data structure. Note that ```` ``aws ...`` ```` will be parsed even further (not just JSON) to return more usable data structures.
* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

## Uniformity

NGS tries to be uniform wherever possible to minimize surprises.

## Power

As rule, trade-offs between power and not allowing to shoot yourself in the foot are resolved in favor of the power solution. The language is aimed at experienced engineers which use their own judgement. The language should be powerful enough to shoot all feet in the building at once.

## Simple methods naming for less guess work

For example, the multimethod `+`:

* `1 + 2` adds the numbers
* `arr1 + arr2` adds (concatenates) arrays
* `hash1 + hash2` produces merged hash.

## Extensibility

* `fetch('your_file.super-format')` can be extended to parse your format.
* `read`, which reads from a file, can be extended to support HTTP or S3.
* Define any operator for existing or your custom types.

## Simplicity

Very small number of core concepts in the language:

* Types with a simple type system, geared only toward multiple dispatch. No classes.
* Multimethods, which allow using same method name for operations on different types, as in the `+` example above.

## Familiarity

Many concepts and syntax constructs come from other languages.

# Two syntaxes overview

NGS has two syntaxes: **command syntax** and **code syntax**.

## Command syntax

This is the resembles-bash syntax geared towards running external programs and i/o redirection.
Command syntax is the syntax at the top level of every NGS script. The most simple NGS scripts might look very similar to bash scripts.
Commands are separated by either newlines or by semicolon (`;`).

Example:

	cat a.txt; touch myfile
	echo mystr >myfile

In addition to running commands and performing redirections, there are several expressions that are also supported in the **commands syntax**. Note `code` below means **code syntax** expressions.

* `{ code }` - see **Switching between syntaxes** below
* assignment: `myvar = code` (`myvar = 1 + 2`)
* in-place assignment: `myvar += code` (`myvar += 10`)
* function definition: `F myfunc(params...) code` (`F myfunc(n:Int) echo(n*10)`)
* function call: `myfunc(arguments...)` (`myfunc(7)`)
* `if condition_code [then] yes_code [else] no_code`
* `while condition_code body_code`
* `for(...) body_code`
* comment: `#` and till end of line

## Code syntax

Code syntax resembles other high-level languages such as Python or Ruby.

Example:

	1 + 2 * 3; %[abc def ghi].without('ghi').each(echo)

Expressions are separated by either newlines or by semicolon (`;`).

**code syntax** is the syntax of **-e**, **-E**, **-p**, **-pi** and **-pj** switches to `ngs` interpreter. Example:

	ngs -p '(1...10).filter(F(num) num % 2 == 0)'

	# Output:
	[2,4,6,8,10]

If the above example looks too verbose, here is the shorter and uglier (and not generally recommended) alternative:

	ngs -p '(1...10)?{A%2==0}'

## Switching between syntaxes

In **command syntax** it is possible to switch to **code syntax** in one of the following ways:

**Simple switching** (for a lack of better name)

	ls
	{ code syntax here }

Use the kind of switching to above when an expression that you are writing is not supported in **command syntax**.

It's not uncommon that the whole file is inside `{...}`:

	{
		%[abc def ghi].without('ghi').each(echo)
	}

**Singe argument substitution switch**

	ls ${ code that computes the file name and returns a string,
	spaces don't matter, expaned into single argument of ls }

**Multiple arguments substitution switch**

	ls $*{ code that computes the files names and returns array of
	strings. Spaces don't matter, each element is expaned into single
	argument of ls }

In **code syntax** it is possible to switch to **command syntax** in one of the following ways:

**Capture switch**

	out = `commands syntax`
	myvar = "mystring\n" + `my other command`

**Capture and parse switch**

	parsed_data_structure = ``commands syntax``
	n = ``curl https://example.com/myservice/stats``.number_of_items_in_storage

**Command switch**

	my_process = $( commands syntax )

# Language syntax and functionality

## Naming convention

* `var_name`
* `method_name` (multimethod name)
* `TypeName`
* `TransformationName` - example: `Strs` (converts to array of strings), `Argv` (constructs command line arguments array), `ExitCode` (converts anything to integer exit code).

Reasoning behind `TransformationName`:
* Transforms data into something else, like many other constructors
* The output data might get it's own data type some day

## Comment

	# comment till end of line

## Calling a function

	echo('Hello world')
	{
		'Hello world'.echo()
	}

	# Output:
	#   Hello world
	#   Hello world

Note: `f(a, b, c)` is same as `a.f(b, c)`

## Variables

	echo(defined a)
	a = 1 + 2
	echo(defined a)
	echo(a)

	# Output:
	#   false
	#   true
	#   3

Note: Reading undefined variables will cause an exception.

## Basic constants: true, false, null

	if true echo("if true")
	if false echo("if false")
	if null echo("if null")
	# Output:
	#   if true

	echo(1 == 1)
	# Output:
	#   true

	echo(1 == 2)
	# Output:
	#   false

	echo("abcd".pos("c"))
	echo("abcd".pos("x"))
	# Output:
	#   2
	#   null

## Booleans

	echo(true and false)
	# Output:
	#   false

	echo(true or false)
	# Output:
	#   true

When a boolean value is needed, such as in `if EXPR {...}`, the `EXPR` is converted to boolean by calling `Bool(EXPR)`.
NGS defines `Bool` multimethod, with methods for many types. These methods cause the following values to be converted to false:

* 0
* null
* false
* empty array
* empty hash
* empty string
* an EmptyBox object, for example `[1,2,3].Box(10)`
* a Failure object, for example `Result({ 1 / 0 })`
* regular expression that did not match, for example `"abc" ~ /XYZ/`

Unless specified otherwise, all other values are converted to `true`.
To define how your user-defined types behaves as boolean, define `Bool(x:YOUR_TYPE)` method.

## Integers

	echo(1 + 2 * 3)
	# Output:
	#   7

	{
		3.each(echo)
	}
	# Output:
	#   0
	#   1
	#   2

	{
		3.map(X*2).each(echo)
	}
	# Output:
	#   0
	#   2
	#   4

## Strings

Strings - string interpolation

	a = 1
	echo("A is now $a")
	echo('A is now $a')
	# Output:
	#   A is now 1
	#   A is now $a

	echo("Calculation result A: ${10+20}")
	echo("Calculation result B: ${ [1,2,3].map((*), 10).join(',') }")
	# Output:
	#   Calculation result A: 30
	#   Calculation result B: 10,20,30

Strings - some basic methods that operate on strings

	echo("abc" + "abc")
	# Output:
	#   abcabc

	echo("abc:def:ggg".split(":"))
	# Output:
	#   ['abc','def','ggg']

	echo("abc7def8ggg".split(/[0-9]/))
	# Output:
	#   ['abc','def','ggg']

	echo("abc7def8ggg".without(/[0-9]/))
	# Output:
	#   abcdefggg

	if "abc" ~ /^a/ {
		echo("YES")
	} else {
		echo("NO")
	}
	# Output:
	#   YES

	m = "abc=120" ~ /=/
	if m {
		echo("Name=${m.before} Value=${m.after}")
	}
	# See RegExp type
	# Output:
	#   Name=abc Value=120

## Arrays

Arrays - basics

	x = ["first", "second", "third", "fourth"]

	echo(x)
	# Output:
	#   ['first','second','third','fourth']

	echo(x.len())
	# Output:
	#   4

	echo('first' in x)
	# Output:
	#   true

	echo('fifth' in x)
	# Output:
	#   false

	echo(x[1])
	# Output:
	#   second

	echo(x[1..3])
	# Output:
	# ['second','third']

	echo(x == %[first second third fourth])
	# Output:
	#   true

	x = [
		"blah"
		2
		'some text'
	]
	echo(x)
	# Output:
	#   ['blah',2,'some text']

	echo(x[10])
	# ... Exception of type IndexNotFound occured ...

Arrays - some basic methods that operate on arrays

	{
		[1,2].each(echo)
	}
	# Output:
	#   1
	#   2

	{
		[1,2].each(F(item) {
			echo("Item: $item")
		})
	}
	# Output:
	#   Item: 1
	#   Item: 2

	echo([1,2].map(X * 10))
	# See "Anonymous function literal using magical X, Y or Z variables"
	# Output:
	#   [10,20]

	echo(Hash([["a",1],["b",2]]))
	# Converts array consisting of pairs into Hash
	# Output:
	#   {a=1, b=2}

## Hashes

Hashes - basics

	x = {"a": 1, "b": 2}
	echo(x)
	# Output:
	#   {a=1, b=2}

	x = %{akey avalue bkey bvalue}
	echo(x)
	# Output:
	#   {akey=avalue, bkey=bvalue}

	echo(x)
	# Output:
	#   {a=1, b=2}

	echo(x['a'])
	echo(x.a)
	# Output:
	#   1
	#   1

	{
		x.b = 20
	}

	echo(x.len())
	# Output:
	#   2

	echo(x.keys())
	# Output:
	#   ['a','b']

	echo(x.values())
	# Output:
	#   [1,20]

	x = {
		"c": 1
		"d": 2
	}

	echo(x)
	# Output:
	#   {c=1, d=2}

	echo('c' in x)
	# Output:
	#   true

	echo(1 in x)
	# Output:
	#   false

	echo(x.get('d'))
	# Output:
	#   2

	echo(x.get('e'))
	# Output:
	#   null

	echo(x.get('e', 'my_default'))
	# Output:
	#   my_default

	echo(x.e)
	# ... Exception of type KeyNotFound occured ...

Hashes - some basic methods that operate on Hashes

	{
		h = {"a": 1, "b": 2}
		h.each(F(k, v) {
			echo("$k = $v")
		})
	}
	# Output:
	#   a = 1
	#   b = 2

	h = {"a": 1, "b": 2}
	echo(h.map(F(k, v) "key-$k value-$v"))
	# Output:
	#   ['key-a value-1','key-b value-2']

	h = {"a": 1, "b": 2}
	echo(h.mapk("key-${X}"))
	# Output:
	#   {key-a=1, key-b=2}

	h = {"a": 1, "b": 2}
	echo(h.mapv(X*10))
	# Output:
	#   {a=10, b=20}

	h = {"a": 1, "b": 2}
	echo(h.mapkv({ ["key-$A", B*100] }))
	# Output:
	#   {key-a=100, key-b=200}

	h = {"a": 1, "b": 2}
	echo(Arr(h))
	# Converts Hash into array consisting of pairs
	# Output:
	#   [['a',1],['b',2]]

## Common higher-order methods (functions)

	echo([1,2,3].all(Int))
	# Output:
	#   true

	echo([1,2,3,"a","b"].all(Int))
	# Output:
	#   false

	echo([1,2,3,"a","b"].any(Str))
	# Output:
	#   true

	echo([1,2,11,12,3].none(X>100))
	# Output:
	#   true

	echo([1,2,"a","b",3].filter(Int))
	# Output:
	#   [1,2,3]

	echo([1,2,"a","b",3].reject(Int))
	# Output:
	#   ['a','b']

	echo([1,2,11,12,3].reject(X>10))
	# Output:
	#   [1,2,3]

	echo([1,2,11,12,3].count(X>10))
	# Output:
	#   2


## Defining a type

	# Switch to code syntax inside { ... }. "type" currently does not work in command syntax
	{
		# Define type
		type Vehicle

		# Define sub-type
		type Car(Vehicle)
	}
	echo(Vehicle)
	echo(Car)

	# Output:
	#   <Type Vehicle>
	#   <Type Car>

	v = Vehicle()
	c = Car()
	echo("v is Vehicle: ${v is Vehicle}")
	echo("v is Car: ${v is Car}")
	echo("c is Vehicle: ${c is Vehicle}")
	echo("c is Car: ${c is Car}")

	# Output:
	#   v is Vehicle: true
	#   v is Car: false
	#   c is Vehicle: true
	#   c is Car: true

## Methods

Defining a method.

When defining a named method, NGS automatically creates a MultiMethod with the given name (if it does not exist) and appends the new method to the multimethod's list of methods.

	{
		type Vehicle
		type Car(Vehicle)
	}

	# c - parameter name
	# Car - parameter type
	F drive(c:Car) {
		echo("Driving the car")
	}
	# "drive" is now a MultiMethod with one method

	mycar = Car()
	drive(mycar)
	# Output: Driving the car


	# Defining method with single expression as body
	# does not require { ... } around the method body
	F park(c:Car) echo("Parking the car")

	park(mycar)
	# Output: Parking the car

	# There is no method drive() that takes a string as an argument
	drive("well...")
	# ... Exception of type MethodNotFound occured ...

Method optional parameters

	F mysum(a:Int, b:Int=100) a+b

	echo(mysum(5))
	# Output: 105

	echo(mysum(5, 200))
	# Output: 205

Method "rest" parameter

	F print_with_prefix(prefix:Str, *strings) {
		strings.each(F(s) {
			echo("$prefix$s")
		})
		echo("Printed ${strings.len()} lines with prefix")
	}

	print_with_prefix('-> ', 'abc', 'def')
	# Output:
	#   -> abc
	#   -> def
	#   Printed 2 lines with prefix

Method "rest keywords" parameter

	F print_properties(separator:Str, **kw_args) {
		kw_args.each(F(name, value) {
			echo("$name$separator$value")
		})
	}

	print_properties(' => ', a=10, b=20)

	# Output:
	#   a => 10
	#   b => 20

Method guard

	F gg(i:Int) {
		echo("First gg active")
		echo(i*10)
	}
	gg(1)
	gg(5)
	# Output:
	#   First gg active
	#   10
	#   First gg active
	#   50

	F gg(i:Int) {
		echo("Second gg checking guard")
		guard i > 3
		echo("Second gg active")
		echo(i*100)
	}
	gg(1)
	gg(5)
	# Output:
	#   Second gg checking guard
	#   First gg active
	#   10
	#   Second gg checking guard
	#   Second gg active
	#   500

Call super methods (methods higher in the multimethod list of methods)

	F sup(x) x+1

	F sup(x) super(x) * 10

	echo(sup(5))
	# Output: 60

Anonymous function (method) literal

	f = F(item) { echo("Item: $item") }
	echo("F is $f")
	# Output: F is <UserDefinedMethod <anonymous> at 1.ngs:1>

	each([10,20,30], f)
	# Output:
	#   Item: 10
	#   Item: 20
	#   Item: 30

	echo([1,2,3].map(F(x) x*5))
	# Output: [5,10,15]

Anonymous function literal using magical X, Y or Z variables

	echo([1,2,3].map(X*5))
	# Output: [5,10,15]

	echo({"a": "one", "b": "two"}.map("Key:" + X))
	# Output: ['Key:a','Key:b']

	echo({"a": "one", "b": "two"}.map("Val:" + Y))
	# Output: ['Val:one','Val:two']

	echo({"a": 1, "b": 2}.map("Key $X, Value $Y"))
	# Output: ['Key a, Value 1','Key b, Value 2']

	echo([1,2,3,11,12].count(X>10))
	# Output: 2

Anonymous function literal using magical A, B or C variables

	# X*5 + 1 would not work as X*5 itself would be anonymous function
	echo([1,2,3].map({ A*5 + 1 }))
	# Output: [6,11,16]

Method-related flow control

	F flow_ret(x) {
		if x < 0 {
			unrelated_calculation = 1
			return "negative"
		}
		x == 0 returns "zero"
		"positive"
	}
	echo(flow_ret(-1))
	echo(flow_ret( 0))
	echo(flow_ret( 1))
	# Output:
	#   negative
	#   zero
	#   positive

	F find_the_one(haystack:Arr, needle) {
		ret_from_find_the_one = Return()
		echo(ret_from_find_the_one)
		haystack.each(F(elt) {
			elt == needle throws ret_from_find_the_one("Found it!")
		})
		"Not found"
	}
	echo([10,20].find_the_one(20))
	echo([10,20].find_the_one(30))
	# Output:
	#   <Return closure=<UserDefinedMethod find_the_one at 1.ngs:2> depth=7 val=null>
	#   Found it!
	#   <Return closure=<UserDefinedMethod find_the_one at 1.ngs:2> depth=7 val=null>
	#   Not found

## Short circuit binary operators

	a = 1 and 2                    # a = 2
	a = null and 2                 # a = null
	a = 1 or 2                     # a = 1
	a = null or 2                  # a = 2
	a = code_with_exception tor 3  # a = 3, exception discarded

## Ignoring exceptions using "try" without "catch"

	myhash = {"a": 1, "b": 2}

	v = try myhash.a
	echo(v)
	# Output: 1

	v = try myhash.c
	echo(v)
	# Output: null

	v = try { unrelated = 1+2; myhash.c }
	echo(v)
	# Output: null

## Exceptions

	{
		type MyError(Error)

		try {
			# "e1 throws e2" is same as "if e1 { throw(e2) }"
			1 == 2 throws Error("This can't be!")
			throw MyError("As usual, very helpful message")
		} catch(e:MyError) {
			echo("[Exceptions] This error was expected: $e")
		} catch(e:Error) {
			echo("[Exceptions] Unexpected error: $e")
			throw e
		}
		# Output: [Exceptions] This error was expected: ...
	}

## Flow control

If

	if my_var > 10 {
		a += 100
		b = "x"
	} else {
		b = "y"
	}

	result = if my_var then "xyz" else "ww".
	result = if my_var 10 20  # result is now either 10 or 20

In `if`, `while`, and `for`, where `{...}` code block is expected, if the block consists of only a single expression, curly braces are optional. In `if`, the `then` and `else` keywords are optional.

Note that `if` is an expression. `if` without else where the condition is equivalent to `false`, evaluates to `null`.

Loops

	for(i=0; i<5; i+=1) {
		if i == 3 {
			continue
		}
		echo("Regular loop, iteration $i")
	}
	# Output:
	#   Regular loop, iteration 0
	#   Regular loop, iteration 1
	#   Regular loop, iteration 2
	#   Regular loop, iteration 4

	for(i;5) {
		i == 3 continues
		echo("Shorthand loop, iteration $i")
	}
	# Output:
	#   Shorthand loop, iteration 0
	#   Shorthand loop, iteration 1
	#   Shorthand loop, iteration 2
	#   Shorthand loop, iteration 4

	for i in [1,5,10,20,50] {
		echo(i)
	}
	# Output:
	#   1
	#   5
	#   10
	#   20
	#   50
	# See "Iterators"

	i = 0
	while i<10 {
		echo("While loop, iteration $i")
		i += 1
		# Same as "if i == 2 { break }"
		i == 2 breaks
	}
	# Output:
	#   While loop, iteration 0
	#   While loop, iteration 1

Switch and switch-like expressions.

* The first match activates the related code, there is no fall-through.
* The value resulting from executing the code is the result of the switch-like expression.
* If there is no match, `switch`, `match` and `cond` return `null`.
* If there is no match, `eswitch`, `ematch` and `econd` throw `SwitchFail` exception.


	a = 10
	result = switch a {
		10 "ten"
		20 "twenty"
		30 { more_code(); "thirty" }
	}
	echo("Switch result for $a is $result")
	# Output: Switch result for 10 is ten

	a = "my_string"
	result = match a {
		Int    "an integer"
		/^my_/ "special string"
		Str    { more_code(); "a string" }
		Any    "not sure"
	}
	echo("Match result for $a is $result")
	# Output: Match result for my_string is special string

	a = 12
	result = cond {
		a > 10
			"Excellent"
		a > 5 {
			more_code(); "Good enough"
		}
		a > 3
			"so so"
	}
	echo("Cond result for $a is $result")
	# Output: Cond result for 12 is Excellent

	# SwitchFail exception will be thrown
	F will_throw_exception1() {
		a = "bad value"
		result = eswitch a {
			1 "one"
			2 "two"
		}
	}

	# SwitchFail exception will be thrown
	F will_throw_exception2() {
		a = true
		result = ematch a {
			Int  "an integer"
			Str  "a string"
		}
	}

	# SwitchFail exception will be thrown
	F will_throw_exception3() {
		a = 10
		result = econd {
			a > 15 "one"
			a > 20 "two"
		}
	}


## Regular expressions

	myregex = /^begin/
	echo(myregex)
	# Output: <RegExp>

	mymatch = "beginABC" ~ myregex
	echo(mymatch)
	# Output: <MatchY matches=['begin'] named={} positions=[[0,5]] whole=begin before= after=ABC>

	echo(mymatch.matches[0])
	# Output: begin

	echo(mymatch.after)
	# Output: ABC

	all_matches = "1a2bcd3efg" ~~ /([0-9])(.)/
	each(all_matches, F(match) {
		echo("The character after the digit ${match.matches[1]} is ${match.matches[2]}")
	})
	# Output:
	#   The character after the digit 1 is a
	#   The character after the digit 2 is b
	#   The character after the digit 3 is e

## Collector facility

	mylist = collector {
		collect("HEADER")
		[10,20].each(collect)
		collect("FOOTER")
	}
	echo(mylist)
	# Output: ['HEADER',10,20,'FOOTER']

	myhash = collector/{} {
		collect("first", -1)
		{"a": 1, "b": 2, "c":100}.each(F(k, v) {
			if v < 100 {
				collect("($k)", v*10)
			}
		})
		collect("last", -2)
	}
	echo(myhash)
	# Output: {first=-1, (a)=10, (b)=20, last=-2}

	mysumm = collector/0 [1,10,100].each(collect)
	echo(mysumm)
	# Output: 111

## Running external programs

	t = `echo -n text1`
	echo("[ $t ]")
	# Output: [ text1 ]

	seq = `seq 5`.lines()
	echo(seq)
	# Output: ['1','2','3','4','5']

	proc = $(seq 3)
	each(inspect(proc), echo)
	# Output:
	#   CommandsPipeline
	#     command[0]: <Command options={} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[seq,3]>
	#     process[0]: Process
	#     process[0]:   command = <Command options={} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[seq,3]>
	#     process[0]:   pid = 43582
	#     process[0]:   exit_code = 0
	#     process[0]:   exit_signal = 0
	#     process[0]:   output on fd 1, stdout (3 lines):
	#     process[0]:     1
	#     process[0]:     2
	#     process[0]:     3
	#     process[0]:   output on fd 2, stderr (0 lines):

	data = ``echo '{"a": 1}'``
	echo("Parsed data: $data, a is ${data.a}")
	# Output: Parsed data: {a=1}, a is 1


## Binary operators and precedence

Higher numbers mean higher precedence.

	tor     40  "Try ... or", short-circuit   questionable_code tor default_value
	tand    50  "Try ... and", short-circuit  (not sure when it's needed, don't use it)
	or      60  Logical or, short-circuit
	and     65  Logical and, short-circuit
	in      70  Value-in-container check      1 in [1, 2, 3]
	                                          "a" in {"a": 1}
	not in  70  Value-not-in-container check  10 not in [1, 2, 3]
	                                          "b" not in {"a": 1}
	is      90  Instance-of check             1 is Int
	is not  90  Not-instance-of check         1 is not Str


	|      120  "Pipe", currenty not used
	===    130  "Same as"                     v = [1, 2]; v === v
	!==    130  "Not same as",                [1, 2] !== [1, 2]
	==     130  "Equals",                     [1, 2] == [1, 2]
	!=     130  "Not equals"                  [1, 3] != [1, 2]
	<=     150  "Less than or equals"
	<      150  "Less than"
	>=     150  "Greater or equals"
	>      150  "Greater"
	~      150  "Match"                       "a1b2c" ~ /[0-9]/
	~~     150  "Match all"                   "a1b2c" ~~ /[0-9]/
	...    160  "Inclusive range"             0...5               # 0,1,2,3,4,5
	..     160  "Exclusive range"             0..5                # 0,1,2,3,4
	+      190  "Plus"
	+?     190  "Plus maybe"                  "a" + "b"           # "ab"
	                                          null + "b"          # null
	                                          "a" + null          # null
	-      190  "Minus"
	*      200  "Multiply" or "repeat"        3 * 5               # 15
	                                          "ab" * 3            # "ababab"
	                                          EmptyBox * 2        # two values of EmptyBox type
	%      200  "Modulus" or "each"           3 % 2               # 1
	                                          ['a', 'b'] % echo   # Outputs a and b on different lines
	/      200  "Divide" or "map"             10 / 5
	                                          [1, 2, 3] / F(x) x * 2
	?      200  "Filter"                      [1, 2, 3] ? F(x) x > 1
	\      200  "Call"                        [1, 2, 3] \ echo

## Assignment shortcuts

These are syntactically equivalent expressions:

	a = a + 1      a += 1
	a = a - 1      a -= 1
	a = a * 1      a *= 1
	a = a / 1      a /= 1
	a = a % 1      a %= 1
	a = a.f()      a .= f()    a = f(a)
	a = a.f(b)     a .= f(b)   a = f(a, b)

# Language gotchas

This section will be expanded as I get feedback :)

## NGS should not be your first language

I do not recommend NGS as your first language. Python for example would be a much better choice. Programming experience is assumed prior to using NGS.

## Watch the version

NGS is under development. Currently NGS has no version, breaking changes can happen. If you do anything important with NGS, it's preferable to note the git revision you are using for reproducible installation. The plan is to stop breaking NGS when it reaches version 1.0.0 Since that version, the behaviour will be common - patch level increase for fixes, minor for improvements, major for breaking changes.

## Keyword arguments gotchas

Keyword arguments implementation is preliminary so:

	# Keyword arguments are silently ignored if corresponding positional argument is passed.
	kwargs = {"a": 10}
	F f(a, **kw) a; f(1, **kwargs) == 1

	# Keyword arguments for existing named parameters cause parameters not to match
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

## Namespaces gotchas

Since by default all variables are local, the following example will not add method to `some_global_name` multimethod but will create namespace-local `some_global_name` multimethod.

	myns = ns {
		F some_global_name() ...
	}

	# Only available as myns::some_global_name(...)
	# Global some_global_name(...) will not be able to access the
	# above implementation.

The correct version to add method to a global multimethod is

	ns {
		global some_global_name
		F some_global_name() ...
	}

	# Available as global some_global_name(...)

## Comments syntax gotchas

Comments syntax is implemented in many places but not everywhere. If you get syntax error regarding comment, move to somewhere nearby.

## Mutable default parameter gotchas

The code below will probably not to what was intended. Note that default parameter value is only computed once, at function definition time.

	F f(x, a:Arr=[]) {
		a.push(x)
		a
	}

	echo(f(10))  # [10]
	echo(f(20))  # [10,20]

Same happens in Python and hence already described: http://docs.python-guide.org/en/latest/writing/gotchas/#mutable-default-arguments

# Types

NGS is dynamically typed language: values (and not variables) have types.

	a = 1
	a = "ok"
	# 'a' had value of one type and then value of another type

NGS is a "strongly typed" language: values are not implicitly converted to unrelated types. This makes the language more verbose in some cases but helps catch certain type of bugs earlier.

	echo(1+"2")
	# ... Exception of type MethodNotFound occured ...
	# That means that NGS has no method that "knows" how to add an Int and a Str

	echo(1+Int("2"))
	# Output: 3

## Built-in types

There are several built-in types. Sample types and values:

* `Bool` - true, false
* `Int` - 1,2,3
* `Str` - 'a', "bc"
* `Arr` - `[1, true, 'x']`
* `Any` - any value in NGS is of type `Any` :)

Checking types:

	echo(1 is Int)
	# Output: true

	echo(1 is Str)
	# Output: false

	echo(1 is not Str)
	# Output: true

	echo(typeof(1))
	# Output: <Type Int>

See types reference: [ngstyp(1)](ngstyp.1.html).

## Define your own types

You can define your own types. Let's define `Counter` type and a few methods that can operate on values of the `Counter` type. Then we'll define `MyCounter` sub-type and it's `incr` method:

	{
		# Declare that we have a new type
		type Counter

		# * Define constructor which is called when Counter() is invoked.
		# * First argument of constructor, named "c" is the newly-created value of type Counter
		# * The value that the constructor returns is discarded,
		#   Counter() returns the newly-created value of type Counter

		F init(c:Counter) {
			# Initialize the counter_value field
			c.counter_value = 0
		}

		# Define increment method
		F incr(c:Counter) {
			c.counter_value = c.counter_value + 1
			# Return the Counter itself, allowing chaining such as c.incr().incr()...
			c
		}

		# Define get method
		F get(c:Counter) c.counter_value

		c = Counter()
		# c.incr() and incr(c) are syntactically equivalent
		c.incr()
		echo(c.get())
		# Output: 1

		# Declare MyCounter type, a sub-type of Counter
		# MyCounter inherits from Counter meaning that any method that works with Counter also works with MyCounter
		type MyCounter(Counter)

		# Define incr method for MyCounter type
		F incr(c:MyCounter) {
			c.counter_value = c.counter_value + 10
			c
		}
		# incr method has now two different implementations: one for Counter type and one for MyCounter type.

		# Instantiate new MyCounter
		c = MyCounter()

		# * Will run incr(c:MyCounter) method.
		# * Both incr(c:Counter) and incr(c:MyCounter) implementations match the arguments,
		#   the second implementation wins because it was declared last and search is perfomed
		#   from last to first.
		c.incr()

		# c.get() will run get(c:Counter) because
		# parameter of type Counter and argument of type MyCounter will match
		echo(c.get())
		# Output: 10
	}

## Type constructors and object creation

* When type definition (`type T`) is executed, NGS defines the `.constructors` MultiMethod that includes one method. This method creates object of type `T`. The type of this method is `NormalTypeConstructor`.
* After defining a type `T`, one can add methods using `F T(...) {...}` definitions. If you add your method to the `.constructors` field, please make sure it returns an object of type `T` or object of a type that is a subtype of `T`. Otherwise, it will be surprising for the callers.
* When you call a user defined type, its `.constructors` MultiMethod is called.
* If the default constructor added by NGS (method of type `NormalTypeConstructor`) gets executed, it does the following:
	* Creates object of type `T`
	* Runs `init()` MultiMethod with the new object as the first argument and the arguments of the call to `T(...)` as second and on arguments to `init(...)`.
	* Note that calling `init()` might trigger `MethodNotFound` exception. It is ignored if and only if the call to `T()` had no arguments.


	# Customizing Box object creation (code from stdlib)
	F Box(x) FullBox(x)
	F Box(n:Null) EmptyBox()
	F Box(a:Arr, idx) { ... }
	F Box(h:Hash, k) { ... }

	# FullBox initialization, called when FullBox(x) is called
	F init(b:FullBox, val) b.val = val

	# Note that there is no init(eb:EmptyBox) as there is nothing to initialize

	Box(1)    # FullBox with 1 as value
	Box(null) # EmptyBox


# Methods, multimethods and calling

Each value in NGS has a type, similar to many other languages. One of the main features of NGS is choosing the correct method of a multimethod, based on types of the arguments:
Let's start with the following snippet:

	F +(a:Int, b:Int) {
		...
	}

	F +(s1:Str, s2:Str) {
		...
	}

	{
		1 + 1
		# -> 2

		'a' + 'b'
		# -> 'ab'
	}

The `+` in NGS is a multimethod. It has few methods. You can see definitions of two of the methods in the example above. One method can add numbers. Another method concatenates strings. How NGS knows which one of them to run? The decision is made based on arguments' types. NGS scans the methods list backwards and invokes the method that matches the given arguments (this matching process is called multiple dispatch).

# Handlers and hooks

Handlers and hooks are called by NGS when a certain condition occurs. What exactly happens when they are called differs between handlers and hooks.

**A handler** is a multimethod. Like with any other multimethod, you can override what it does by defining your own method with the same name further down in the code. Since standard handlers are defined in **stdlib.ngs** which is typically loaded first, your own methods will be "further down".

**A hook** is a `Hook` object. Some hooks are called by NGS when a certain condition occurs. You are free to create and use your own hooks. When called, it executes all registered functions. The main difference is that using hook you get accumulative behaviour instead of overriding behaviour.

User-defined hook example:

	{
		h = Hook()
		h.push({ echo("A") })
		h.push({ echo("B") })
		h()
	}
	# Outputs one per line: A, B

Another way is to add named hook handlers (also a practical example):

	exit_hook['cleanup_temp_files'] = F(exit_info:Hash) {
		# remove my temp files
	}


## `method_not_found_handler`

`method_not_found_handler` is called when a multimethod was called but no method matched the arguments. Use `F method_not_found_handler(callable:Fun, *args) ...` to add your behaviours.


## `global_not_found_handler`

`global_not_found_handler` is called on attempt to read from an undefined global variable. Sample usage from **stdlib.ngs**

	F global_not_found_handler(name:Str) {
		require("${NGS_DIR}/autoload/${name}.ngs")
	}


## `exit_hook`

`exit_hook` is called when NGS is about to exit. Typical cases are:

* All of the given code was executed.
* `throw` was invoked and no matching `catch` was found.

Method signature: `exit_hook(exit_info:Hash)`. `exit_info` currently has two keys: `exit_code` and `exception`. **stdlib.ngs** defines two standard hooks.

	$ ngs -pi 'exit_hook.Hash()'
	Hash of size 2
	[print_exception] = <UserDefinedMethod <anonymous> at /usr/share/ngs/stdlib.ngs:2110>
	[exception_to_exit_code] = <UserDefinedMethod <anonymous> at /usr/share/ngs/stdlib.ngs:2117>

* `print_exception` prints exception details if an exception occurred.

# Variables' scoping rules

## Default scoping rules

In a function, any variable that is not assigned to inside the function is looked up as an **upvar** (enclosing functions) and as **global**.

	a = 1
	F f() {
		echo(a)
	}
	f()
	# Output: 1

	a = 1
	F f() {
		a = 2
		F g() {
			echo(a)
		}
		g()
	}
	f()
	# Output: 2

In a function, any identifier that is mentioned in any of the enclosing functions is automatically `upvar` - references the variable in the outer scope.

	a = 1
	F f() {
		a = 2
		F g() {
			a = 10
		}
		g()
		echo(a)
	}
	f()
	echo(a)
	# Output: 10
	# Output: 1

In a function, any variable that is assigned to (including the `i` in constuct `for(i;10) ...`) in the function is automatically `local` unless it's an `upvar` as described above. Inner functions are also `local` by default.

	a = 1
	F f() {
		a = 2
		echo(a)
		F inner_func() "something"
	}
	f()
	echo(a)
	# Output: 2
	# Output: 1
	# Can not call inner_func from outside f()

## Modifying variables' scoping

You can modify default scoping using the `global` and `local` keywords.

	a = 1
	F f() {
		a = 2
		F g() {
			# local instead of upvar
			local a
			a = 3
		}
		g()
		echo(a)
	}
	f()
	# Output: 2

	a = 1
	F f() {
		a = 2
		F g() {
			# global instead of upvar
			global a
			a = 3
		}
		g()
		echo(a)
	}
	f()
	# Does not work yet due to a bug, "a" stays upvar
	# Output: 3

# Predicates

Methods such as `filter`, `filterk`, `filterv`, etc take predicate as one of the arguments.
Traditionally, such functions took a function (method) as the predicate argument.
A predicate function is a function with one parameter and that returns a boolean.

NGS allows more powerful and convenient usage of predicates in these and other methods.
All kinds of instances can be passed as predicates.
NGS higher-order functions that have predicate parameters convert the predicates into predicate functions using the `Pred` function:

	# Note that "predicate" can be of any type
	F filter(e:Eachable1, predicate) {
		pred = Pred(predicate)  # <-- Converting predicate to a predicate function.
		t = typeof(e)
		ret = t()
		e.each() do F(elt) {
			if pred(elt)
				ret.push(elt)
		}
		ret
	}

This behaviour allows defining how any given type behaves as a predicate. Several built-in types have `Pred` function defined for them.
Here are some examples of how this can shorten the code:


	# Would be {...}.filterk(F(k) k ~ /^b/)
	{"a1": 1, "a2": 2, "b1": 10}.filterk(/^b/)  # {"b1": 10}

	# Would be {...}.filter(F(elt) elt.x == 7)
	[{"x": 10, "y": 20}, {"x": 7, "y": 30}].filter({"x": 7})  # [{x=7, y=30}]

	# Would be {...}.filter(F(t) t is Str)
	["abc", 1, "def"].filter(Str)  # [abc,def]

One can easily define how a type behaves as a predicate: just define appropriate `Pred` function.
Here is example of `Pred` function that allows `.filter(SomeType)` such as `.filter(Str)` example above:

	F Pred(t:Type) F is_pred(x) x is t

That's a function (`Pred`) that returns a predicate function which is called `is_pred`.
Naming the returned function helps a bit when looking at backtrace. `is_pred`, the predicate functions, checks whether it's argument is of type `t`.

Note that using a function as predicate is still possible:

	[1,2,11].filter(F(x) x > 10)  # [11]

That is because `Pred` function leaves it's argument as is, if it's a function:

	F Pred(f:Fun) f

# Iterators

Iterators give more flexibility and control as opposed to `each` iteration.

## The iterator protocol

Iterators must implement

* `Bool(x:YOUR_ITER_TYPE)` which tells whether there are more values
* `next(x:YOUR_ITER_TYPE)` which returns the next value (or throws `NoNext` exception)

When `for x in EXPR { BODY }` syntax is used, it is equivalent to the following:

	_hidden_iter = Iter(EXPR)
	while _hidden_iter {
		x = _hidden_iter.next()
		BODY
	}

## Referencing the iterator in the BODY

Since `Iter(x:Iter)` is defined as `x`, you can use the following solution if you want to use the `for` syntax and have access to the iterator:

	for x in my_iter=Iter(EXPR) {
		BODY # can manipulate my_iter for advanced control
	}

The above works as follows: `my_iter=Iter(EXPR)` is an assignment which evaluates to an `Iter` object. `for` uses `Iter` on that value to get an iterator but it is the same iterator that `my_iter` references.

## Built-in iterators

See `Iter` type documentation to see which iterators are available in NGS.

# Executing external programs

When a command (more precisely commands pipeline) is parsed, an `CommandsPipeline` object is created. Then depending on the syntax, appropriate method is called with the `CommandsPipeline` argument. The methods are: `$()`, ```` `` ````, and ```````` ```` ````````.

The syntactic options for executing external programs are:

* Being in top level syntax of an NGS script, which is the **commands syntax**
* In **code syntax**: `$(...)` - run the process and get the reference to it
* In **code syntax**: `%(...)` - construct the `CommandsPipeline` but do not run it. Useful for passing ready-to-run commands around: `t=%(ls /); ...; $($t)`
* In **code syntax**: `` `...` `` - capture the output
* In **code syntax**: ```` ``...`` ```` - capture and parse the output

Note that if you want just to construct the command and not run it

External commands are represented with type `CommandsPipeline`. In the simple case, there is exactly one command. In more complex cases `CommandsPipeline` references several programs and pipes between them, for example constructed by `$(my_prog_1 | my_prog_2 | my_prog_3)` .

	$ ngs -pi '$(ls | wc -l)'
	CommandsPipeline
	  command[0]: <Command options={} redirects=[<Redirect 1 null <WritingPipeBetweenChildren read_fd=4 write_fd=5>>] argv=[ls]>
	  process[0]: Process
	  process[0]:   command = <Command options={} redirects=[<Redirect 1 null <WritingPipeBetweenChildren read_fd=4 write_fd=5>>] argv=[ls]>
	  process[0]:   pid = 87711
	  process[0]:   exit_code = 0
	  process[0]:   exit_signal = 0
	  process[0]:   output on fd 1, stdout (0 lines):
	  process[0]:   output on fd 2, stderr (0 lines):
	  pipe[1]: <CommandsPipe options={} name=|>
	  command[1]: <Command options={} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=6 write_fd=7>>,<Redirect 0 null <ReadingPipeBetweenChildren read_fd=4 write_fd=5>>] argv=[wc,-l]>
	  process[1]: Process
	  process[1]:   command = <Command options={} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=6 write_fd=7>>,<Redirect 0 null <ReadingPipeBetweenChildren read_fd=4 write_fd=5>>] argv=[wc,-l]>
	  process[1]:   pid = 87712
	  process[1]:   exit_code = 0
	  process[1]:   exit_signal = 0
	  process[1]:   output on fd 1, stdout (1 lines):
	  process[1]:           20
	  process[1]:   output on fd 2, stderr (0 lines):

## Substitution of command line arguments

	$(ls $fname)      # $var_name - expands to exactly one argument
	$(ls ${ EXPR })   # ${ EXPR } - expands to exactly one argument
	$(ls $*fnames)    # $*var_name - expands to zero or more arguments
	$(ls $*{ EXPR })  # $*{ EXPR } - expands to zero or more arguments

`EXPR` above can be single expression or any number expressions, new-line or `;` separated, value of last one is used for the substitution.

The examples above show `$()` syntax but the substitution works the same for all syntaxes mentioned in **syntactic options for executing external programs** above.

## Options

Rationale: it is sometimes desirable to alter behaviour of execution of external program.
NGS provides syntax for passing arbitrary key-value pairs to augment `CommandsPipeline` or `Command`.

Syntax:

* Options for one command: `option1:value1 option2:value2 optionN:valueN command and arguments` inside `$()` and friends and at the top level.
* Options for one the whole pipeline: `option1::value1 option2::value2 optionN::valueN command_1 | command_2` inside `$()` and friends and at the top level.

In both cases, the values must follow the colon immediately, no spaces allowed. Missing value defaults to `true`.

Options:

* `$(nofail: my_command)` - (deprecated, use `$(ok: my_command)`) no exception will be thrown as `finished_ok` always returns `true` for commands with option.
* `$(ok:CODES my_command)` - `finished_ok` will return `true` for the listed `CODES`. `CODES` can be a boolean (`Bool`), single integer (`Int`), an array of integers (`Arr` of `Int`), or a `NumRange`.
* `$(top_level:: my_command)` - do not capture the output of the command. This is the default behaviour of the top-level **commands syntax**: the stdout connected to the stdout of the NGS process.
* Internally the `&` in `$(my_command &)` sets the `&` option to `true`. This mechanism can be used to modify the behaviour of `CommandsPipeline` prepared by `%(...)`.

		ngs -pi '%(ls / &)'
		CommandsPipeline
		  options: Hash of size 1
		  options:   [&] = true
		  command[0]: <Command options={} redirects=[] argv=[ls,/]>

* `` `line: my_prog` `` - return the first line of the output, without the new-line characters

**WARNING**: unrecognized options are silently ignored.

## Input/Output Redirections

Redirections are represented by the `Redir` type. When a command is parsed and converted to `CommandsPipeline`, each `Command` gets a (possibly empty) list of redirections (in the `.redirects` property)

Redirections syntax (based on bash syntax):

* Overwrite : `my_command >myfile`
* Append: `my_command >>myfile`
* Read from: `my_command  <myfile`
* Specify file descriptor, overwrite: `my_command 2>myfile`
* Specify file descriptor, append: `my_command 2>>myfile`
* Capture: `my_command 2>${true}` . The default is to not capture stderr but rather output it immediately to NGS' process stderr.


	$ ngs -pi '$(ok: ls xxx)'
	ls: xxx: No such file or directory
	CommandsPipeline
	  command[0]: <Command options={ok=true} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[ls,xxx]>
	  process[0]: Process
	  process[0]:   command = <Command options={ok=true} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[ls,xxx]>
	  process[0]:   pid = 26157
	  process[0]:   exit_code = 1
	  process[0]:   exit_signal = 0
	  process[0]:   output on fd 1, stdout (0 lines):
	  process[0]:   output on fd 2, stderr (0 lines):
	$ ngs -pi '$(ok: ls xxx 2>${true})'
	CommandsPipeline
	  command[0]: <Command options={ok=true} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=6 write_fd=7>>,<Redirect 2 > <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[ls,xxx]>
	  process[0]: Process
	  process[0]:   command = <Command options={ok=true} redirects=[<Redirect 1 null <CollectingPipeFromChildToParentProcess read_fd=6 write_fd=7>>,<Redirect 2 > <CollectingPipeFromChildToParentProcess read_fd=4 write_fd=5>>] argv=[ls,xxx]>
	  process[0]:   pid = 26179
	  process[0]:   exit_code = 1
	  process[0]:   exit_signal = 0
	  process[0]:   output on fd 1, stdout (0 lines):
	  process[0]:   output on fd 2, stderr (1 lines):
	  process[0]:     ls: xxx: No such file or directory

## Exit codes handling when running external programs

Immediately after an external program finishes, it's exit code (and in future, possibly other aspects) is checked using `finished_ok` multimethod. The multimethod returns a `Bool`. If it's `false`, an exception is thrown.

For unknown programs, `finished_ok` returns `false` for non-zero exit code and hence an exception is thrown. This behaviour can be modified with the `ok:` option of the command.

NGS knows about some of external programs (`false`, `test`, `fuser`, `ping`) which might return non-zero exit codes which do not indicate an error. These commands do not cause exceptions for exit code one.

If there is a particular program that you are using which also returns non-zero exit codes which should not cause an exception, you should add your own method `finished_ok` which handles that program. Please consider contributing this addition back to NGS.

## External program boolean value

When converting to `Bool`, for example in expression `if $(test -f my_file) { ... }`, all programs in the `CommandsPipeline` must exit with code zero in order to get `true`, otherwise the result of converting to `Bool` is `false`.

## Constructing command line arguments

While it's feasible to construct an array with command line arguments for a program, `Argv` makes this task much easier by supporting common cases such as omitting a command line switch which has no corresponding value to use.

See `Argv` multimethod.

## Globbing

Globbing is not implemented yet.

# NGS exit codes

Both regular results of running a program and exceptions are converted to exit code using `ExitCode` MultiMethod. Built in methods of `ExitCode` do the following conversion:

* `true` - 0
* `false` - 1
* `Int` object - the number itself
* `Exception` objects - 240
* `FullBox` object - 0
* `EmptyBox` object - 1
* `Success` object - 0
* `Failure` object - 1
* `FatalError` objects - using the `exit_code` field of the exception, which if not given to `FatalError` constructor defaults to 1.
* `CommandsPipeline` / `Process` objects - the exit code of the external command
* `MatchY` object - 0
* `MatchN` object - 1
* All others - 0

Special cases:

* An exception and additional exception when converting the first exception using `ExitCode` - 241
* An exception and `exit_hook` failure - 242
* An exception, additional exception when converting the first exception using `ExitCode`, and `exit_hook` failure - 243
* NGS could not find the bootstrap file - 244
* NGS could not open bootstrap file - 245
* NGS could not parse bootstrap file - 246

## Customizing translation of your types/objects to exit code

Define `F ExitCode(t:MyType) {...}` method that returns `Int` to customize the exit code.

# Debugging

This section is planned to grow over time.

## MethodNotFound exception

	$ ngs -pi 'map(1,2,3)'
	...
	Exception of type MethodNotFound occurred
	...

	# Means the parameters did not match any of the existing methods.
	# See which methods are defined:

	ngs -pi map
	MultiMethod
	  Methods
		Array of size 7
		  [0] = <UserDefinedMethod map(e:Eachable, mapper:Fun) at /usr/local/lib/ngs/stdlib.ngs:242>
		  [1] = <UserDefinedMethod map(arr:Arr, n:Int, mapper:Fun) at /usr/local/lib/ngs/stdlib.ngs:1600>
		  [2] = <UserDefinedMethod map(h:Hash, mapper:Fun) at /usr/local/lib/ngs/stdlib.ngs:2209>
		  [3] = <UserDefinedMethod map(fb:FullBox, mapper:Fun) at /usr/local/lib/ngs/stdlib.ngs:2550>
		  [4] = <UserDefinedMethod map(eb:EmptyBox, mapper:Fun) at /usr/local/lib/ngs/stdlib.ngs:2555>
		  [5] = <UserDefinedMethod map(s:Success, fun:Fun) at /usr/local/lib/ngs/stdlib.ngs:2762>
		  [6] = <UserDefinedMethod map(f:Failure, fun:Fun) at /usr/local/lib/ngs/stdlib.ngs:2771>
