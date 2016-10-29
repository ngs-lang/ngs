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

If you do, you can run the script as `./script_name.ngs` or `/full/path/to/script_name.ngs` (you must make your script executable, `chmod 655 script_name.ngs`.

See more about running NGS in [ngs(1)](ngs.1.html).

# WHY NGS?

If your attitude towards system tools is like mine, NGS would resonate better with how you think than bash, Python, Ruby, Perl or any other language for systems administration tasks.

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

You are probably using Python or Ruby or Perl (and if you are like me, Perl is out of the question because of it's syntax horror). You use one of the above languages because bash is not powerful enough / not convenient enough to do the tasks that these languages do. On the other hand something as simple as `echo mystring >myfile` or run an external program is not as convenient to do in these languages. Yes all of the languages above support system tasks to some degree. None of these languages support system tasks as a language that was built ground-up for system tasks. See the double-backtick examples above... for example.

## You are using configuration management tools

I've seen unjustified usage of configuration management tools too much. Yes, these are the "Cool Shiny New DevOps" hype tools: Chef, Puppet, Ansible and friends. I assume that in many situations it would be better to script these tasks instead of using configuration management tools. They are very complex and they take control away from you. The price of complexity is too high in many cases in my opinion. Any system beyond the most simple will need customization/wrapping/forking of ready-made modules for these configuration management systems to such degree that usage of such systems will be at very least questionable TCO-wise.

Why not make your own clean solution that matches your own needs exactly? I mean except for being unpopular. The issue is that currently there is no good language to make these scripts. It would be inconvenient to script the task even if you wanted. I hope NGS will enable easy scripting of system tasks.


# LANGUAGE PRINCIPLES OVERVIEW

This section is about principles behind NGS language design.

## Systems engineers language

NGS is a domain-specific language. It is aimed to solve common system tasks in a convenient manner.

## Do the most practical thing

* `read('myfile.json')` will parse the JSON and return the data structure. (Use `fetch()` to get raw contents).
* The ```` ``my_command`` ```` will parse the command output (JSON for example) and return the data structure. Note that ```` ``aws ...`` ```` will be parsed even further (not just JSON) to return more usable data structures.
* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

## Uniformity

NGS tries to be uniform wherever possible to minimize surprises.

## Power

Trade-offs between power and not allowing to shoot yourself in the foot are usually resolved in favor of the power solution. The language is aimed at experienced engineers which use their own judgement. The language should be powerful enough to shoot all feet in the building at once.

## Simple methods naming for less guess work

* `1+2` adds the numbers (method name `+`)
* `arr1+arr2` adds (concatenates) arrays
* `hash1+hash2` produces merged hash.

## Extensibility

* `read('your_file.super-format')` can be extended to parse your format.
* `fetch`, which reads from a file, can be extended to support HTTP or S3.
* Define any operator for existing or your custom types.

## Simplicity

* Minimal possible number of concepts in the language.
* No classes. Only types, methods and multiple dispatch (picking the right method implementation by matching types of parameters and arguments)
* Simple type system.

## Familiarity

Many concepts and syntax constructs come from other languages.

# SYNTAX

NGS has two syntaxes: **command syntax** and **code syntax**.

## Command syntax

This is the close-to-bash syntax geared towards running external programs and i/o redirection.
Command syntax is the syntax at the top level of every NGS script. The most simple NGS scripts might look very similar to bash scripts. Commands are separated by either newlines or by semicolon (`;`).

Example:

	cat a.txt; touch myfile
	echo mystr >myfile

In addition to running commands and performing redirections, there are several expressions that are also supported in the **commands syntax**. Note `code` below means **code syntax** expressions.

* `{ code }` - see **Switching between syntaxes** below
* assignment: `myvar = code` (`myvar = 1 + 2`)
* in-place assignment: `myvar += code` (`myvar += 10`)
* wrapper (also called decorator): `@code code`
* function definition: `F myfunc(params...) code` (`F myfunc(n:Int) echo(n*10)`)
* function call: `myfunc(arguments...)` (`myfunc(7)`)
* `if condition_code [then] yes_code [else] no_code`
* `while condition_code body_code`
* `for(...) body_code`
* comment: `#` and till end of line

TODO: redirection syntax

## Code syntax

Code syntax resembles other high-level languages such as Python or Ruby.

Example:

	1 + 2 * 3; %[abc def ghi].without('ghi').each(echo)

Expressions are separated by either newlines or by semicolon (`;`).

**code syntax** is the syntax of **-e**, **-E**, **-p**, **-pi** and **-pj** switches to `ngs` interpreter. Example:

	ngs -p '(1...10).filter(F(num) num % 2 == 0)'

	# Output:
	[2,4,6,8,10]

If the above example looks too verbose, here is the shorter and uglier alternative:

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

## Quick dive into syntax

	# comment

	# Function call
	echo('Hello world')

	# Function call, alternative syntax:
	#   something.method_name(optionally_more_args)
	# Whatever is before the dot is used first parameter
	'Hello world'.echo()

	# Assignment
	a = 1 + 2

	# String interpolation. Two expressions on same line
	echo("A is now $a"); echo('... and additional statement after semicolon')

	# String interpolation with ${...}
	echo("expr=${10+20}")
	# Outputs: expr=30

	# Basic constants: true, false, null
	a = true
	a = false
	a = null

	# Switch to code syntax inside { ... }.
	{
		# Define type
		type Vehicle

		# Define sub-type
		type Car(Vehicle)

		# Define method
		# c - parameter name
		# Car - parameter type
		F drive(c:Car) {
			expression1
			expression2

			# Same as:
			#   if condition1 { return expr1 }
			condition1 returns expr1

			last_expression_is_the_return_value
		}

		# Array literal, x is Arr type
		x = [1, 2, 'x']

		# Array literal, alternative syntax
		x = [
			"blah"
			2
			'x'
		]

		# Hash literal, x is Hash type
		x = {"a": 1, "b": 2}

		# Hash literal, alternative syntax
		x = {
			"a": 1
			"b": 2
		}

		# Define method
		# Omitting { ... } for method body code when it's just a single expression
		F my_summ1(a:Int, b:Int) a+b

		# Define method, all extra arguments go to the "rest" array
		F my_rest_method(a:Int, *rest) sum(rest) - a

		# Define method, all extra keyword arguments go to the "kw_args" hash
		F my_kw_method(a:Int, **kw_args) sum(kw_args.values()) - a

		# Define method, b has default value
		F my_summ2(a:Int, b:Int=100) a+b

		# Anonymous function literal
		f = F(a,b) a+b

		# Anonymous function literal, alternative syntax 1
		# Logically same as f = F(x=null, y=null, z=null) x > y
		# Using X or Y or Z wraps nearest function call in
		# anonymous function. With X, Y and Z arguments which
		# all default to null. Nearest function call in this case
		# is the > operator.
		[1,2,3,11,12].count(X>10).echo()
		# Output: 2

		# Anonymous function literal, alternative syntax 1 (additional example)
		# f is now partially applied my_two_args_func.
		# Logically same as f = F(x=null, y=null, z=null) my_two_args_func(x, 10)
		f = my_two_args_func(X, 10)

		# Anonymous function literal, alternative syntax 2
		# Logically same as f = F(A=null, B=null, C=null) A+B+1
		f = { A + B + 1 }

		# --- Short circuit binary operators ----------
		a = 1 and 2                    # a = 2
		a = null and 2                 # a = null
		a = 1 or 2                     # a = 1
		a = null or 2                  # a = 2
		a = code_with_exception tor 3  # a = 3, exception discarded

		# --- Ignoring exceptions with "try" without "catch" ----------
		a = try one_expression_code_with_exception   # a = null, exception discarded
		a = try { code; with; exception }            # a = null, exception discarded
		my_data = try my_parsed_json.optional_field  # could be useful in real life

		# --- Exceptions ----------
		# Define our exception type
		# You can throw anything but Error has a nice constructor
		# that takes a message argument so inheriting it.
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

		# --- Loops ----------
		for(i=0; i<5; i+=1) {
			if i == 3 {
				continue
			}
			echo("Regular loop, iteration $i")
		}

		# Same as above
		for(i;5) {
			i == 3 continues
			echo("Shorthand loop, iteration $i")
		}

		# While loop and "breaks" demonstration
		i = 0
		while i<10 {
			echo("While loop, iteration $i")
			i += 1
			# "e1 breaks" is same as "if e1 { break }"
			i == 2 breaks
		}

		# --- Accessing items in arrays, hashes and other containers ----------
		arr = [10, 20, 30]
		item_1 = arr[1]
		echo("Arr item 1 is $item_1") # "... 20"

		myhash = {"a": 1, "b": 2}
		item_b = myhash["b"]
		item_b_alt = myhash.b
		echo("Hash item 'b' is $item_b / $item_b_alt") # "... 2 / 2"

		# --- switch and switch-like statements ----------

		# switch
		a = 10
		result = switch a {
			10 "ten"
			20 "twenty"
			30 { my_complex_code; "thirty" }
		}
		echo("Switch result for $a is $result") # "... ten"

		# cond
		a = 12
		result = cond {
			a > 10
				"Excellent"
			a > 5 {
				my_complex_code; "Good enough"
			}
			a > 3
				"so so"
		}
		echo("Cond result for $a is $result") # "... Excellent"

		# There are also eswitch and econd which throw an exception
		# when no match is found as opposed to swicth and cond which
		# return null when no match is found.

		F will_throw_exception1() {
			a = "bad value"
			result = eswitch a {
				1 "one"
				2 "two"
			}
		}

		F will_throw_exception2() {
			a = 10
			result = econd {
				a > 15 "one"
				a > 20 "two"
			}
		}

		# --- "defined" keyword ----------
		F show_defined() {
			echo(defined a) # false
			a = 1
			echo(defined a) # true
		}
		echo("Defined:")
		show_defined()
	}

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

# LANGUAGE GOTCHAS

This section will be expanded as I get feedback :)

## NGS should not be your first language

I do not recommend NGS as your first language. Python for example would be a much better choice. Programming experience is assumed prior to using NGS.

## Watch the version

NGS is under development. Currently NGS has no version, breaking changes can happen. If you do anything a important with NGS, it's preferable to note the git revision you are using for reproducible installation. The plan is to stop breaking NGS when it reaches version 1.0.0 Since that version, the behaviour will be common - patch level increase for fixes, minor for improvements, major for breaking changes.

## Keyword arguments gotchas

Keyword arguments implementation is preliminary so:

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

# TYPES

NGS is dynamically typed language: values (and not variables) have types.

	a = 1
	a = "ok"
	# 'a' had value of one type and then value of another type

NGS is a "strongly typed" language: values are not implicitly converted to unrelated types. This makes the language more verbose but prevents some bugs.

	echo(1+"2")
	# ... Exception of type ImplNotFound occured ...
	# That means that NGS has no method implementation that "knows" how to add an Int and a Str

	echo(1+Int("2"))
	# Outputs: 3

# BUILT-IN TYPES

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

See types reference: [ngstyp(1)](ngstyp.1.html).

# DEFINE YOUR OWN TYPES

You can define your own types. Let's define `Counter` type and a few methods that can operate on values of the `Counter` type. Then we'll define `MyCounter` sub-type and it's `incr` method:

	{
		# Declare that we have a new type
		type Counter

		# * Define constructor which is called when Counter() is invoked.
		# * First argument of constructor, named "c" is the newly-created value of type Counter
		# * The value that the constructor returns is discarded,
		#   Counter() returns the newly-created value of type Counter

		F init(c:Counter) {
			# Initialize the counter_value attribute
			c.counter_value = 0
		}

		# Define increment method implementation
		F incr(c:Counter) {
			c.counter_value = c.counter_value + 1
			# Return the Counter itself, allowing chaining such as c.incr().incr()...
			c
		}

		# Define get method implemetation
		F get(c:Counter) c.counter_value

		c = Counter()
		# c.incr() and incr(c) are syntactically equivalent
		c.incr()
		echo(c.get())
		# Output: 1

		# Declare MyCounter type, a sub-type of Counter
		# MyCounter inherits from Counter meaning that any method that works with Counter also works with MyCounter
		type MyCounter(Counter)

		# Define incr method implementation for MyCounter type
		F incr(c:MyCounter) {
			c.counter_value = c.counter_value + 10
			c
		}
		# incr method has now two different implementations: one for Counter type and one for MyCounter type.

		# Instantiate new MyCounter
		c = MyCounter()

		# * Will run incr(c:MyCounter) method implementation.
		# * Both incr(c:Counter) and incr(c:MyCounter) implementations match the arguments,
		#   the second implementation wins because it was declared last and search is perfomed
		#   from last to first.
		c.incr()

		# c.get() will run get(c:Counter) because
		# parameter of type Counter and argument of type MyCounter will match
		echo(c.get())
		# Output: 10
	}

# METHODS, METHOD IMPLEMENTATIONS AND CALLING

Each value in NGS has a type, similar to many other languages. One of the main features of NGS is choosing the correct **method implementation** based on types of the arguments:
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

The `+` in NGS is a method. It has few **method implementations**. You can see definitions of two of the implementations in the example above. One implementation can add numbers. Another implementation concatenates strings. How NGS knows which one of them to run? The decision is made based on arguments' types. NGS scans the **method imlementations** array backwards and invokes the **method implementation** that matches the given arguments (this matching process is called multiple dispatch).

# HANDLERS AND HOOKS

Handlers and hooks are called by NGS when a certain condition occurs. What exactly happens when they are called differs between handlers and hooks.

**A handler** is a regular method (`Arr` of **method implementations**). Like with any other method, you can override what it does by defining your own **method implementation** with the same name further down in the code. Since standard handlers are defined in **stdlib.ngs** which is typically loaded first, your own **method implementation** will be "further down".

**A hook** is an instance of the `Hook` type. Some hooks are called by NGS when a certain condition occurs. You are free to create and use your own hooks. When called, it executes all registered functions. The main difference is that using hook you get accumulative behaviour instead of overriding behaviour.

User-defined hook example:

	{
		h = Hook()
		h.push({ echo("A") })
		h.push({ echo("B") })
		h()
	}
	# Outputs one per line: A, B

Same example using decorators syntax:

	h = Hook()
	@h {
		echo("A")
	}
	@h {
		echo("B")
	}
	h()

And finally practical example:

	@exit_hook {
		# Remove my temp files
	}


## `impl_not_found_handler`

`impl_not_found_handler` is called when a method was called but no **method implementation** matched the arguments. Use `F impl_not_found_handler(callable:Fun, *args) ...` to add your behaviours.


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

	# ngs -pi 'exit_hook.handlers'
	Hash of size 2
	[print_exception] = <Closure <anonymous> at /usr/share/ngs/stdlib.ngs:2110>
	[exception_to_exit_code] = <Closure <anonymous> at /usr/share/ngs/stdlib.ngs:2117>

* `print_exception` prints exception details if an exception occurred.
* `exception_to_exit_code` sets the exit code using `to_exit_code`. Unless defined for your specific exception, `to_exit_code` of an `Exception` returns **200**.

# VARIABLES SCOPING RULES

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

In a function, any variable that is assigned to (including the `i` in constuct `for(i;10) ...`) in the function is automatically `local` unless it's an `upvar` as described above.

	a = 1
	F f() {
		a = 2
		echo(a)
	}
	f()
	echo(a)
	# Output: 2
	# Output: 1

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

