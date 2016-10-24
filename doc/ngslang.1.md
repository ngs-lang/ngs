% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngslang - Next Generation Shell language tutorial.

# WHY NGS?

## Best fit for systems administration tasks

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
* The ```` ``my_command`` ```` will parse the command output (JSON for example) and return the data structure. Note that ```` ``aws ...`` ```` will be parsed even further (not just JSON) to become more usable data structures.
* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

## Uniformity

NGS tries to be uniform wherever possible to minimize surprises.

## Power

Trade-offs between power and not allowing to shoot yourself in the foot are usually resolved in favor of the power solution. The language is aimed at experienced engineers which use their own judgement. The language should be powerful enough to shoot all feet in the building at once.

## Simple methods naming for less guess work

`1+2` adds the numbers (method name `+`), `arr1+arr2` adds (concatenates) arrays and `hash1+hash2` produces merged hash.

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

# SYNTAX OVERVIEW

NGS has two syntaxes.

## Command syntax

This is the close-to-bash syntax geared towards running external programs and i/o redirection.
Command syntax is the default syntax at the top level of every NGS script. The most simple NGS scripts might look very similar to bash scripts. Commands are separated by either newlines or by semicolon (`;`).

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

Code syntax resembles other high-level languages such as Python or Ruby. Example:

	1 + 2 * 3

Expressions are separated by either newlines or by semicolon (`;`).

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

# MAIN LANGUAGE CONCEPTS

## Types

In NGS, each value is of some type. `1` for example is an `Int`, `"xyz"` is a `Str` and `[1,2,3]` is an `Arr`.

Define `Counter` type and a few methods for it (code syntax, must be inside `{...}`):

	type Counter

	F init(c:Counter) c.counter = 0

	F incr(c:Counter) {
		c.counter = c.counter + 1
		c
	}

	F get(c:Counter) c.counter

Use the `Counter` type:

	c = Counter()
	c.incr()
	echo(c.get())
	# Output: 1

`c.incr()` and `incr(c)` are syntactically equivalent.

Inheritance:

	type MyCounter(Counter)

`MyCounter` inherits from `Counter` meaning that any method that works with `Counter` also works with `MyCounter`:

	F incr(c:MyCounter) {
		c.counter = c.counter + 10
		c
	}

	c = MyCounter()
	c.incr()
	echo(c.get())
	# Output: 10

`incr` method now has two **method implementations**: one for `Counter` and one for `MyCounter` so `incr` method behaves differently for these types.

`get` method still has one **method implementation** which is handling both `Counter` and `MyCounter` types.

## Methods, method implementations and calling

A method in NGS is an `Arr` (array) of functions. Each function in such array is called **method implementation**.

	F bigger(x:Int) x+1
	F bigger(s:Str) "+${s}+"

`bigger` is now an `Arr` with two elements, the functions defined by `F`. When calling an `Arr`, NGS scans the array backwards and invokes the **method implementation** that matches the given arguments (this matching process is called multiple dispatch). Example:

	echo(bigger("x"))
	# Output: +x+

	echo(bigger(1))
	# Output: 2


No matching **method implementation** causes `ImplNotFound` exception.

	echo(bigger(true))
	# ImplNotFound exception

## Customization using methods

Sometimes, like in the `Counter` and `MyCounter` example above, it's enough to define a new **method implementation** and let the arguments matching do the work of figuring out which **method implementation** to run.

In other cases, a finer-grained approach is needed. That's when a `guard` is needed. If `guard` condition evaluates to true, it tells NGS that this **method implementation** is the correct one. If `guard` condition evaluates to false, it tells NGS to keep searching for the correct **method implementation** up the `Arr` it was looking at. Note that using guards does not cancel argument matching done by NGS. Regarding the `guard`, a **method implementation** is only invoked when arguments matched the parameters.

	F bigger(x:Int) {
		guard x >= 100
		x + 10
	}
	echo(bigger(1))
	echo(bigger(100))

	# Outputs one per line: 2, 110

The example above added second **method implementation** with the `x:Int` argument.

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

