% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngslang - Next Generation Shell language reference.

# What is NGS?

NGS is an alternative shell. At its core is a domain-specific language that was specifically designed to be a shell language.

NGS is under development. The language part is already good enough to write some useful scripts. The CLI still doesn't exist and will be written using the same language.


# Running NGS scripts

**ngs** *script_name.ngs*

You can put the following line as first line of your script:

	#!/usr/bin/env ngs

If you do that, you can run the script as `./script_name.ngs` or `/full/path/to/script_name.ngs` (you must make your script executable, by running `chmod 755 script_name.ngs` ).

See more about running NGS in [NGS(1)](ngs.1.md).

# Terminology

* type - Built-in or user-defined data type, similar to Python, Ruby and other languages.
* object - Instance of a type, similar to Python, Ruby and other languages. The phrase "MyType object" refers to an Instance of "MyType".
* field - A named slot of an object, similar to field in Python, Java, etc.
* attributes - Slot for auxiliary data on most types of objects (the ones that are references, i.e. not Int,Bool,Null), typically a `Hash` or `null`. Attributes are not fields and are accessed differently (using `attrs` method).
* method - Built-in or user-defined function. User defined methods can be closures. Methods are also called functions in several places in the documentation.
* multimethod - A MultiMethod object containing ordered list of methods. When called, the appropriate method is selected from the list to perform the computation.

# Language principles

This section is about principles behind NGS language design.

## Systems engineers' language

NGS is a domain-specific language. It is aimed to solve common system tasks in a convenient manner. Some examples:

* `fetch('myfile.json')` will decode the JSON and return the data structure. (Use `read()` to get raw contents).
* The ```` ``my_command`` ```` will decode the command output (JSON for example) and return the data structure. Note that ```` ``aws ...`` ```` will be parsed even further (not just JSON) to return more usable data structures.
* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

## Uniformity

NGS tries to be uniform wherever possible to minimize surprises. The idea here is to match between expected and actual behaviour of given method plus parameters.

## Power

As a rule, trade-offs between power and not allowing to shoot yourself in the foot are resolved in favor of the power solution. The language is aimed at experienced engineers which use their own judgement. The language should be powerful enough to shoot all feet in the building at once.

## Simple methods naming for less guess work

For example, the multimethod `+`:

* `1 + 2` adds the numbers
* `arr1 + arr2` adds (concatenates) arrays
* `hash1 + hash2` produces merged hash.

## Extensibility

* `fetch('your_file.super-format')` can be extended to decode your format.
* `read`, which reads from a file, can be extended to support HTTP or S3.
* Most of the syntax (for example `my_var.my_field` or `my_var[my_index]`) is just sugar for calling methods. This behaviour lets you, the user, define any operator for existing or your custom types.

## Simplicity

Very small number of core concepts in the language:

* Types with a simple type system, geared only toward multiple dispatch. No classes.
* Multimethods, which allow using same method name for operations on different types, as in the `+` example above.
* Closures
* Exceptions

## Familiarity

Many concepts and syntax constructs come from other languages.

# Naming convention

* `var_name`
* `CONST_NAME` - Currently NGS does not support constants, it's only a naming convention
* `method_name` (multimethod name)
* `TypeName`
* `TransformationName` - example: `Strs` (converts to array of strings), `Argv` (constructs command line arguments array), `ExitCode` (converts anything to integer exit code).

Reasoning behind `TransformationName`:

* Transforms data into something else, like many other constructors
* The output data might get it's own data type some day

# The two syntaxes

NGS has two syntaxes: **commands syntax** and **code syntax**.

Motivation: I can not imagine any syntax for a "real" programming language that I would like which is built on top of the **commands syntax**, which is similar to bash. That's why NGS has **code syntax** for "real" programming and **commands syntax** for running external programs and i/o redirection, where this syntax is very convenient.


## Commands syntax

This is the resembles-bash syntax geared towards running external programs and i/o redirection. Command syntax is the syntax at the top level of every NGS script. The most simple NGS scripts might look very similar to bash scripts.
Commands are separated by either newlines or by semicolon (`;`).

	cat a.txt; touch myfile
	echo mystr >myfile
	ls | wc -l

## Code syntax

Code syntax resembles other high-level languages such as Python or Ruby.

Example:

	1 + 2 * 3; %[abc def ghi].without('ghi').each(echo)

Expressions are separated by either newlines or by semicolon (`;`). It is also permitted to have trailing semicolon in the end of a line:

	my_func();
	my_other_func()

**code syntax** is the syntax of **-e**, **-E**, **-p**, **-pi**, **-pl**, **-pj**, **-pjl**, and **-pt** switches to `ngs` interpreter. Example:

	$ ngs -p '(1...10).filter(F(num) num % 2 == 0)'

	# Output:
	[2,4,6,8,10]

## Switching between commands and code syntaxes

In **commands syntax** it is possible to switch to **code syntax** in one of the following ways:

	ls
	{ code syntax here }

	ls ${ code that computes the file name and returns a string,
	spaces don't matter, expaned into single argument of ls }

	# Expands to zero or more positional arguments to ls
	ls $*{ code that computes the files names and returns array of
	strings. Spaces don't matter, each element is expaned into single
	argument of ls }


In **code syntax** it is possible to switch to **commands syntax** in one of the following ways:

	# Capture output
	out = `commands syntax`
	my_var = "mystring\n" + `my other command`

	# Capture output and decode it
	parsed_data_structure = ``commands syntax``
	n = ``curl https://example.com/myservice/stats``.number_of_items_in_storage

	# Get reference to a process
	my_process = $( commands syntax )

# Comments

Comments are allowed in both commands and code syntax. At the beginning of a line, a comment starts with a hash sign (`#`). A comment that starts in a middle of the line starts with space and a hash sign (`#`) and continues to end of the line.

	# comment till end of line
	run_external_program --input my_file # COMMENTS HERE ARE NOT IMPLEMENTED YET

	{
		a = 1 # comment till end of line
	}

Also, lines that start with "TEST " (note the trailing space) are considered to be comments. They are ignored. The purpose is to allow an external testing script to extract and run these tests.

# Variables

As in other languages, variables are named locations that can store values. Variables' names should consist of ASCII letters (a-z, A-Z) and numbers (0-9). Variable name must start with a letter. Assignment to variables looks the same in both commands and code syntax. Referencing a variable in the code syntax is just the variable's name while referencing it in commands syntax or inside string interpolation is `$my_var` (not recommended) or `${my_var}` (recommended).

Advanced topic: more precisely, the naming restrictions for the variables mentioned above are naming restrictions on identifiers. NGS can have variables that are named not by the rules above.

Note that it is not recommended to use names which are not valid identifiers. As an exception to this rule, it's OK for methods which correlate with NGS syntax to have names which are not valid identifiers. Example of such methods are binary operators (`+`, `-`, etc.). See more about methods' naming in "Methods and multimethods" section below.

Assigning to a variable works in both commands and code syntax.

	# Assigning value to a variable.
	# Note that syntax to the right of = is code syntax even if the surrouning syntax is commands syntax.
	a = 1 + 2

	# Referencing a variable in code syntax.
	# Not that arguments to method call are in code syntax even if the surrouning syntax is commands syntax.
	echo(a)
	# Output:
	#   3

	my_file = '1.txt'
	# Referencing a variable inside commands syntax.
	$(ls $my_file)
	$(ls ${my_file})

	# Referencing a variable inside string interpolation.
	echo("A is ${a}")
	# Output:
	#   A is 3

	# In-place assignment ( -=,+=,*=,/=,%= )
	a += 100
	# The above is exactly the same as a = a + 100

Referencing undefined variable will cause `GlobalNotFound` or `UndefinedLocalVar` exceptions.

## Checking whether a variable is defined

	echo(defined a)
	a = 100
	echo(defined a)
	# Output:
	#   false
	#   true

It's a rare circumstance that one needs to use `defined`. Please try to avoid such situations.

## Variables' scoping rules

Variables scoping is similar to Python's.

Variables scope types:

* `local` - local to a method
* `upvar` - local to enclosing method
* `global` - global

In a method, any variable that is not assigned to inside the method is looked up as an `upvar` (enclosing methods) and as `global`.

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

In a method, any variable that is mentioned in any of the enclosing methods is automatically `upvar` - references the variable in the outer scope.

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

In a method, any variable that is assigned to (including the `i` in construct `for(i;10) ...`) in the method is automatically `local` unless it's an `upvar` as described above. Inner methods are also `local` by default.

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

## Planned features for variables

Destructuring assignment. Something like the following:

	[a, b, *rest] = my_arr

Syntax for `Hash` destructuring is not clear yet:

	... = my_hash

Destructuring should also be available in `for`:

	# Hash iterator gives Arr of two elements each iteration.
	# Destructuring would place the two elements in k and v varaibles.
	for [k, v] in my_hash {
		...
	}

# Methods and multimethods

Methods' names are composed of letters (a-z, A-Z), digits (0-9) and the following characters: `_`, `-`, `|`, `=`, `!`, `@`, `?`, `<`, `>`, `~`, `+`, `*`, `/`, `%`, `(`, `)`, `$`, `.`, ```` `` ````, `"`, `:`, (space), `[`, `]` .

## Multi-dispatch

Each value in NGS has a type, similar to many other languages. One of the main features of NGS is choosing the correct method of a multimethod, based on types of the arguments. This feature is called multi-dispatch.

Let's start with the following example:

	F +(a:Int, b:Int) {
		...
	}

	F +(s1:Str, s2:Str) {
		...
	}

	{
		1 + 1     # 2
		'a' + 'b' # 'ab'
	}

The `+` is a multimethod. It has a few methods. You can see definitions of two of the methods in the example above. One method adds numbers. Another method concatenates strings. How NGS knows which one of them to run? The decision is made based on arguments' types. NGS scans the methods list backwards and invokes the method that matches the given arguments (this matching process is called multiple dispatch).

When a `MultiMethod` is called, the multimethod's methods list is searched from last element to first element. The method which parameters' types match is executed. This model is different from many other languages, where the most specific method is called. In NGS, if two methods' parameters both match the arguments, that one that was defined last is called. Typically, methods for more specific types are defined later in code so the behaviour is similar to other languages. If matched method is executed and a `guard` (see below) fails, the search is continued as if the method did not match.

Consider the following types:

	{
		type Vehicle
		type Car(Vehicle)
	}

Typical MultiMethod definition:

	F park(v:Vehicle) { ... }
	F park(c:Car) { ... }

	park(Vehicle())  # calls the first method
	park(Car())      # calls the second method

Leveraging the NGS MultiMethod call behaviour: one can for example do debugging by adding the third method to the `park` MultiMethod:

	F park(v:Vehicle) {
		debug("Parking vehicle ${v}")
		super(v)  # Execute one of the two methods defined above
	}

## Calling a method - simple syntax

This syntax is available in both commands and code syntax.

	echo('Hello world')
	# Output:
	#   Hello world

## Calling a method - object-style syntax

This syntax is available in code syntax only.

As a syntactic sugar, method call `f(a, b, c)` can be written as `a.f(b, c)`.

	{
		'Hello world'.echo()
	}

	# Output:
	#   Hello world

## Methods for operators

In NGS, binary operators are just syntactic sugar for calling methods. The variety of permitted characters in methods' names allow using same method name as the operator.

	# Define the + operator for Fun objects (Fun objects are callable objects: methods and types)
	F +(f:Fun, g:Fun) {
		F composed_function(*args) {
			f(g(*args))
		}
	}
	...
	F reject(something:Eachable, predicate) {
		p = Pred(predicate)
		something.filter(not + p) # Usage
    }

Current methods with special names are listed below.

	# Output wrapped manually for your convenience.
	# All of them correspond to a syntax, mostly binary operators.
	$ ngs -p 'globals().filterv(Fun).keys().filter(/[^a-zA-Z0-9_]/).join(", ")'
	==, ., [], +, *, /, -, <, <=, >, >=,
	.=, []=, %, ===, ::, is not, ::=,
	?, \, +?, !=, ~, not in, .., ...,
	!==, ~~, "$*", $(), ``, ````, %(), //

## Binary operators (methods) and precedence

Higher numbers mean higher precedence. Note that short-circuit operators can not and are not implemented as method calls - that would require eager evaluation of the arguments.

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

## Defining a method

Method definition works in both commands and code syntax.

When defining a named method, NGS automatically creates a `MultiMethod` with the given name (if it does not exist) and appends the new method to the multimethod's list of methods.

	F my_method() {
		echo("wassup?")
		echo("my method is running")
	}

Method with optional parameters

	F mysum(a:Int, b:Int=100) a+b

	echo(mysum(5))
	# Output: 105

	echo(mysum(5, 200))
	# Output: 205

Method with "rest" parameter

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

Method with "rest keywords" parameter

	F print_properties(separator:Str, **kw_args) {
		kw_args.each(F(name, value) {
			echo("$name$separator$value")
		})
	}

	print_properties(' => ', a=10, b=20)

	# Output:
	#   a => 10
	#   b => 20

## Method guards

Method guards restrict method execution to specific conditions, typically the conditions are functions of parameters' values. If the guard condition evaluates to `false`, NGS considers the method as if it did not match the arguments and continues to search for other methods to run in the multimethod's methods list.

	F gg(i:Int) {
		echo("First gg active")
		echo(i*10)
	}

	# gg is now MultiMethod with one method

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

	# gg is now MultiMethod with two methods

	gg(1)
	gg(5)
	# Output:
	#   Second gg checking guard
	#   First gg active
	#   10
	#   Second gg checking guard
	#   Second gg active
	#   500

## Calling super methods

Super methods are methods higher in the multimethod's list of methods.

	F sup(x) x+1

	F sup(x) super(x) * 10

	echo(sup(5))
	# Output: 60

## Anonymous function (method) literal

Syntactically, anonymous method is `F` not followed by a name.

	f = F(item) { echo("Item: $item") }
	echo("F is $f")
	# Output: F is <UserDefinedMethod <anonymous> at 1.ngs:1>

	each([10,20,30], f)
	# Output:
	#   Item: 10
	#   Item: 20
	#   Item: 30

	# Anonymous method as map() parameter
	echo([1,2,3].map(F(x) x*5))
	# Output: [5,10,15]

## Anonymous function (method) literal using magical X, Y or Z variables

Three special variables can be used as a syntactic sugar for creating anonymous methods. A method call which uses one of the special variables X, Y, or Z is wrapped in anonymous function as follows: `F(X=null, Y=null, Z=null) { method_call(....) } `.

Operators are just syntactic sugar for method calls so `X*5` for example is also processed as described.


	# X*5 is same as F(X=null, Y=null, Z=null) { X*5 } which in out case is roughly equivalent to F(X) X*5
	echo([1,2,3].map(X*5))
	# Output: [5,10,15]

	# "Key:" + X is same as F(X=null, Y=null, Z=null) { "Key:" + X }
	echo({"a": "one", "b": "two"}.map("Key:" + X))
	# Output: ['Key:a','Key:b']

	echo({"a": "one", "b": "two"}.map("Val:" + Y))
	# Output: ['Val:one','Val:two']

	echo([1,2,3,11,12].count(X>10))
	# Output: 2

If the special variables X, Y, or Z are found in a string interpolation, such as `"Key $X, Value $Y"`, the string is wrapped in anonymous function as described above.

	echo({"a": 1, "b": 2}.map("Key $X, Value $Y"))
	# Output: ['Key a, Value 1','Key b, Value 2']

## Anonymous function (method) literal using magical A, B or C variables

Additional syntactic sugar for anonymous function is `{ ... }`. This is equivalent to `F(A=null, B=null, C=null) { ... }`.

	# X*5 + 1 would not work as X*5 itself would be anonymous function
	echo([1,2,3].map({ A*5 + 1 }))
	# Output: [6,11,16]

## Returning values from methods

Method evaluates to the last expression, as in Lisp and Ruby. Additionally following expressions allow immediate return from a method.

* `return` - returns `null`
* `return EXPR` - returns the value of EXPR
* `COND returns` - if COND evaluates to true, returns `null`
* `COND returns EXPR` - if COND evaluates to true, returns the value of EXPR

Examples of `return`, `returns` and last expression value as result of a method.

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

## Returning from inner method

Use `Return` type to return from inner methods.

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

## Referencing specially named methods

Since identifiers are alphanumeric (with underscores), `my_method` can be referenced simple by name, while `+` for example can not. Here is example of referencing specially named methods:

	# (+) references the method +
	F sum(something:Eachable1) something.reduce(0, (+))

# Types

NGS is a dynamically typed language: values (and not variables) have types.

	a = 1
	a = "ok"
	# 'a' had value of one type and then value of another type

NGS is a "strongly typed" language: values are not implicitly converted to unrelated types. This makes the language more verbose in some cases but helps catch certain type of bugs earlier.

	echo(1+"2")
	# ... Exception of type MethodNotFound occured ...
	# That means that NGS has no method that "knows" how to add an Int and an Str

	echo(1+Int("2"))
	# Output: 3

NGS has several pre-defined types. A programmer can define additional types. Types roughly correspond to classes in other languages (Python, Ruby, Java) but without defined fields and methods.

## Creating object of given type

Creating object of a given type is calling the type. Syntactically, it's type name followed by parenthesis which optionally contain arguments.

	# Create object of type Path
	p = Path("/")

	# Create EmptyBox object
	eb = EmptyBox()

## Defining your own types

Type definition is only supported in code syntax.

	# Switch to code syntax inside { ... }. "type" currently does not work in command syntax
	{
		# Define type
		type Vehicle

		# Define sub-type
		type Car(Vehicle)

		type RedThing

		# Multiple inheritance
		type RedCar([Car, RedThing])
	}
	echo(Vehicle)
	echo(Car)
	echo(RedCar.parents)

	# Output:
	#   <Type Vehicle>
	#   <Type Car>
	#   [<Type Car>,<Type RedThing>]

## Type constructors and object creation

* When type definition (`type T`) is executed, NGS defines the `.constructors` MultiMethod that includes one method. This method creates object of type `T`. The type of this method is `NormalTypeConstructor`.
* After defining a type `T`, one can add methods to `.constructors` using `F T(...) {...}` definitions. If you add your method to the `.constructors` field, please make sure it returns an object of type `T` or object of a type that is a subtype of `T`. Otherwise, it will be surprising for the callers.
* When you call a user defined type, its `.constructors` MultiMethod is called.
* If the default constructor added by NGS (method of type `NormalTypeConstructor`) gets executed, it does the following:
	* Creates object of type `T`
	* Runs `init()` MultiMethod with the new object as the first argument and the arguments of the call to `T(...)` as second and on arguments to `init(...)`.
	* Note that calling `init()` might trigger `MethodNotFound` exception. It is ignored if and only if the call to `T()` had no arguments.

Examples:

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

# Loading and running additional code

## require

The `require` method reads, compiles (to bytecode) and executes the given file. Currently absolute paths and paths relative to current directory are supported. In future, paths relative to current directory will not be supported but rather paths relative to the file that does `require()` call will be supported, similar to Node.js. This needs work which was not done yet, there was never intention to support paths relative to current directory; it's just wrong.

	require('/home/someone/my_module.ngs')

`require` returns the value of last expression in the file. This feature plays nice with namespaces (see below) in cases when the whole file is an `ns { ... }` expression.

## Auto-loading

When an undefined global variable is referenced, `global_not_found_handler` is called. This handler tries to load a module (currently from the `lib/autoload` directory) that will define the referenced variable. Here are some modules that are autoloaded and additional variables/methods they define.

* AWS
* IP (IPAddr, IPNet)
* Iter (ArrIter, ConstIter, HashIter, RangeIter, ...)

# Namespaces

Namespaces are used to prevent names collisions and hide methods. One of the main usages for namespaces is in modules.

Let's consider a file named `my_ns.ngs`:

	ns {

		# This method is only visible to other methods in the namespace
		F _private_helper_method() {
			...
		}

		F accessible_method() {
			...
			_private_helper_method()
			...
		}

		global global_method
		F global_method() {
			...
			_private_helper_method()
			...
		}
	}

Another file, `example.ngs` could be using the above namespace utilizing the `require` method:

	arbitrary_ns_name = require('my_ns.ngs')

	global_method()

	# :: is namespace member access operator
	arbitrary_ns_name::accessible_method()

## How namespace definition works

Namespace definition works in both commands and code syntax.

The `ns { ... }` definition is equivalent to the following code:

	F() {
		_exports = {}

		....

		_exports
	}() # Call as soon as the method is defined

Inside the `F() { ... }`, all variables and methods which do not start with underscore (`_`) are automatically added to the `_exports` hash. The underscore exclusion prevents from `_exports` itself to be added to the hash. This implementation also allows arbitrary manipulations of the returned value.

	# The simple case
	ngs -pi 'ns { F func() 7; }'
	Hash of size 1
	  [func] = <MultiMethod with 1 method(s)>

	# Prefix all exported methods and variables with "my_"
	ngs -pi 'ns { F func() 7; _exports .= mapk("my_$X") }'
	Hash of size 1
	  [my_func] = <MultiMethod with 1 method(s)>

	# Make "ns" return arbitrary value
	$ ngs -pi 'ns {_exports=7}'
	Int: 7

	# Show exclusion of underscore prefixed items
	$ ngs -pi 'ns { F visible() 7; F _invisible() 7; }'
	Hash of size 1
	  [visible] = <MultiMethod with 1 method(s)>

	# Not only methods but also variables are exported
	$ ngs -pi 'ns { visible=1; _invisible=2; }'
	Hash of size 1
	  [visible] = 1

## Different names for external variables in a namespace

Namespaces also support additional syntax `ns(PARAMETERS) { ... }`. This syntax is converted to `F(PARAMETERS) { ... }()`. This allows referencing external variables by another names. Inside the namespace one can now define variables and methods which would shadow the global (or upvar) variables and still access them.

Here is example from `lib/autoload/AWS.ngs` :

	ns(GlobalRes=Res, GlobalResDef=ResDef, global_test=test) {
		...
		doc AWS resource
		type Res(GlobalRes)
		...
		doc AWS Resource definition
		type ResDef(GlobalResDef)
		...
	}

Note that when `ns(PARAMETERS) { ... }` syntax is converted to `F(PARAMETERS) { ... }`, the method is called without any arguments so all parameters must have default values or an exception will occur:

	$ ngs -pi 'ns(a) { 7 }'
	... Exception of type ArgsMismatch occurred

## Planned future changes to namespaces

The `ns { ... }` construct will probably not return a `Hash` object but object of a new type, `Namespace` which will probably be subtype of `Hash`. In any case, the `::` operator will be defined in such way that `my_namspace::my_method()` will continue to work.


# Frequently used types and methods

This section is not a reference but rather an overview. It includes the most important types and methods. For full documentation regarding types and methods see the generated documentation.

## Null

`null` signifies absence of data. `null` is the only value of type `Null`.

	echo("abcd".pos("c"))
	echo("abcd".pos("x"))
	# Output:
	#   2
	#   null

	if null echo("if null")
	# No output, null converts to false

## Bool

The type `Bool` represents a boolean. The values `true` and `false` are the only values of type `Bool`.

	if true echo("if true")
	if false echo("if false")
	# Output:
	#   if true

	echo(1 == 1)
	# Output:
	#   true

	echo(1 == 2)
	# Output:
	#   false

	echo(true and false)
	# Output:
	#   false

	echo(true or false)
	# Output:
	#   true

When a boolean value is needed, such as in `if EXPR {...}`, the `EXPR` is converted to boolean by calling `Bool(EXPR)`.
NGS defines `Bool` multimethod (technically `Bool` type constructors), with methods for many types. These methods cause the following values to be converted to false:

* 0
* null
* false
* empty array
* empty hash
* empty string
* an EmptyBox object, for example `[1,2,3].Box(10)` (which boxes the element at index 10)
* a Failure object, for example `Result({ 1 / 0 })`
* regular expression that did not match, for example `"abc" ~ /XYZ/`

Unless specified otherwise, all other values are converted to `true`.

To define how your user-defined types behave as boolean, define `Bool(x:YOUR_TYPE)` method.

## Int

`Int` represents integer numbers

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

## Str

`Str` represents string of bytes.

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

## Arr

`Arr` represents an ordered list which can be accessed by zero-based index. Currently `Arr` is implemented as consecutive memory locations.

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

## Hash

`Hash` represents a mapping between keys and values. `Hash` is implemented as a hash table.

Hashes - basics

	x = {"a": 1, "b": 2}
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

	x = %{akey avalue bkey bvalue}
	echo(x)
	# Output:
	#   {akey=avalue, bkey=bvalue}

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

# Higher-order functions (methods)

This section is an overview. The purpose is to give overall impression of how higher-order functions feel in NGS. For full documentation regarding methods (including higher-order functions) see the generated documentation.

Higher order functions are functions that get another function as a parameter.

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


# Exceptions

Similar to other languages (such as Python, Ruby, Java, etc), NGS has exceptions which can be thrown and caught.

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

The `catch` clauses are implemented using multimethod. If an exception occurs, the `catch` multimethod is called with a single argument, the exception. Unlike all other situations, the search of a method that matches the exception is done from the beginning to the end of methods list.

Implementation of `catch` using multimethod prevents using `return` inside `catch` clauses in the expected way. That's why this implementation might change in the future.

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

## Default value for expression that causes exception

The short-circuit binary operator `tor` (try-or) allows specifying default value for an expression that might throw an exception.

	# .backtrace might not exist, .frames might not exist
	# Accessing non-existent fields using the . notation will cause KeyNotFound exception
	parent_frames = parent.backtrace.frames tor []

`EXPR tor null` is functionally equivalent to `try EXPR`.

## Alternative mechanism for handling exceptions

See generated documentation for the `Result` type.

# Flow control

## If

If works in both commands and code syntax.

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

## Loops

Loops work in both commands and code syntax.


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

## Switch and switch-like expressions

Switch and switch-like expressions work in both commands and code syntax.

* The first match activates the related code, there is no fall-through.
* The value resulting from executing the code is the result of the switch-like expression.
* If there is no match, `switch`, `match` and `cond` return `null`.
* If there is no match, `eswitch`, `ematch` and `econd` throw `SwitchFail` exception.

Switch and switch-like expressions examples.

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


# Regular expressions

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

# Collector facility

Collector facility assists in building lists, hashes and other data structures. Collector facility should be used where `map` and other functional facilities are not meeting the requirements or when using collector is cleaner. Typically, the code of the following forms is replaced by collector (or some mix or something similar to the following forms):

	# --- 1 ---
	my_arr = []
	for ... {
		my_arr.push(my_something_one)
		if ... {
			my_arr.push(my_something_two)
		}
	}

	# --- 2 ---
	my_arr = []
	my_arr.push(my_something_one)
	for ... {
		if ... {
			my_arr.push(my_something_two)
		}
	}
	my_arr.push(my_something_three)


The `collector` keyword wraps the following expression (or expressions if they are grouped using `{...}` syntax) in a function with single argument, `collect`.

`collector` keyword can be followed by slash (`/`) and initial expression, which defaults to an empty array.

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

	# Code from stdlib:
	F flatten(arr:Arr)
		collector
			arr.each(F(subarr) {
				subarr.each(collect)
			})

## Customizing collector facility

`collector` keyword is implemented as a call to `collector` method with two arguments: the initial value (which defaults to empty array) and the wrapped code, which is passed as a method.

By defining `collector` method, the programmer can customize the behaviour. Here is implementation of collector for `Arr`.

	# Code from stdlib:
	F collector(a:Arr, body:Fun) {
		body(F(elt) a.push(elt))
		a
	}

Here `a` gets the initial value, `body` is the wrapped code that comes after the `collector` keyword, the anonymous function that is passed to `body` becomes the `collect` function inside the wrapped code.

Here is `collector` for `Hash` and it's usage example:

	# Code from stdlib:

	F collector(h:Hash, body:Fun) {
		body(F(k, v) h[k] = v)
		h
	}

	F mapk(h:Hash, mapper:Fun)
		collector/{}
			h.each(F(k, v) collect(mapper(k), v))

# Running external programs

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


# Assignment shortcuts

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

I do not recommend NGS as your first language. Python, for example, would be a much better choice. Programming experience is assumed prior to using NGS.

## Watch the version

NGS is under development. Breaking changes can happen. If you do anything important with NGS, it's preferable to note the git revision you are using for reproducible installation. The plan is to stop breaking NGS when it reaches version 1.0.0. From that version, the behaviour will be common - patch level increase for fixes, minor for improvements, major for breaking changes.

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

Comments syntax is implemented in many places but not everywhere. If you get syntax error regarding comment, move it to somewhere nearby.

## Mutable default parameter gotchas

The code below will probably not do what was intended. Note that the default parameter value is only computed once, at method definition time.

	F f(x, a:Arr=[]) {
		a.push(x)
		a
	}

	echo(f(10))  # [10]
	echo(f(20))  # [10,20]

Same happens in Python and hence already described: http://docs.python-guide.org/en/latest/writing/gotchas/#mutable-default-arguments

# Handlers

Handlers are called by NGS when a certain condition occurs.

**A handler** is a `MultiMethod`. Like with any other multimethod, you can override what it does by defining your own method with the same name further down in the code. Since standard handlers are defined in **stdlib.ngs** which is typically loaded first, your own methods will be "further down".

## method_not_found_handler

`method_not_found_handler` is called when a multimethod was called but no method matched the arguments. Use `F method_not_found_handler(callable:Fun, *args) ...` to add your behaviours.

## global_not_found_handler

`global_not_found_handler` is called on attempt to read from an undefined global variable. Sample usage from **stdlib.ngs**

	F global_not_found_handler(name:Str) {
		require("${NGS_DIR}/autoload/${name}.ngs")
	}

# Hooks

**A hook** is a `Hook` object. Some hooks are called by NGS when a certain condition occurs. You are free to create and use your own hooks. When called, it executes all registered methods. The main difference vs calling a `MultiMethod` is that using hook you get accumulative behaviour instead of overriding behaviour.

User-defined hook example:

	{
		h = Hook()
		h.push({ echo("A") })
		h.push({ echo("B") })
		h()
	}
	# Outputs one per line: A, B

Another way is to add named hook handlers (also a practical example):

	exit_hook['cleanup_temp_files'] = F(exit:Exit) {
		# remove my temp files
	}

## exit_hook

`exit_hook` is called when NGS is about to exit. Typical cases are:

* All of the given code was executed.
* `throw` was invoked and no matching `catch` was found.

Method signature: `exit_hook(exit:Exit)`. `Exit` currently has two keys: `exit_code` (`Int`) and `exceptions` (`Arr`). **stdlib.ngs** defines one standard exit hook.

	$ ngs -pi 'exit_hook.Hash()'
	Hash of size 1
	  [print_exception] = <UserDefinedMethod <anonymous>(exit:Exit) at /usr/local/lib/ngs/stdlib.ngs:5345>

* `print_exception` prints exception details if an exception occurred.

# Predicates

Methods such as `filter`, `filterk`, `filterv`, etc take predicate as one of the arguments.
Traditionally, such methods (functions) took a method (function) as the predicate argument.
A predicate function is a function with one parameter and that returns a boolean.

NGS allows more powerful and convenient usage of predicates in these and other methods.
All kinds of objects can be passed as predicates.
NGS higher-order methods (functions) that have predicate parameters convert the predicates into predicate methods (functions) using the `Pred` multimethod:

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

This behaviour allows defining how any given type behaves as a predicate. Several built-in types have `Pred` method defined for them.
Here are some examples of how predicate conversion described above can shorten the code:


	# Would be {...}.filterk(F(k) k ~ /^b/)
	{"a1": 1, "a2": 2, "b1": 10}.filterk(/^b/)  # {"b1": 10}

	# Would be {...}.filter(F(elt) elt.x == 7)
	[{"x": 10, "y": 20}, {"x": 7, "y": 30}].filter({"x": 7})  # [{x=7, y=30}]

	# Would be {...}.filter(F(t) t is Str)
	["abc", 1, "def"].filter(Str)  # [abc,def]

One can easily define how a type behaves as a predicate: define appropriate `Pred` method.
Here is the `Pred` method that allows `.filter(SomeType)` such as `.filter(Str)` example above:

	F Pred(t:Type) F is_pred(x) x is t

That's a method (`Pred`) that returns a predicate method (function) which is called `is_pred`.
Naming the returned method helps a bit when looking at backtrace. `is_pred`, the predicate method, checks whether it's argument is of type `t`.

Note that using a method as predicate is still possible:

	[1,2,11].filter(F(x) x > 10)  # [11]

That is because `Pred` method leaves it's argument as is, if it is callable (a method for example):

	F Pred(f:Fun) f

# Iterators

Iterators give more flexibility and control as opposed to `each` iteration.

## The iterator protocol

Iterators must implement the following methods:

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
* In **code syntax**: ```` ``...`` ```` - capture and decode the output

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

`EXPR` above can be a single expression or any number expressions, new-line or `;` separated, value of the last one is used for the substitution.

The examples above show `$()` syntax but the substitution works the same for all syntaxes mentioned in **syntactic options for executing external programs**.

## Options

Rationale: it is sometimes desirable to alter the behaviour of execution of an external program.
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

Running external programs examples:

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

When converting to `Bool`, for example in the expression `if $(test -f my_file) { ... }`, all programs in the `CommandsPipeline` must exit with code zero in order to get `true`, otherwise the result of converting to `Bool` is `false`.

## Constructing command line arguments

While it's feasible to construct an array with command line arguments for a program, `Argv` makes this task much easier by supporting common cases such as omitting a command line switch which has no corresponding value to use.

See `Argv` multimethod.

## Globbing

Globbing is not implemented yet.

# Accessing environment variables

On NGS startup, the global variable `ENV` is populated with the environment variables. `ENV` is a `Hash` object.

	echo(ENV.HOME)
	echo(ENV["HO" + "ME"])

`ENV` is also passed to external programs.

	{
		ENV.HOME = "/home/fake"
		$(my_prog)
	}

# NGS exit codes

Both regular results of running a program and exceptions are converted to exit code using `ExitCode` MultiMethod.

Built in methods of `ExitCode` do the following conversion:

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
* Non-Exception was thrown and then caught by C code - 245
* NGS could not find the bootstrap file - 246
* NGS could not open bootstrap file - 247
* NGS could not parse bootstrap file - 248

## Customizing translation of your types/objects to exit code

Define `F ExitCode(t:MyType) {...}` method that returns `Int` to customize the exit code.

# Declarative primitives

Some of NGS libraries (currently only AWS) implement declarative primitives approach. Declarative primitives approach is to expose functions to manipulate resources in an idempotent way. Similar to configuration management systems, exposed resources are manipulated by declaring their desired state. Unlike configuration management systems, this approach does not deal with dependencies and order resolution. Declarative primitives approach provides convenient scripting facility, not a configuration management system.

## Related methods

**Constructors**

Resources references are created using constructors, such as `AWS::Vpc`. Resources references are of type which is a subtype of `ResDef`.

	my_vpc = AWS::Vpc()                # Reference to all VPCs that can be fetched using AWS CLI
	echo(my_vpc)                       # Output: <Aws::Vpc anchor={regions=null, Tags={}}>
	echo(AWS::Vpc().typeof())          # Output: <Type Vpc>
	echo(AWS::Vpc().typeof().parents)  # Output: [<Type ResDef>]

**find**

`find` - find the specified resources. Almost never should be called manually. `expect`, `converge`, `each` and other methods run `find` automatically if needed (if it did not run previously on the reference).

	my_vpc = AWS::Vpc().find()
	echo(my_vpc)
	# Output:
	#   <Aws::Vpc vpc-ef...,vpc-ef...,vpc-bf... anchor={regions=null, Tags={}}>

Empty result is not an exception for `find`.

**expect**

`expect` - check that referenced resources exist. Throw an exception otherwise. Used to assert assumptions about the system. If you write a script that for example counts on exactly one VPC that matches your description to exist, I strongly recommend to be explicit and state your expectation as `.expect(1)`.

	my_vpc = AWS::Vpc().expect() # If no VPCs exist - throws exception
	echo(my_vpc)
	# Output:
	#   <Aws::Vpc vpc-ef...,vpc-ef...,vpc-bf... anchor={regions=null, Tags={}}>

	# Find VPCs with name tag "XX"
	my_vpc = AWS::Vpc({"Name": "XX"}).expect() # If no VPCs exist - throws exception
	# ... Exception of type AssertFail occurred ...

	# Find VPCs with the given tag
	my_vpc = AWS::Vpc({"aws:cloudformation:stack-name": "edge-vpc"}).expect(1)
	# Would throw exception if there was not exactly one resources matching the description
	echo(my_vpc)
	# Output:
	#   <Aws::Vpc vpc-ef... anchor={regions=null, Tags={aws:cloudformation:stack-name=edge-vpc}}>

**converge**

`converge` - Creates or updates resources if needed in order to match given properties.

Example from production code, shortened for brevity:

	# Library that describes ELB creation. The system has several ELBs which are similarly configured.
	# The library contains this common configuration.
	ns(c=converge) {
		F converge(env, role, dns=null) {
			edge_vpc_anchor = {'aws:cloudformation:stack-name': 'edge-vpc'}
			vpc = AWS::Vpc(edge_vpc_anchor).expect(1)
			tags = {'env': env, 'role': role, 'creator': 'system/images/elb.ngs'}

			# Anchor: name + vpc_id
			elb_sg = AWS::SecGroup("https-server", vpc)
			elb = AWS::Elb("${env}-${role}")

			# ---> Converge happens here <---
			# Load balancer is either created or updated here
			elb.c(
				Tags = tags + if dns { {'DNS': dns} } else { {} },
				ListenerDescriptions = ...,
				Subnets = AWS::Subnet(edge_vpc_anchor).expect(2),
				SecurityGroups = elb_sg,
				HealthCheck = ...,
				Instances = AWS::Instance({'env': env, 'role': role}).expect()
			)
		}
	}

**delete**

`delete` deletes referenced resources.

	# Delete referenced resources, in this case one specific load balancer
	AWS::Elb("prod-mobile-redirect").delete()

	# Delete all unused load balancers
	AWS::Elb().reject(X.Instances).delete()

## Anchor

Anchors describe which resources are referenced.

As you probably have noticed, resource references, which are created using constructors, get some parameters. These parameters are called "Anchor". Note that all parameters passed to the constructors are called "Anchor" collectively.

Examples of Anchors:

* Tags hash: `AWS::Instance({'env': env, 'role': role})`
* Explicit tags: `AWS::Instance(Tags={'env': 'prod', 'role': 'ssh-sandbox'})`
* Resources specific properties: `AWS::Vpc(IsDefault=true).expect(1)`
* Tags and resource-specific properties: `AWS::Vpc(IsDefault=false, Tags=...).expect(1)`

Any combination of explicit tags and resource-specific properties is possible.

## Properties

Properties describe the desired state of resources. The parameters to `converge` are properties. See the `converge` example above.

## Planned future changes

* `regions` in AWS library will probably be renamed to `_Region` or something else starting with underscore to emphasize it's a syntactic property, not something that correlates to AWS CLI output.
* Currently there is no way to know whether `converge` has created resources or updated them. There will be a way to tell.

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
