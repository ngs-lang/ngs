% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngslang - Next Generation Shell language overview.

# LANGUAGE PRINCIPLES OVERVIEW

* Systems engineers language - NGS is a domain-specific language. It is aimed to solve system tasks in a convenient manner.

* Do the most practical thing
	* `read('myfile.json')` will parse the JSON and return the data structure.
	* The ```` ``my_command`` ```` will parse the command output (JSON for example) and return the data structure.
	* `my_array.my_prop` returns an array of `.my_prop` properties of each element.

* Uniformity unless contradicted by the above rule or seems to be too much work, which results will not be used.

* Power. Trade-offs between power and not allowing to shoot yourself in the foot usually produce the power solution. The language is aimed at experienced engineers which use their own judgement. The language should be powerful enough to shoot all feet in the building at once.

* Simple methods naming for less guess work. `1+2` adds the numbers (method name `+`), `arr1+arr2` adds (concatenates) arrays and `hash1+hash2` produces merged hash.

* Extensibility
	* `read('your_file.super-format')` can be extended to parse your format.
	* `fetch`, which reads from a file, can be extended to support HTTP or S3.
	* Define any operator on your custom types or for existing types.

* Simplicity
	* No classes. Only types, methods and multiple dispatch (picking the right method implementation by matching types of parameters and arguments)
	* Simple type system.

* Familiarity - many concepts and syntax constructs come from other languages

# LANGUAGE OVERVIEW AND GENERAL FEEL

* The language is functional in a sense that:

	* A function is a first class citizen, which can for example be assigned to a variable and passed as a function argument to another function. Example:

			doc Make a new function that given an argument,
			doc will apply the original function f twice to the argument x
			doc f - original function
			F twice(f)
				F(x) f(f(x))

			F f(x) x+3

			g = twice(f)

			echo(g(7))

			# Output: 13

	* Standard library defines many [higher-order-functions](https://en.wikipedia.org/wiki/Higher-order_function) such as `each`, `map`, `any`, `all`, etc.

* Most of the language operators, such as `+`, are really a function calls. This gives uniformity, ability to pass an operator as a function to a higher-order-function and customizability.

		echo([1, 2, 3].map((+), 1))
		# Outputs: [2,3,4]

		# 1+1 is same as (+)(1, 1)

* Types have constructors. To minimize the "how I create a Hash" searches for example, `Hash` has many constructors so the first place and most probably answer is that you should look at [`Hash`](ngstyp.1.html#Hash) type documentation and not search for an unknown function. Example is a `Hash(arr, cb)` which has an array and a callback as arguments and builds a `Hash` where the keys come from the given array and the values are calculated by the callback.

		echo([1, 2, 3].Hash(F(elt) "ab" * elt))
		# Outputs: {1=ab, 2=abab, 3=ababab}


# LANGUAGE GOTCHAS

This section will be expanded as I get feedback :)

## NGS should not be your first language

I do not recommend NGS as your first language. Python for example would be a much better choice. Programming experience is assumed prior to using NGS.

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

* `print_exception` prints exception details if an exception occured.
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

