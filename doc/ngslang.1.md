% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngslang - Next Generation Shell language overview.

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

# LANGUAGE GOTCHAS - READ FIRST

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

Most language syntax constructs are actually method calls. For example `1 + 2` is `+(1,2)` . The method named `+` is called with 1 and 2 as arguments. This means you can customize what a `+` does when applied to your types. Since types are "open", you can also define what a `+` means for existing types too.

Backwards array scanning allows later code (your code as opposed to previously loaded library for example) to override the behaviour. This override can be narrowed to specific cases using the `guard` clause. Continuing the example above:

	F bigger(x:Int) {
		guard x >= 100
		x + 10
	}
	echo(bigger(1))
	echo(bigger(100))

	# Outputs one per line: 2, 110

When the condition following the `guard` clause is true, the execution continues. When the condition is false, method implementation execution is terminated and the search in the array of method implementations continues as if this implementation did not match the types of the arguments. I recommend not to cause any side effects inside the method implementation in statements above `guard`, if you have any.

# LANGUAGE HANDLERS AND HOOKS

## `impl_not_found_handler`

`impl_not_found_handler` is called when a method was called but no **method implementation** matched the arguments. Use `F impl_not_found_handler(callable:Fun, *args) ...` to add your behaviours.


## `global_not_found_handler`

`global_not_found_handler` is called on attempt to read from an undefined global variable. Sample usage from **stdlib.ngs**

	F global_not_found_handler(name:Str) {
		require("${NGS_DIR}/autoload/${name}.ngs")
	}


## `exit_hook`

`exit_hook` is called when `throw` is invoked and no matching `catch` is found. Method signature: `exit_hook(exit_info:Hash)`. `exit_info` currently has two keys: `exit_code` and `exception`. **stdlib.ngs** defines two standard hooks.

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

