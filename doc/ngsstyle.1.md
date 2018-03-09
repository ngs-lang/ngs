% NGSSTYLE(1) NGS User Manual
% Ilya Sher
% 2018

# NAME

ngsstyle - Next Generation Shell language style guide.

# Empty arrays vs null

Prefer empty arrays to `null`. This will avoid wrapping `my_arr.each(...)` in `if my_arr { ... }`.

Bad:

	F my_func() {
		# Returns null or an array
	}    

	...
	my_arr = my_func()

	if my_arr {
		my_arr.each(...)
	}

Good:

	F my_func() {
		# Always returns an array, potentially empty
	}    

	...

	my_func.each(...)

# Anonymous function that only calls another function

Bad:

    my_arr.map(F(x) my_func(x))

Good:

    my_arr.map(my_func)

When I see the "Bad" variant, I assume one of the following:

* The author wanted to explicitly name the elements (`x`). Good name for `my_arr` eliminates the need for naming elements.
* The author does not understand how to use the `map` function

# Functional operators

Avoid using the "functional operators". They are confusing to the reader.

Bad:

	my_list / my_func
	my_list ? my_func
	my_list % my_func
	my_val \ my_func

Good:

	my_list.map(my_func)
	my_list.filter(my_func)
	my_list.each(my_func)
	my_func(my_val)
