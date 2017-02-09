% NGSLANG(1) NGS User Manual
% Ilya Sher
% 2016

# NAME

ngsint - Next Generation Shell language internals.

# ARCHITECTURE OVERVIEW

NGS consists of three main parts:

* Language core, written in C
* Language standard library that extends NGS, written in NGS
* Shell, written in NGS (not done yet)

## Minimal language core

Language core should be as minimalistic as possible: language parsing, compilation, vm and a bit of run-time. I assume optimizations which might be done later will collide with this principle.

In order to allow the core to be small, C functions have relatively thin wrappers. C functions that have wrappers that only convert input and output data types are exposed as NGS' `c_...` functions (for example `c_read`, `c_write`, `c_kill`). These are used in standard library which wraps these functions to make them more high-level and more practical to use.When `c_...` exposed, argument names should match the ones specified in `man` for that C function.

For good example of exposing and wrapping `c_...` functions see `autoload/Thread.ngs` which wraps `c_pthread_attr_t`, `c_pthread_attr_init`, `c_pthread_create` and `c_pthread_join`. 

# GARBAGE COLLECTION

To minimize my efforts I use external library ( http://www.hboehm.info/gc/ ) for garbage collection. Please use `NGS_MALLOC` for your objects and `NGS_MALLOC_ATOMIC` in cases when your data can not contain pointers to another `NGS_MALLOC`ed structures.

# COMPILATION

(WIP)

NGS parses the sources to AST and then compiles to bytecode. Compilation is done in `compile.c`. Functions of interest:

## compile()

	char *compile(ast_node *node, char *source_file_name, size_t *len)

* `ast_node *node` - top-level AST node
* `char *source_file_name` - name of the compiled file


# BYTECODE

(WIP)

`BYTECODE_HANDLE` typedef'ed struct. Although reading/writing bytecode to a file is not implemented yet, bytecode layout in memory was designed to be easy to write to or read from a file.

`ngs_add_bytecode_section()` function

# VM

NGS runs a [stack-based VM](https://en.wikipedia.org/wiki/Stack_machine).

`VM` typedef'ed struct represents the VM.

`CTX` typedef'ed struct represents an ngs-thread of execution. Currently only one ngs-thread runs per thread. This might change in future if green threads are implemented.

The unusual solution is that call frames are not allocated on the stack. It simplifies creating of closures later as stack data should not be copied to heap when a closure is created.


# ADDING NEW BUILT-IN TYPE 

This is procedure for adding ordinary (such as for `DIR` type used in `opendir()` and friends) built-in (native) type

In `obj.h` - add something along these lines:

	typedef struct {
		OBJECT base;         // must be present and
		                     // must be the first member of the structure
		VALUE my_field_1;    // free style fields here and below
		VALUE my_field_2;
		size_t my_data_len;
		char *my_data;
	} MY_OBJECT;


On `obj.h` - add your type in the `IMMEDIATE_TYPE` enum:

	...
	T_MYTYPE = (INT_ON_LINE_ABOVE_PLUS_ONE << 8) + T_OBJ
	...

On `obj.h` - add your `IS_...` macro.

	#define IS_MYTYPE(v) ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_MYTYPE)

In `obj.c` - add `make_...()` function similar to:

	VALUE make_mytype(...) {
		VALUE v;
		DIR_OBJECT *tmp = NGS_MALLOC(sizeof(*tmp));
		tmp->base.type.num = T_MYTYPE;
		...
		return v;
	}

On `obj.h` - add `make_...()` function declaration similar to:

	VALUE make_mytype(...);

In `vm.h` - in typedef `VM` add

	NGS_TYPE *MyType;

In `vm.c` - add

	MK_BUILTIN_TYPE(MyType, T_MYTYPE);

# ADDING NEW BUILT-IN FUNCTION

Typical native built-in function in `vm.c` looks like this:

	METHOD_RESULT your_function_name METHOD_PARAMS {
		... Use argv[] arguments ...
		METHOD_RETURN(ret);
	}

TODO
