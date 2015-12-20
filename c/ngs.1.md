% NGS(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngs - Next Generation Shell. This documentation is work in progress. Most of it covers yet unimplemented parts. This documentation was mostly applicable to previous implementation (in Lisp). Current implementation is a rewrite (now in C) and it's in early stages.

# SYNOPSIS

ngs *script_name* \
ngs [**-e**|**-E**] *expression*

Temporary, run as **./ngs <** *your_script*

# DESCRIPTION

**ngs** is a Next Generation Shell. It has two main parts: the language and the interactive shell.

This project contains the language part which is under development. This manual covers both implemented and unimplemented parts.

# MOTIVATION

The circumstances have changed greatly since the development of the conventional shells such as **bash**. Conventional shells are not fit for the job anymore. Here I will point out things that can be improved and suggest better (IMHO) alternatives.

## Syntax

Since conventional shells were first designed just to run commands and only later extended to be a programming languages while keeping syntax compatibility, the syntax is inconvenient.
Examples:

* bash - **while some command;do ... done**. NGS - **while $(some command) { ... }**
* bash - while running **my_command \$var**, the number of arguments passed to **my_command** is not known from syntax due to expansion rules. bash - **my_command "\$var"**, ngs - **my_command \$var**. bash - **my_command \$var**, ngs - **my_command \$\*var** (zero or more arguments, equals to the number of elements in the **\$var** array).

## Language design

* For example, one can not simply set a variable defined outside while with code inside the while (because of sub-shell): **a=1; cat /dev/null | while true;do a=2;break; done; echo $a** - prints **1**.

## Cloud

Bash was meant to manage one machine.

* There is no built-in support for managing multiple servers.
* There is no convenient way to work with cloud services APIs. This is mostly due to unsupported nested data structures.

## Inspired by

The language was inspired by Python, JavaScript, Ruby. Also lifted good parts from bash.

# OPTIONS

Given *script_name* runs the script.\
Using *expression* is equivalent to running a script that consists of **{** *expression* **}**. **-e** loads stdlib.ngs before evaluating the expression, **-E** skips the loading of stdlib.ngs.

# EXIT CODE

The exit code is whatever the running code (*script_name* or *expression*) returns using the following conversions:

## Int (integer)
A Number is used as exit code (modulo 256). For example, **ngs -E 10** returns exit code 10.

## Bool (boolean)

**true** results 0, **false** results 1.

## T (user-defined type)

The exit code can be customized by **to_exit_code(x:T)** method.

## Anything else, including user-defined types without **to_exit_code(x:T)** method.

Exit code 0.

# ENVIRONMENT


# FILES


# LANGUAGE OVERVIEW

## Principles

* Do the most practical thing
	* **read('myfile.json')** will parse the JSON and return the data structure. If you read a JSON file, most probably you want to look at the data, not handle it as a binary or a string.
	* If you run a command with \`\`my\_command\`\`, (which is a syntax for run and parse output) and it returns JSON, the expression evaluates to the parsed data structure.
	* If you **map** or **each** a hash, the callback function will be called with two arguments: key and value.
	* **my\_array.my\_prop** does **array.map(F(elem) elem.my\_prop)** because that's probably what you would like to get when using **.my\_prop** on array of hashes, get that property of each element of the array. Kind of **jq .[].my\_prop**.

* Have types x methods matrix.
	* The idea here is that the programmer will have to minimize the guess work about the names of methods (functions). Same method should work on maximum number of types. Example: if you have **1+2** which adds the numbers, you will have **arr1+arr2** which will add (concatenate) the arrays, rather than **arr1.concat(arr2)** and **hash1+hash2** will produce merged hash rather than **hash1.merge(hash2)**.
	* If you have a data type and a method that can do remotely expected thing on that data type, it should work. Example of "remotely" expected is: **number.map(mapper)** runs the **mapper** function for each number in range of 0 to the *number* - 1.

* Extensibility.
	* Have custom data format? There is no reason that **read('your\_file.super-format')** will not return the parsed data structure.
	* Have your custom software that returns non-zero exit code but it's OK? Extend NGS so that running that command will not produce a fatal error. NGS always runs in a mode that is equivalent to **bash -e**, saving many tears.
	* Want **read('http://example.com/my-resource')** to work? Extend NGS to be able to fetch HTTP or S3.
	* Define any operator on your custom data types.

* Consistent syntax.
	* No more **do ... done** and **if ... fi** in the same language. Blocks are **{ ... }**.
	* If you can call a function, **my\_func(\*args)** and have "splat" effect (**args** array becomes several arguments when calling the function), there is no reason why it would not work when you create an array: **[1, 2, \*args, 3, 4]**, same for **\*\*kwargs** and **{my\_hash\_default: 7, \*\*user\_supplied\_options, my\_override: 8}**.

* Short syntax for common cases. **F (a,b) a+b** is equivalent to JavaScript's **function(a,b) { return a+b; }** (last expression's value is returned, like in Ruby).

* Syntax sugar for common cases.
	* **@ expr** is equivalent to **F(X=null, Y=null, Z=null) expr**. So if you want to pass a function that just adds to numbers this is how you can do it: **my\_super\_two\_arrays\_mapper(@X+Y)**.
	* **expr1 @ expr2** is equivalent to **expr1.map(F(X=null, Y=null, Z=null) expr2)**. Producing a new array with each element mapped to plus one of the original is hence **old\_array @ X+1**. Another example with using **number.map()** to build an array with element of 0 to 9 would be **10 @ X**
	* **expr1 @? expr2** is equivalent to **expr1.filter(F(X=null, Y=null, Z=null) expr2)**.
	* **for(i;n) ...** is equivalent to **for(i=0;i<n;i=i+1) ...**

* Simplicity
	* No classes. Only types, methods and multi-dispatch.
	* Simple type system.

## Syntax

A notable difference between say JavaScript, Ruby, Perl, Python and NGS is that there are two syntaxes in the language. One is called **command syntax** and the other is called **code syntax**. The **command syntax** covers the tasks of running programs and i/o redirection. The **code syntax** is a full blown language. The decision to have two syntaxes was made because I don't see another way to provide both good interactive experience and a complete, normal programming language. What I mean is that typing **system('ls')** or even **\`ls\`** is not a good interactive experience. On the other hand having syntax for full blown language based on interactive syntax is not a good thing either. See control structures in bash. It's horrible. **if ... ;then ...; fi**.

**Command syntax**

This is the syntax at top level of the file or when you start an interactive shell. When in **code syntax** you can embed **command syntax** within **\$(...)**, which returns the **Process** object (or a list of these, if **\$(...)** contains pipes).

**Command syntax examples**

* **ls**
* **ls $my\_file**
* **ls $\*my\_files**



# THANKS
Thanks to Guy Egozy, Avishai Ish-Shalom and other friends for ideas and feedback.
