% NGS(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngs - Next Generation Shell.

# SYNOPSIS

**ngs** *script_name.ngs* [arguments]

**ngs** [**-e**|**-E**|**-p**|**-pi**|**-pj**|**-ppj**|**-pl**|**-pjl**|**-pt**] *expression* [arguments]

**ngs** [**-h**|**--help**]

# DESCRIPTION

**ngs** is a Next Generation Shell. It has two main parts: the language and the interactive shell.

# OPTIONS

Given *script_name*, runs the script. The script runs top to bottom. If the script defines `main` function/multimethod, it is automatically invoked (after the top-to-bottom run), with command line arguments converted to appropriate types. If no `main` is defined (or `main` did not match the arguments) *and* the result of top-to-bottom execution is a Namespace (`ns { ... }`) *and* at least one command line argument was passed, the first command line argument is taken as function/multimethod name in the Namespace to run.

If *script_name* is **.**, `ngsfile` is used as the *script_name*.

Using *expression* in the options described below is equivalent to running a script that consists of `{` *expression* `}`.

**-e** evaluates the *expression*.

**-E** prevents loading of **stdlib.ngs** and evaluates the *expression*.

**-p** prints the resulting *expression* value in a human readable NGS format.

**-pi** prints `inspect(...)` (detailed information) of the resulting *expression* value.

**-pj** prints the resulting *expression* value as JSON.

**-ppj** prints the resulting *expression* value as pretty JSON. Requires `jq` to be installed.

**-pl** prints elements of the result, one per line (mnemonic "print lines")

**-pjl** prints elements of the result, one per line as JSON (mnemonic "print JSON lines")

**-pt** prints the resulting *expression* value as a table.

**-h** and **--help** shows help

# MOTIVATION

NGS tries to fill the void between classical shells such as **bash** and general-purpose programming languages such as **Ruby**, **Python**, **Perl**, **Go**. The shells are domain-specific languages but the domain has changed so classical shells are not optimal for today's tasks. General-purpose languages on the other hand are not domain-specific so they are not good as shells and too verbose for system tasks scripting, not a good fit.

# EXAMPLES

For examples, see [ngstut(1)](ngstut.1.md).

# EXIT STATUS

The exit code is whatever the running code (*script_name* or *expression*) returns using the following conversions:

* `Int` (integer) - used directly as exit code (modulo 256). For example, `ngs -E 10` returns exit code 10.
* `Bool` (boolean) - `true` results 0, `false` results 1.
* `T` (a user-defined type) - The exit code can be customized by `ExitCode(x:T)` method.
* Anything else, including user-defined types without `ExitCode(x:T)` method will result 0.

In case of an uncaught exception, the exit code is 240. In cases where additional errors were encountered during exiting, the exit codes will be greater than 240, [ngslang(1)](ngslang.1.md) has specific information on that.

# ENVIRONMENT

* `DEBUG` - when non-empty string, switches on `debug` method output (default is off). It's recommended to use `debug("my debug info")` in your scripts for turning debug output on and off easily, using the `DEBUG` environment variable.
* `NGS_BOOTSTRAP` points to the bootstrap NGS file. On NGS startup this file is always run. Defaults to built-in stdlib with bootsrapper.
* `NGS_BOOTSTRAP_DEBUG` - if defined, show **bootstrap.ngs** debugging information.
* `NGS_PATH` (defaults to `$HOME/.ngs:/usr/local/etc/ngs:/usr/local/lib/ngs:/etc/ngs:/usr/lib/ngs`) - location of the **autoload** directory. Files are automatically loaded from this directory when an undefined global variable is used.
* `NGS_EXIT_BACKTRACE` - print backtrace when `exit()` is called.
* `NGS_WARN_BACKTRACE` - print backtrace when `warn()` is called. Prints only unique backtraces.
* `NGS_ERROR_BACKTRACE` - print backtrace when `error()` is called. Prints only unique backtraces.

# FILES

## stdlib.ngs

Packaged into `ngs` binary during build. Standard library. Defines many methods and autoloading behaviour.

# SEE ALSO

|||
|-|-|
|[ngstut(1)](ngstut.1.md)| NGS language tutorial|
|[ngslang(1)](ngslang.1.md)| NGS language manual|
|[ngswhy(1)](ngswhy.1.md)| Motivation behind creation of NGS|
|[ngsstyle(1)](ngsstyle.1.md)| Recommended style for NGS scripts|


# THANKS

Thanks to Zeev Glozman for Mac support, CMake contribution and other help.
Thanks to Ricardo Gomes for feedback, fixes to documentation, and other contributions.
Thanks to Guy Egozy, Avishai Ish-Shalom and other friends for ideas and feedback.
Thanks to Rui Chen for Homebrew related fixes and GitHub actions fixes.
