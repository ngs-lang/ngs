% NGS(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngs - Next Generation Shell.

# SYNOPSIS

**ngs** *script_name.ngs*

**ngs** [**-e**|**-E**|**-p**|**-pi**|**-pj**|**-pl**|**-pjl**] *expression*

# DESCRIPTION

**ngs** is a Next Generation Shell. It has two main parts: the language and the interactive shell.

This project contains the language part which is under development. The interactive part will be dealt with later. This manual covers both implemented and unimplemented parts.

# OPTIONS

Given *script_name* runs the script.

Using *expression* is equivalent to running a script that consists of `{` *expression* `}`.

**-e** evaluates the *expression*.

**-E** prevents loading of **stdlib.ngs** and evaluates the *expression*.

**-p** prints the resulting *expression* value in a human readable NGS format.

**-pi** prints `inspect(...)` (detailed information) of the resulting *expression* value.

**-pj** prints the resulting *expression* value as JSON.

**-pl** prints elements of the result, one per line (mnemonic "print lines")

**-pjl** prints elements of the result, one per line as JSON (mnemonic "print JSON lines")

## MOTIVATION

NGS tries to fill the void between classical shells such as **bash** and general-purpose programming languages such as **Ruby**, **Python**, **Perl**, **Go**. The shells are domain-specific languages but the domain has changed so classical shells are not optimal for today's tasks. General-purpose languages on the other hand are not domain-specific so they are not good as shells and too verbose for system tasks scripting, not a good fit.

# EXIT STATUS

The exit code is whatever the running code (*script_name* or *expression*) returns using the following conversions:

* `Int` (integer) - used directly as exit code (modulo 256). For example, `ngs -E 10` returns exit code 10.
* `Bool` (boolean) - `true` results 0, `false` results 1.
* `T` (a user-defined type) - The exit code can be customized by `ExitCode(x:T)` method.
* Anything else, including user-defined types without `ExitCode(x:T)` method will result 0.

In case of an uncaught exception, the exit code is 240. In cases where additional errors were encountered during exiting, the exit codes will be greater than 240, [ngslang(1)](ngslang.1.md) has specific information on that.

# ENVIRONMENT

* `DEBUG` - when non-empty string, switches on `debug` method output (default is off). It's recommended to use `debug("my debug info")` in your scripts for turning debug output on and off easily, using the `DEBUG` environment variable.
* `NGS_BOOTSTRAP` points to the bootstrap NGS file. On NGS startup this file is always run. Defaults to first of:
	* `$HOME/.bootstrap.ngs`
	* `/etc/ngs/bootstrap.ngs`
	* `/var/lib/ngs/bootstrap.ngs`
	* `/usr/share/ngs/bootstrap.ngs`
* `NGS_BOOTSTRAP_DEBUG` - if defined, show **bootstrap.ngs** debugging information.
* `NGS_DIR` (defaults to `/usr/share/ngs`) - location of **stdlib.ngs** file and the **autoload** directory. Files are automatically loaded from this directory when an undefined global variable is used.
* `NGS_EXIT_BACKTRACE` - print backtrace when `exit()` is called.
* `NGS_WARN_BACKTRACE` - print backtrace when `warn()` is called. Prints only unique backtraces.
* `NGS_ERROR_BACKTRACE` - print backtrace when `error()` is called. Prints only unique backtraces.

# FILES

## bootstrap.ngs

Typically located in `NGS_DIR`. Responsible for bootstrapping NGS.

* Loads **stdlib.ngs** if needed.
* Runs the script specified in the command line or exectes *expression* according to swithces.
* Prints *expression* value if needed.

## stdlib.ngs

Located in `NGS_DIR`. Standard library. Defines many methods and autoloading behaviour.

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
