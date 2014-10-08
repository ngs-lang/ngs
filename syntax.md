Ze Syntax
=========

Anywhere - commands and code
----------------------------

Generally the syntax will have 2 modes:
* Commands - for executing external commands, such as "ls --color=yes xyz $other\_file\_name"
* Code - for internal ze programming, such as "subst(v, /\.txt/, '.mytext')"

* `CCEXPR` - commands or code expression. can have the form of
  * `(` commands `)` -> last process object
  * `{` code `}` -> last value

Anywhere - substitution
-----------------------

* $VAR / $CCEXPR - simple substitution
* $\*VAR / $\*CCEXPR - uses the array in VAR or CCEXPR to generate several words (command arguments usually)

Assignment
----------

* VAR=code

Control strucures
-----------------

* `if` CCEXPR CCEXPR [`else`] CCEXPR
* `while` CCEXPR CCEXPR
* `do` CCEXPR `while` CCEXPR

Define functions
----------------

* `defun` NAME(params) CCEXPR -> F
* `lambda` (params) CCEXPR -> F

Command built-ins
-----------------

*Alphabetically sorted*

* `run_if_executable CMD ARG ...` - run the command with arguments if `CMD` file exists and is executable
* `source CODE_FILE`
* `source_if_exists CODE_FILE` - source the `CODE_FILE` if it exists

THINK: these should be implemented as part of standard library, not part of the shell itself.

    # Somethng like this:
    defcmd run_if_executable() {
      if {File.executable(args[0])} (
        $*args
      )
    }
    defcmd source_if_exists(f) {
      if {File.exists(f)} (
        source $f
      )
    }

Built-in functions
------------------

* `split(str, delim=/\s+/)` -> List: string split by the delimiter
* `link(existing, new, soft=false, force=false)`


Macros (?)
----------

* `defmacro` NAME(params)
