## (UNRELEASED) Version 0.2.5

### New features

* Running NGS without arguments no more produces an error but a friendly usage message
* Docker image for each commit; built by Travis
* Threads-related
	* Add `Thread.local` - thread local storage (a Hash)
		* `Thread.local.thread` - current thread object
	* Threads are now named:
		* Add `init(t:Thread, name:Str, f:Fun)`
		* Add `init(t:Thread, name:Str, f:Fun, arg)`
* `aws` command output parsing - `.Reservation` renamed to `._Reservation`
* Exiting
	* Add `ExitException` - parent of all exceptions with `exit_code` field
		* `FatalError` is now a child of `ExitException`
		* Add `NormalExit` (child of `ExitException`)
	* Add `exit(exit_code:Int=1)`
	* Add `exit(s:Str, exit_code:Int=1)`
* Add `dollar \$ escaping syntax`
* Add `map_idx_val(e:Eachable1, mapper:Fun)`
* Add `myip()` - returns your IP as observable from the Internet
* Add experimental `each_chunk(e:Eachable1, n:Int, cb:Fun)`
* Add experimental `decode(s:Str, t:Type)`
* Add experimental `nd` (Ngs Data) command line tool

### Fixes and improvements

* Improve `Pred(r:RegExp)`
* Fix `join(Arr)` for array of `Thread`s
* `Bool(Path)` now checks that underlying file system object is of the correct type
* `dir()` now skips `.` and `..` entries
* `dir()` now has "raw" parameter
* decode() now parses "locate" output
* nop() now takes any arguments
* retry() - removed "catch_exceptions" parameter
* assert_path_exists() - improve implementation

### Work in progress

* `AWS2` -- second version of AWS (among other: `.Region` renamed to `._Region`)
* `bin/na` ("NGS AWS") -- CLI for AWS wrapper around declarative primitives library
* documentation
* Moving `todo.txt` to [GitHub issues](https://github.com/ngs-lang/ngs/issues)

## 2018-04-21 Version 0.2.4

### New features

* Add `Time`, built on exposed c primitives - gettimeofday, strptime, mktime
* Add `Set` type
* Add `.user` field for arbitrary data on types
* Add `bin/hn.ngs` -- "tail -f" like script for Hacker News
* status() will override previous one on tty using escape codes
if no other output was sent between the calls
* status() - use iTerm2 badges

### Fixes and improvements

* `join(Thread)` - throw exception if the other thread had exception
* Improve AWS declarative primitives library
* Implement file descriptors redirection: `$(my_prog 2>${1})`
* Better `Hash` iteration in stdlib due to additional exposed `ll_hash_*` primitives and `LLHashEntry` type.
* Fix Linux installation instructions
* Update Mac installation instructions
* Automatically set reference to namespace in methods defined in `ns { ... }`
* Improve memory allocation - use "atomic" memory in few additional places
* Rename "Method" to "Multimethod" and "Method imlementation" to "Method" all over
* Rename "attribute" to "field" all over
* Deprecate KV
* Rename `to_exit_code` to `ExitCode` for consistency (deprecate `to_exit_code`)

### Work in progress

* AWS2 -- second version of AWS
* `bin/na` ("NGS AWS") -- CLI for AWS wrapper around declarative primitives library
* `Table2` -- the more advanced tabular data holder
* documentation
* Experimental syntax for building documents
* Add experimental `MapIter`, `FilterIter`, `FunIter`

## 2017-07-09 Version 0.2.3

* Fix missing `version.h` and `ChageLog` update in 0.2.2

## 2017-07-09 Version 0.2.2

* Fix `assert_*` functions after pipes support in 0.2.0

## 2017-06-07 Version 0.2.1

* Add `lines(cp:CommandsPipeline)` to fix broken `$(command).lines()`
after pipes support in 0.2.0
* Add `to_exit_code(cp:CommandsPipeline)` to fix broken exit codes
after pipes support in 0.2.0
* Fix tests broken with `RegExp` syntax change in 0.2.0
* Fix `Str(t:Time, format:Str=TIME_FORMAT, gmt:Bool=false)`
that was not using `format`
* Add experimental `Pred(h:Hash)`

## 2017-06-02 Version 0.2.0

### Breaking changes

* BREAKING: Swap write() arguments from write(what, file) to write(file, what)
Many methods with old arguments order will still work but will give a
deprecation warning.

###  New Features
* Pipes support
* Add optional "count" (bytes) parameter to read(fd:Int)
* Add preliminary `Str(t:c_pthread_t)`
* Add missing `id(rrset:RecordSetRes)`
* Add `abs(i:Int)`
* Add `decode_uri_component(s:Str)`

### Fixes and improvements

* Slightly improved documentation
* Improve stability of `Str(r:Res)`
* Fix error handling in `[](arr:Arr, r:NumRange)`
* Add optional "max_parts" parameter to split(s:Str, delim:Str)
* A bit more readable inspect() output
* Fix backslashes syntax in `/regular expressions/`

## 2017-05-13 Version 0.1.0

* Incorporate Mac OS X support contributed by Zeev Glozman
