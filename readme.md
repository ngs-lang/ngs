Next Generation Shell. [![Build Status](https://travis-ci.org/ilyash/ngs.svg?branch=master)](https://travis-ci.org/ilyash/ngs)

![NGS logo - cloud and UNIX shell icon](img/ngs-logo.svg?raw=true "NGS")

Quick links
===========

* [Language tutorial](https://ilyash.github.io/ngs-doc/ngslang.1.html)
* [Built-in types generated documentation](https://ilyash.github.io/ngs-doc/types.html) (WIP)
* [Built-in method generated documentations](https://ilyash.github.io/ngs-doc/methods.html) (WIP)
* [NGS Facebook group](https://www.facebook.com/groups/next.generation.shell/)
* [Reddit](https://www.reddit.com/r/NextGenerationShell/)

Motivation - The problem with current state
===========================================

Shells are [Domain Specific Languages](https://en.wikipedia.org/wiki/Domain-specific_language).  The domain has changed greatly since the shells we use today were conceived.  The shells never caught up.

What I see is a void. There is no good language for system tasks (and no good shell). What's near this void is classical shells on one hand and general-purpose (non-DSL) programming languages on the other. Both are being (ab)used for system tasks.

The problem with classical shells looks pretty clear: they were made with one kind of tasks in mind but are used for other, bigger and more complex tasks. Such scripts usually look as a fight against the language and working around it much more than using it to solve the problem.

The problem of using general purpose programming languages (Python, Ruby, Perl, Go) is not so obvious. Domain-specific language makes your life much easier when solving the tasks that the language was built for.  Of course you can write to a file in any language but probably not as easy as `echo something >my_file`. You can run a program but it's probably won't be a simple `ls`. The scripts that I've seen (and written in Python and Ruby) look too verbose and show unnecessary effort. Such scripts do not look an optimal solution (at the very least).

Vision
======

* Create a language that will be domain-specific for system tasks.
* Create a shell (in that language) that is up to date with today's tasks - working with APIs, cloud, remote execution on a group of hosts.

About this document
===================

This document started as internal draft. Some sections might not be clear. Still exposing it as per "release early" policy. Feel free to open a GitHub issue or email me directly: ilya (DOT) sher (AT) coding (DASH) knight (DOT) com

Project status
==============

Development. Help is welcome.

The bigger part of the language is implemented to the point that some useful scripts can be written. See [the utilities folder](bin).

The language feels (to me) like a mix of Python, bash and a bit less Ruby and Perl 6 with saner syntax, all of it taken to a more functional direction. Unique features are rare but contribute a lot toward solving domain-specific solutions. Most useful of the unique features (in my opinion) is the ```````` ``command`` ```````` syntax which runs the command and parses the output.

* Demo 1: [describe ec2 instances](bin/ec2din.ngs). The script has nicely aligned output for humans. It uses `stdlib`'s `Table` to do output layout and columns configuration. `Table` handles columns presence and order and it can be configured via environment variable.
* Demo 2: [build chunk of hosts file](bin/ec2hostsfile.ngs) for a management machine. Hosts named `env-role` or `env-role-N`, depending on whether you have one or more machines of specific role in the environment.
* Demo 3: [demonstrates race condition](bin/locks.ngs) and locks.


Running using docker
====================

	# Build the docker
	docker build -t ngs
	# Run the container
	docker run -it --rm ngs
	# Use NGS inside the container
	ngs -pi 'sum(0..10)'


Compile and run
===============

	sudo apt-get install uthash-dev libgc-dev libffi6 libffi-dev libjson-c2 libjson-c-dev peg libpcre3-dev make
	sudo type awk || sudo apt-get install gawk
	make
	# NGS_DIR, where stdlib.ngs resides defaults to /usr/local/lib/ngs . Either link it to the "lib" folder or run with NGS_DIR=lib
	NGS_DIR=lib ./ngs SCRIPT_NAME.ngs

Tested as follows:

* Debian Stretch: gcc 4.8.5 + 4.9.3 + 5, clang 3.6
* Debian Jessie: gcc 4.8.4 + 4.9.2, clang 3.5
* Ubunty Trusty: gcc 4.8.4, clang 3.4

If you have troubles compiling, please try to compile the commit tagged `tested`.

Running tests
=============

	# NGS_DIR, where stdlib.ngs resides defaults to /usr/local/lib/ngs . Either link it to the "lib" folder or run with NGS_DIR=lib
	NGS_DIR=lib make test

Contributing
============

Fork on GitHub, work on whatever you like, preferably from the top of [the todo](todo.txt), make a pull request. If the change is big or involves modifying the syntax, it's better to coordinate with Ilya before you start.

Planned Features
================

UI
--
* Screencast of `small-poc` is on youtube: http://www.youtube.com/watch?v=T5Bpu4thVNo

* Not to block, allow typing next commands even if previous command(s) still run(s).
	* Open issue: how to deal with a command that requires interaction.

* Provide good feedback. In GUI for example, this can be green / red icon near a completed command to show exit status. Tweaking prompt to include such info or typing `echo $?` all the time is not what I dream about.

* All operations made via a UI, including mouse operations in GUI _must_ have and display textual representation, allowing to copy / paste / save to a file / send to friend.

* Different UI modules must exist. In the beginning we can start with these:
  * Console (use pty)
  * Web (allow multiple users to collaborate, some rw, some ro)

* Commands' outputs displayed below the commands up to max N lines then scroll the output in
  a small window below the command. When there is more output than a human can process -
  don't display it and suggest saving it to a file (maybe).

* Commands scroll up, new commands are added at the bottom. When a command that haven't
  completed yet, reaches top of the screen, it can be converted to a mini-area at the
  top (right?) of the screen, representing the command and current progress (and exit
  status later).

* [Later] Confirmation mode. One user in collaboration mode gives the command to execute,
  another user must approve the command for execution.

* Display structured results as real f\*cking structures (JSON, Yaml, ...)
	* Most of the data dealt with is tables. List of files, list of instances in a cloud, list of load balancers. Amazing that none of current UNIX shell tools (I heard of) don't treat the data as such. The closest you get is set of records in `awk`. Well, if the fields in records are the same it's actually a table. `$1` in awk could be `id` or `name`, referencing the data by column name and not by field number. Yes, you have `jq` and it's close but it still works (in best case) with list of records with same fields. PowerShell has something in that direction (`Get-Process | Where-Object {$_.handles -gt 200}`)[https://technet.microsoft.com/en-us/library/ee177028.aspx] + `Format-Table`.
	* (Maybe) Allow editing it and saving to file.
	* (Maybe) Allow write jq filters in (G)UI by selecting the elements

* Underline red/green for existing/non-existing files? Fish shell does it for the commands.

* Actions on objects that are on screen. Think right click / context menu.

* Commands history: among duplicate commands all but last should be grayed out, so that non-grayed out commands are unique.

* When hover over an object, highlight the same object everywhere it appears.

Cross-system
------------

* Feedback
	* Some new protocol is needed to convey process progress and other information
	* For external programs that don't support such protocol (currently all existing programs)
	  use heuristics such as look at open files + position in such files to guess the progress
	  with high probability.

* Ability to add new commands after a running one completes

* Manage multiple servers at once

	* Preferably using nothing more than standard SSH,
	  maybe uploading required parts automatically when a command needs to run.

	* Smart displaying of results,
	  such as "OK on all T servers", "OK on N servers, fail on M servers, out of total T servers"

	* Smart handling of failures,
	  maybe divide into groups depending on command status / output and then letting
	  to manage these groups. Consider dividing to several "fail" groups depending on
	  the fail mode. Think deploy script that should handle the conditions. Also make
	  one large group for any failures (contains all fail sub-groups).

	* Automatic server groups by
		* (In a cloud) a security group, a tag or other properties (regex on name for example)
		* (Configuration management: Chef/Puppe) by properties
		* (Locally) by /etc/hosts remark or by .ssh/config properties or remarks
		* by naming conventions (for example regex defined) for all cases above
		* Dynamic by a command output/exit code
		  Think `netstat -lpnt | grep -q :8000` or `pgrep java` or `dpkg -l '*apache*' >/dev/null`

	* Allow to run commands on remote hosts and connect them with pipes
		* Example: `@web_servers { cat /var/log/messages } | @management_server { grep MY_EVENT | sort > /tmp/MY_EVENT }`
		  That's just for the sake of an example, it would probably be better to `grep` locally.
		* Issue warning if the output of `cat` can not be pushed or pulled directly between
		  the machines (and therefore will be transferred through the controlling host, where the shell runs)
			* Have shortcut key to setup SSH access required for direct transfer
			* Or.. run temporary SSH daemons to allow this?
		* Provide meaningful progress on that, including ETA. This won't be easy but it's worth it.
		* Provide processing speeds inspection, CPU graphs, network usage, including graphs.
		  It can be helpful to identify and show slow machines.
		  If it's a cluster,the performance should be similar. If not, it can inidcate a problem.

* Easy integration with currently available software

	* Easy way to write hooks that will provide additional information
	  about running process, such as progress.

* Smart completion, context sensitive
	* Command switches and values
	* Complete objects (file names, urls, etc) depending on context.
	  Think `wget .../x.tgz`, `tar ` [COMPLETION_KEY] [Maybe some choice keys] -> ` xzf x.tgz`
	* Maybe API to insert objects to completion history
	* Auto-detect completion history objects for existing commands (by wrappers and/or hooks probably)

* "Mentioned" completion
	* Complete objects from output of previous commands. Example: `apt-cache search ...` , `apt-get install ...`
	  Isn't this copy+paste annoying? It's already on the screen, it's a package name, and still the system can't
	  complete...

* Toaster/script prepare mode/assist
	* After a command is run (succsessfully?) a key shortcut key would
	  append it (the last command) to a buffer / file.

* Processes communication

	* Remove stupid limit of one output is connected to one input of the next process,
	  allow fan out and fan in

	* Support named inputs and outputs

	* Allow on the fly connection to pipes/files. Think a logger is writing to a file on full disk.
	  Just reconnect to another file on another partition/disk.

	* UI representation of a job. A map with all involved processes, their open files,
	  sockets, pipes, resource usage (CPU, disk, network), process running time, accumulative
	  CPU time, ...

History
-------

Two types of history:

* Commands history, similar to todays history but each entry will consist of the following fields:
	* Timestamp
	* Command
	* Output (stdout, stderr)
	* Input (stdin) ?
	* Exit code
	* Comment - can be provided when runnig the command, editable after that
	* Context:
		* Target host / group
			* Working directory
			* Values of used variables
* Host history - all changes on given host. Points:
	* History entry will consist of:
		* Timestamp
		* Script or basic command (create/update/chmod file, etc)
		* Script that changed the object (I guess only files for now)
		* Host that was running the script
		* User that was running the script
		* Session (name) under which the script was running
		* Relevant global variables
		* Consider putting in git all modified files
	* Host history will be kept on both the shell host and the target host

Development
-----------

* Code completion
* Variables values shown when editing the commands / code (think `ls $a`, when the cursor is on `$a`)
* Running scripts will once in a while update current line/column in the job info
* Ability to start tracing already running scripts
* API to report the progress
	* Current task (`Copying mydata.txt to /tmp/`)
	* Overall progress (`70%` or `File 7 out of 10`)
	* ETA maybe

The NGS language
----------------

Two languages actually.

* Current-shells-like but simplified (called "commands"), `$(...)` syntax
* Scripting language, "normal" programming language (called "code"), `{...}` syntax

### The NGS "code" language ###

* Functional
* Types (File, Host, Group, Str, Num, ...)
* Multi-dispatch with guards (trying to avoid "regular" full-blown OO to minimize the work)
	* For example:
		* `replace(Str orig, Str a, Str b)`
		* `replace(Array orig, Str a, Str b)` - replaces in all strings in the `orig` array
			* This may have a guard something like: `all(orig, isStr)`
		* `replace(File f, Str a, Str b)` - will `sed` the file, possibly backing it up.
* Lots of functions for data manipulation (TODO: list them)
* File, Host, Group literals:
	* f'/tmp/my-temp.txt'

Later / unformed / unfinished thoughts
--------------------------------------

* Measure and graph pipes throughput (and/or process performance for example by how fast it reads an input file)

* In a job, per process state / title

* Number of open files / sockets is part of the process status

* Interactive pipe construction: show intermediate results
	* only when requested (for example by a shortcut key) because commands can have side effects
	* white list of "safe" commands to show output

* Preview output with a shortcut key (don't run the command and insert it's output to the stream)

* Dynamic update command output, think `ps` / `top` when their output is in the stream
	* "pin" feature so user defined command sticks to the screen and being re-run and the output updated,
	  essentially making it a widget

* On the fly check of called commands existence
	* Also on servers group
	* Shortcut key to install the providing package (of course with textual representation)

* On the fly check of command arguments
	* Bring relevant man section
	* Show error on unsupported switches
	* Warn on different versions / implementations of the command in a hosts group
	* Preview wildcards expansions (can be difficult to implement because of relativity to current directory)

* Hosts group that is automatically updated should show last update time
	* ... and updates log, when machines were added/removed
	* "Current" hosts group to execute commands on.
	* Whenever group is formed, connectivity must be checked and problems notified
	* Each host should have statuses, such as `pending` for EC2 machines
	  (in the shell can be pending till SSH connection is ready)
  * Have group history (snapshots of list of hosts in the given group)
  * When running a command on a group of hosts, run on one first and then rolling
    as default behaviour. Maybe stop at certain error rate.

* Statuses should have "debug level" to them so that background jobs could be showed as less important

* Sessions. Total and global environment with it's own (saved) history etc. Think session per project or client.
  Option to share a session with other people. Open issue: history - common for session or per user
  (probably per user).

* Quick navigation. Search for session, host, host group, IP, history, etc.

* Icons (in any UI that allows it). Icons are processed much faster than text.

* Every failed script must have exact error information to it.
  No re-run with added `-x` or `echo`s should be required.

* Commands of the shell will pass objects in pipes between them, not strings.
  External commands should use JSON or API (not sure here).

* For remote host or hosts group, give an option to execute command(s)
  as soon as the host is available. Notify when done, failed or timed out.

* In-line editor (maybe) for when a command line becomes few lines.
  Or quick way to take the long line and continue editing it in an
  editor.

* BIG: Arguments / etc. - description language. Think Javadoc.
	* Python (and other high level languages) is half-way there with argparse.
	  If a special comment is present meaning the script is "argparse safe",
	  it can be run with the shell replacement for argparse to inspect the arguments.
	* Auto discovery of file arguments: run a command with a unique value for
	  an argument and see if it tries to open such file. Tricky, Dangerous.
	* Auto discovery for jars? Think ec2 tools.
	* Anyway, there must be a way to specify externally argument types and
	  objects (file/url/pid... see bash doc about completion for more types)
	* Learn from interaction. Example `curl URL` -> curl has argument of type URL.
		* Provide easy access to modify/delete/blacklist learned commands' arguments.
		* Shortcut key to define object type. Example `curl [SHORTCUT_KEY_PRESSES]` ->
		  menu with object types -> remember the selection.
		* Use same format for learned and pre-defined arguments, allowing easy adding
		  to the shell package and interchange between people.
			* The format (future feature, low priority) will include version detection
			  and which switches are supported in which versions.
			* The format will include how to do completion for specific arguments.
			  Think `ec2kill` < `ec2din ...`.

* Define which commands will run where when using hosts group. Think `ec2...` on
  a group of machines which include all ec2 machines: "management" machine, web, app,
  etc. servers.

* Hosts groups should be organized in a stack ( pushd/popd style )

* Hosts group will be ordered. When running commands, one could specify to run in order or async.
	* When commands run in order there should be an option to stop on first fail.

How to run the POC
==================

Following instructions should work (tested on Debian)

	cd small-poc
	mkdir ssl
	cd ssl
	openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout mysitename.key -out mysitename.crt
	cd ..
	npm install
	nodejs server.js

* Go to <a href="https://127.0.0.1:8443/main.html">https://127.0.0.1:8443/main.html</a>
	* Commands to try:
		* `ls`
		* `pr` - a long process with progress bar
		* `sleep` - a process that sleeps for 5 seconds
		* `fail` - a process that fails

Have you heard of project X? How it compares to NGS?
====================================================

None of the shells below provide interaction with objects on the screen: if you run a command to describe EC2 instances for example, there is no way to interact with the shown list. Such interaction is a planned feature in NGS.

None of the shells below have built-in interaction with a cloud. In NGS the work on ["declarative primitives for the cloud"](https://ilya-sher.org/2016/07/06/declarative-primitives-or-mkdir-p-for-the-cloud/) has already started. Declarative primitives are somewhat similar to Chef or Puppet resources. The main difference between declarative primitives and a configuration management resource is that I'm proposing just a library function which can be called when you need it and not a control-grabbing framework. So for example `AwsElb(...)` function call will make sure the load balancer exists and is configured as described.


* [oil shell](http://www.oilshell.org/) is a very promising project with motiviation similar to that behind NGS. It's too early to tell the differences.
	* [Oil goals](https://github.com/oilshell/oil/wiki/Project-Goals)
	* Current (2017-01) work in the project is on the importer that could import existing bash scripts. NGS does not have such importer and it's not currently planned.
	* The project also has a [good page listing other shells](https://github.com/oilshell/oil/wiki/ExternalResources)
	* [Subreddit](https://www.reddit.com/r/oilshell/).
* [fish shell](http://fishshell.com/) has very nice features and improvements but is still more bash-like. For example it doesn't have nested data structures nor a full-featured programming language.
* [Plumbum](https://github.com/tomerfiliba/plumbum) makes it easier to call shell commands from python. Too verbose to be used as a shell or shell script. It helps when you need to use python and call external programs. Primary target audience seems to be Python developers, not system engineers.
* [Xonsh](http://xon.sh/) Python with bash-like additions. Python is not a domain specific language, making bash-like additions can not bring it to be optimal for system tasks.
* [rc shell](https://swtch.com/plan9port/man/man1/rc.html) is much closer to Bash than to NGS.
* [Es: A shell with higher-order functions](https://wryun.github.io/es-shell/paper.html) . ES and NGS share quite a bit of common ideas. NGS goes further with making a shell a real programming language. ES vs NGS would probably be a matter of personal preference. ES is simpler and been here for a while.
* [elvish](https://github.com/elves/elvish/) aims to extend instead of revolutionize traditional shells; the core syntax is very close to bash. It has nested data structures, and rich pipes to carry such complex data. It also has some neat interactive features, but is still terminal-oriented.
* [Windows PowerShell](https://en.wikipedia.org/wiki/Windows_PowerShell) is probably the best thing that ever happened to Windows. I'm not familiar with it enough but here are my points
	* PowerShell is built on top of .NET while NGS is a standalone language (as of writing, NGS will be a shell). In my opinion, PowerShell is an adaptation of .NET for scripting while NGS is built from ground up for scripting. I wrote some bootstrapping script in PowerShell and it felt very inconvenient and weird compared to bash or NGS.
	* Syntax
		* PowerShell has also two syntaxes. They are called [parsing modes](https://technet.microsoft.com/en-us/library/hh847892.aspx). These roughly correspond to commands and expression mode of NGS. Compared to NGS, the rules of switching between the two parsing modes are numerous and complex.
		* PowerShell is too verbose by default.
		* NGS syntax is much better in my opinion than syntax of PowerShell.
	* Extending PowerShell is either inconvenient because you have to write in PowerShell which is inconvenient by itself or you have to know C#.
	* PowerShell got some things right compared to other shells: structured data and consistent `$` in front of variables come to mind.
	* Despite some similarities, writing a script in PowerShell and NGS is a completely different experience. You should try both and pick NGS without any doubt :)
* [Shill - Scripting with Least Privilege](http://shill.seas.harvard.edu/) . Security focused (capability-based), runs on FreeBSD only (looks like Shill kernel module is required), examples mostly show security features, written in Racket. Not much development since initial commit at 2014. Real world usability is unclear. At this point I assume NGS as a programming language is much more usable.
* [the shok command shell](http://shok.io/)
	* Similarity: opinion regarding current shells and status. I agree with most of the [motivation page](http://shok.io/info/motivation.html):
		* We can do much better.
		* Current attempts at solutions do not solve the problem.
		* New shell is needed
		* New programming languag
	* Similarity: two-modes syntax. Apparently @nfomon also haven't figured out a way to have one syntax.
	* Difference: Shok shell is in C++. NGS' shell is not implemented yet but it will be in NGS.
	* Difference: Shok has [modular design](http://shok.io/info/implementation.html) .
	* Status: as of 2016-12-30 the latest commit was over a year ago.
* [Ammonite](http://www.lihaoyi.com/Ammonite/#Ammonite-Shell)
	* Similarity: opinion that shells can be much better - "Replacing Bash for the 21st Century", "You think that technology has improved in the last 38 years and a modern systems shell should be better than the shells of our forefathers"
	* Similarity: opinion that shell needs a full-featured programming language.
	* Difference: Scala as the shell language. NGS uses new, domain-specific language. I don't think any amount of tinkering with existing languages (maybe except a lot of it with Lisp) can make these languages as usable for shell as new language that was specifically designed to be a shell language.
	* Difference: Ammonite is JVM based. I think it would be really hard to convince anyone that manages systems to have JVM installed on the managed systems just to run a shell. NGS is written in C and compiles to native binary.
	* Difference: Ammonite's REPL looks very good. NGS does not have a REPL yet.
	* If you are OK with Scala, Ammonite is worth trying. I think Scala is too complicated, especially as a shell language. Looking at [HTTP request](http://www.lihaoyi.com/Ammonite/#HTTPRequests): `val resp = Http("https://api.github.com/repos/scala/scala").asString` and `val parsed = upickle.json.read(resp.body).asInstanceOf[upickle.Js.Obj]`. In NGS that would be ```````` parsed=``curl -s "https://api.github.com/repos/scala/scala"`` ````````. On the other hand [Ammonite-Ops](http://www.lihaoyi.com/Ammonite/#Ammonite-Ops) and [Ammonite-Shell](http://www.lihaoyi.com/Ammonite/#Ammonite-Shell) aim to make common "operations" tasks convenient to handle.


Discussion / requests / comments
================================

* If you are totally unsure - open GitHub issues.
* Feel free to fork/edit/pull-request this document.
