<img alt="NGS logo - cloud and UNIX shell icon" align="right" src="img/ngs-logo-300.png" />

[![Join the chat at https://gitter.im/ngs-lang/Lobby](https://badges.gitter.im/ngs-lang/Lobby.svg)](https://gitter.im/ngs-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ngs-lang/ngs.svg?branch=dev)](https://travis-ci.org/ngs-lang/ngs)


**Next Generation Shell** is a powerful programming language and a shell designed specifically for Ops. *Because you deserve better*.

# Quick links

* [Use Cases](https://github.com/ngs-lang/ngs/wiki/Use-Cases)
* [NGS Website](https://ngs-lang.org/)
* [Docker repository](https://hub.docker.com/r/ngslang/ngs/)

# The problem

* Absence of up-to-date programming language for Ops.
	* Classical shells were made for completely different situation than what we have today. We typically manage systems using APIs now. Structured data is not a luxury anymore but a necessity. `jq` works. I find it tremendously more convenient when a language itself has data structures.
	* General purpose programming languages (Python, Ruby, Go, ...) are not a good fit for typical Ops tasks. Show me one where `echo my_string >my_file` is as easy and as concise as in bash.
* Inadequate shells that Ops use. Like the [dumpster in your terminal](https://en.wikipedia.org/wiki/Redirection_(computing)#Piping) when you can't distinguish between stdout and stderr of several processes? I don't.

# Suggested solution - NGS

I have designed and implemented a programming language with typical Ops tasks in mind. See the use cases below. The next big planned part is the interactive shell.

# NGS use cases

Here are recommended use cases.

* Data manipulation: Got some JSON from an API/CLI call and want to do something with it? Maybe extract something? Maybe modify a bit and shove it into next API call?
* Testing: test CLI programs and API endpoints using built-in testing mini-framework
* Typical Ops Scripting
	* Run external programs
		* NGS has syntax for pipes and i/o redirection
		* Use built-in `Argv` facility for convenient building of command line arguments
		* Let NGS built-in mechanism to throw exceptions when exit codes indicate error
		* Let NGS built-in mechanism to parse commands' output
		* Easily pipe data in and out of external program
	* Easily output JSON or a human-readable nicely formatted table
	* Use built-in `debug()` output function
	* Use built-in `status()` reporting function (previous status is overridden with new status on screen)
	* Use built-in `log()` function which prints timestamps the output
	* Use built-in `retry()` function which prints timestamps the output
* WIP: AWS library
	* Easy integration with existing infra
	* Create new or modify existing infrastructure
	* Easier scripting as opposed to declarative tools (CloudFormation, Terraform) allows modeling of processes and not just final state
* Future: interactive shell

See [Use Cases](https://github.com/ngs-lang/ngs/wiki/Use-Cases) wiki page for more information about the use cases and examples.

# Project status

**The language** is very useful. See the the [bin folder](bin) for examples. NGS is used in Beame.io for miscellaneous scripting such as testing CLI tools, performance tests orchestration, cloud manipulation, etc.

**The shell** is not started yet. The shell will be implemented in NGS. See the [design document](https://github.com/ngs-lang/ngs/wiki/UI-Design).

From George Nachman, creator of [iTerm2](https://www.iterm2.com/):
> Neat! This is totally the project I would do if I had unlimited free time :) I've wished for something like this for a long time. I think there's a lot of room for improvement over a basic TTY, and you've nailed some of the important things

# Code examples

## Arrays


	a = [1, 2, 3]
	arr = a.map(X*2) # arr is now [2, 4, 6]
	for i in arr {
		echo(i)
	}

### Hashes

	h = {"a": 1, "b1": 2, "b2": 3}
	echo(h.filterk(/^b/).mapv(X+10))  # {b1=12, b2=13}


## Functions (multimethods) and multi-dispatch

	F my_func(x:Int) x*10 # Single expression does not require { ... } syntax

	doc This method is documented!
	F my_func(s:Str) {
		t = s * 2
		"[" + t + "]" # Last value returned as the result
	}

	echo(my_func(1))      # 10
	echo(my_func("xyz"))  # [xyzxyz]
	echo(my_func)         # <MultiMethod with 2 method(s)>

More information about the language and syntax in particular is in [ngslang.1](doc/ngslang.1.md)

## Basic cloud

This is how an instance can be created using NGS (real working code). No state file involved!

	{
		NGS_BUILD_CIDR = '192.168.120.0/24'
		NGS_BUILD_TAGS = {'Name': 'ngs-build'}

		vpc    = AWS::Vpc(NGS_BUILD_TAGS).converge(CidrBlock=NGS_BUILD_CIDR, Tags=NGS_BUILD_TAGS)
		gw     = AWS::Igw(Attachments=[{'VpcId': vpc}]).converge(Tags=NGS_BUILD_TAGS)
		rtb    = AWS::RouteTable(VpcId=vpc).converge(Routes=Present({"DestinationCidrBlock": "0.0.0.0/0", "GatewayId": gw}))
		subnet = AWS::Subnet(VpcId=vpc, CidrBlock=NGS_BUILD_CIDR).converge()

		sg = AWS::SecGroup("ngs-build-sg", vpc).converge(
			Description = "ngs-build-sg"
			Tags = NGS_BUILD_TAGS
			IpPermissions = [ AWS::util::world_open_port(22) ]
		)

		ami = AWS::Image(OwnerId=AWS::AMI_OWNER_DEBIAN, Name=Pfx('debian-jessie-amd64-hvm'), RootDeviceType='ebs', VolumeType='gp2').latest()

		instance = AWS::Instance(
			ImageId = ami
			State = null
			KeyName = ENV.get('AWS_NGS_BUILD_KEY', 'ngs-build')
			SecurityGroups = sg
			SubnetId = subnet
			PublicIpAddress = true
			Tags = NGS_BUILD_TAGS
		).converge(
			State = 'running'
		)

		# Get SSH fingerprit from machine's console
		AWS::add_to_known_hosts(instance, 'PublicIpAddress')
	}

## Sample scripts

* [describe ec2 instances](bin/ec2din.ngs). The script has nicely aligned output for humans. It uses `stdlib`'s `Table` to do output layout and columns configuration. `Table` handles columns presence and order and it can be configured via environment variable.
* [build chunk of hosts file](bin/ec2hostsfile.ngs) for a management machine. Hosts named `env-role` or `env-role-N`, depending on whether you have one or more machines of specific role in the environment.
* [Race condition and locks demo](bin/locks.ngs).

# Running using docker

	# Build the docker
	docker build -t ngs .
	# Run the container
	docker run -it --rm ngs
	# Use NGS inside the container
	ngs -pi 'sum(0..10)'

# Compiling and running

## Clone from git

	git clone https://github.com/ngs-lang/ngs.git

	# or if using git over ssh
	git clone git@github.com:ngs-lang/ngs.git

	cd ngs

## Install with dependencies - Debian-based Linux

	./install-linux.sh
	
## Install with dependencies - MacOS (brew)

	./install-mac.sh

## Install without dependencies

	sudo make install

## Run tests

	make tests

## Compile

	make build

## Run

	# If NGS is not installed (from the root of ngs project):
	NGS_DIR=lib NGS_BOOTSTRAP=lib/bootstrap.ngs ./build/ngs SCRIPT_NAME.ngs

	# If NGS is installed:
	ngs SCRIPT_NAME.ngs

Tested as follows (some time ago):

* Pandoc 2.2
* Debian Stretch: gcc 4.8.5 + 4.9.3 + 5, clang 3.6
* Debian Jessie: gcc 4.8.4 + 4.9.2, clang 3.5
* Ubunty Trusty: gcc 4.8.4, clang 3.4

If you have troubles compiling, please try to compile the commit tagged `tested`.

## Uninstall

	cd build
	for i in $(<install_manifest.txt);do rm "$i";done

## Debug - Mac

	# Debug when stuck (note to self mostly)
	killall -SIGSEGV ngs
	lldb --core /cores/core.XXXXX

## Generate documentation

On Linux

	# --- Linux ---
	cd doc
	rm -r out
	mkdir out
	./make.ngs out

On Mac, follow [the instructions to create case sensitive volume](https://coderwall.com/p/mgi8ja/case-sensitive-git-in-mac-os-x-like-a-pro).

	# --- Mac --- One time setup ---
	cd doc/
	ln -s /Volumes/YOUR-VOLUME-NAME out

	# --- Mac --- Each time ---
	cd doc/
	rm -r out/*;
	./make.ngs out

# Contributing

* Fork on GitHub
* Work on whatever you like, preferably something from the GitHub issues of this project
* Make a pull request (to the `dev` branch).

If the change is big or involves modifying the syntax, it's better to coordinate with Ilya before you start.

# Planned Features

( This section is moving to Wiki and Issues )

See "feature" issues and wiki: https://github.com/ngs-lang/ngs/issues?q=is%3Aissue+is%3Aopen+label%3Afeature
* UI: https://github.com/ngs-lang/ngs/wiki/UI-Design
* Manage multiple servers: https://github.com/ngs-lang/ngs/wiki/Manage-Servers-Design
* History: https://github.com/ngs-lang/ngs/wiki/History-Design

You are welcome to open new issues with `feature-request` label if there is something you would like to see in NGS.

## Cross-system

* Toaster/script prepare mode/assist
	* After a command is run (successfully?) a key shortcut key would
	  append it (the last command) to a buffer / file.

## Development

* Code completion
* Variables values shown when editing the commands / code (think `ls $a`, when the cursor is on `$a`)
* Running scripts will once in a while update current line/column in the job info
* Ability to start tracing already running scripts

## Later / unformed / unfinished thoughts

* BIG: Arguments / etc. - description language. Think Javadoc.
	* Python (and other high level languages) is half-way there with argparse.
	  If a special comment is present meaning the script is "argparse safe",
	  it can be run with the shell replacement for argparse to inspect the arguments.
	* Auto discovery of file arguments: run a command with a unique value for
	  an argument and see if it tries to open such file. Tricky, Dangerous.
	* Auto discovery for jars?
	* Anyway, there must be a way to specify externally argument types and objects (file/url/pid... see bash doc about completion for more types)
	* Learn from interaction. Example `curl URL` -> curl has argument of type URL.
		* Provide easy access to modify/delete/blacklist learned commands' arguments.
		* Shortcut key to define object type. Example `curl [SHORTCUT_KEY_PRESSES]` ->
		  menu with object types -> remember the selection.
		* Use same format for learned and pre-defined arguments, allowing easy adding
		  to the shell package and interchange between people.
			* The format (future feature, low priority) will include version detection and which switches are supported in which versions.
			* The format will include how to do completion for specific arguments. Think `ec2kill` < `ec2din ...`.

# How to run the POC

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

# Have you heard of project X? How it compares to NGS?

* All of the shells below
  * ... have no interaction with objects on the screen: if you run a command to describe EC2 instances for example, there is no way to interact with the shown list. Such interaction is a planned feature in NGS.
  * ... have no built-in interaction with a cloud. In NGS follows the principles described in ["declarative primitives for the cloud"](https://ilya-sher.org/2016/07/06/declarative-primitives-or-mkdir-p-for-the-cloud/). Declarative primitives are somewhat similar to Chef or Puppet resources. The main difference between declarative primitives and a configuration management resource is that I'm proposing just a library function which can be called when you need it and not a control-grabbing framework. So for example `AwsElb(...)` function call will make sure the load balancer exists and is configured as described. NGS has work-in-progress AWS library. It's already usable for a subset of AWS resource types.
  * ... have no CLI (planned NGS feature) that is written in the shell language itself.


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
* [elvish](https://github.com/elves/elvish/) features nestable data structures, lambdas and namespacing and is pretty close to general purpose programming language when it comes to the semantics. When compared to NGS, it prefers to extend traditional mechanisms like output capture and pipelines as more expressive programming constructs, and looks a bit more like traditional shells (it is non-POSIX though). It is currently terminal-oriented.
  * Elvish has neat interactive features.
  * The programming language:
    * Simple and consistent (vs NGS' more richer language). Built around the pipes paradigm (vs NGS' two syntaxes, one minimalistic bash-like and the other more suitable for programming).
    * Single space for built-in functions and external programs faces the same problem that bash has with `while ...;do ... done`, hence `joins` and `splits` functions which avoid name clashing.
    * In Elvish nested data structures can flow in pipes. Pipes in NGS currently carry bytes and the need to carry data structures is not felt because in "code" mode this happens naturaly in other ways such as `mydata.map(myfunc)`.
    * New data types can not be defined in Elvish (as opposed to NGS' user data types and mutlti-dispatch methods).
    * External programs that Elvish runs that exit with code other than 0 are converted to exceptions. This is simpler and more consistent approach than NGS takes. NGS has customizable decision system with few sane defaults that knows which exit codes for which programs are exceptions. `false` for example must return 1 and it's not an exception in NGS. `test -f ...` that returns 0 or 1 is fine, 2 is syntax error which is converted to exception. See [blog post about NGS exit code handling](https://ilya-sher.org/2017/01/28/ngs-unique-features-exit-code-handling/).
* [Windows PowerShell](https://en.wikipedia.org/wiki/Windows_PowerShell) is probably the best thing that ever happened to Windows. I'm not familiar with it enough but here are my points
	* PowerShell is built on top of .NET while NGS is a standalone language (as of writing, NGS will be a shell). In my opinion, PowerShell is an adaptation of .NET for scripting while NGS is built from ground up for scripting. I wrote some bootstrapping script in PowerShell and it felt very inconvenient and weird compared to bash or NGS.
	* Syntax
		* PowerShell has also two syntaxes. They are called [parsing modes](https://technet.microsoft.com/en-us/library/hh847892.aspx). These roughly correspond to commands and expression mode of NGS. Compared to NGS, the rules of switching between the two parsing modes are numerous and complex.
		* PowerShell is too verbose by default.
		* NGS syntax is much better in my opinion than syntax of PowerShell.
	* Extending PowerShell is either inconvenient because you have to write in PowerShell which is inconvenient by itself or you have to know C# (or other .NET laguage?).
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
* [sh module for Python](https://amoffat.github.io/sh/): "sh is a full-fledged subprocess replacement for Python 2.6 - 3.5, PyPy and PyPy3 that allows you to call any program as if it were a function".
	* It's Python. If you have some code and you just need to run some commands, it's much better than builtin Python facilities.
	* As modules for other languages - you can't have the convenient syntax for common tasks. 
		* `sh.wc(sh.ls("-1"), "-l")` is really not the same as `ls -1 | wc -l`. BTW, `-1` is not needed because `ls` does that automatically when when `stdout` is not a `tty` (try `ls | cat`).
		* `p = sh.find("-name", "sh.py", _bg=True)` is not the same as `p = $(find -name sh.py &)` (NGS)
		* No redirections syntax so `ls >/tmp/dir_contents` (bash, NGS) becomes `with open("/tmp/dir_contents", "w") as h: sh.ls(_out=h)` in sh.
	* Exit codes handling
		* Similar option to NGS: give "ok" exit codes when running a program. What's not "ok" becomes an exception which you can catch (both in sh and NGS).
		* I did not see an option to customize the system so that you define once what's an exception for a specific command and then this logic is used every time when you run the specified command. NGS does have this capability.

# Discussion / requests / comments

* If you are totally unsure - open GitHub issues.
* Feel free to fork/edit/pull-request this document.
