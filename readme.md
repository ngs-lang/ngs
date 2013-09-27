Next generation UNIX shell

Vision
======

* The vision is to make a shell which will significantly increase the productivity
  of system guys and girls.

* Current shells are outdated. They are not as powerful as we'd like them to be.

About this document
===================
This document started as internal draft. Some sections might no be clear. Still exposing it as per "release early" policy. Feel free to open a GitHub issue or email me directly: ilya (DOT) sher (AT) coding (DASH) knight (DOT) com

About project name
==================
"ze" is probably temporary.  It means "it" in Hebrew which can be a nice word play.


Features
========

UI
--
* Not to block, allow typing next commands even if previous command(s) still run(s).
	* Open issue: how to deal with a command that requires interaction.

* Provide good feedback. In GUI for example, this can be green / red icon near a completed command to show exit status. Tweaking prompt to include such info or typing "echo $?" all the time is not what I dream about.

* All operations made via a UI, including mouse operations in GUI _must_ have and display textual representation, allowing to copy / paste / save to a file/ send to friend.

* Different UI modules must exist. In the beginning we can start with these:
  * Web (allow multiple users to collaborate, some rw, some ro)
  * Console (use pty)

* Different syntax modules
	* With parse / construct features. Think an operation is done in a GUI, API called to
	  construct the command or whatever that is. Call "construct" on this to create textual
	  representation of the command / whatever.
	* On-the-fly syntax validation and errors reporting.

* Commands scroll up, new commands are added at the bottom. When a command that haven't
  completed yet, reaches top of the screen, it can be converted to a mini-area at the
  top (right?) of the screen, representing the command and current progress (and exit
  status later).

* [Later] Confirmation mode. One user in collaboration mode gives the command to execute,
  another user must approve the command for execution.

* Display structured results as real f\*cking structures (JSON, Yaml, ...)
	* (Maybe) Allow editing it and saving to file.
	* (Maybe) Allow write jq filters in (G)UI by selecting the elements

* GUI
	* Underline red/green for existing/non-existing files?

* Actions on objects that are on screen. Think right click / context menu.

* Commands history: among duplicate commands all but last should be grayed out,
  so that non-grayed out commands are unique.

* When hover over an object, highlight the same object everywhere it appears.

Cross-system
------------

* Feedback
	* Some new protocol is needed to convey process progress and other information
	* For external programs that don't support such protocol (currently all existing programs)
	  use heuristics such as look at open files + position in such files to guess the progress
	  with high probability.

* Manage multiple servers at once

	* Preferably using nothing more than standard SSH,
	  maybe uploading required parts automatically when a command needs to run.

	* Smart displaying of results,
	  such as "OK on all T servers", "OK on N servers, fail on M servers, out of total T servers"

	* Smart handling of failures,
	  maybe divide into groups depending on command status / output and then letting
	  to manage these groups.

	* Automatic server groups by
		* (In a cloud) a security group, a tag or other properties (regex on name for example)
		* (Configuration management: Chef/Puppe) by properties
		* (Locally) by /etc/hosts remark or by .ssh/config properties or remarks
		* by naming conventions (for example regex defined) for all cases above
		* Dynamic by a command output/exit code
		  Think "netstat -lpnt | grep -q :8000" or "pgrep java" or "dpkg -l '*apache*' >/dev/null"

* Easy integration with currently available software

	* Easy way to write hooks that will provide additional information
	  about running process, such as progress.

* Smart completion, context sensitive
	* Command switches and values
	* Complete objects (file names, urls, etc) depending on context.
	  Think "wget .../x.tgz", "tar " [COMPLETION_KEY] [Maybe some choice keys] -> " xzf x.tgz"
	* Maybe API to insert objects to completion history
	* Auto-detect completion history objects for existing commands (by wrappers and/or hooks probably)

* "Mentioned" completion
	* Complete objects from output of previous commands. Example: "apt-cache search ..." , "apt-get install ..."
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

Later / unformed / unfinished thoughts
--------------------------------------

* Measure and graph pipes throughput

* In a job, per process state / title

* Number of open files / sockets is part of the process status

* Interactive pipe construction: show intermediate results
	* only when requested (for example by a shortcut key) because commands can have side effects
	* white list of "safe" commands to show output

* Preview output with a shortcut key (don't run the command and insert it's output to the stream)

* Dynamic update command output, think "ps" / "top" when their output is in the stream
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
	* Each host should have statuses, such as "pending" for EC2 machines
	  (in "ze" can be pending till SSH connection is ready)

* Statuses should have "debug level" to them so that background jobs could be showed as less important

* Sessions. Total and global environment with it's own (saved) history etc. Think session per project or client.
  Option to share a session with other people. Open issue: history - common for session or per user
  (probably per user).

* Quick navigation. Search for session, host, host group, IP, history, etc.

* Icons (in any UI that allows it). Icons are processed much faster than text.

* Every failed script must have exact error information to it.
  No re-run with added "-x" or "echo"s should be required.

* Commands of "ze" will pass objects in pipes between them, not strings.
  External commands should use JSON or API (not sure here).

* For remote host or hosts group, give an option to execute command(s)
  as soon as the host is available. Notify when done, failed or timed out.

* In-line editor (maybe) for when a command line becomes few lines.
  Or quick way to take the long line and continue editing it in an
  editor.

* BIG: Arguments / etc. - description language. Think Javadoc.
	* Python (and other high level languages) is half-way there with argparse.
	  If a special comment is present meaning the script is "argparse safe",
	  it can be run with ze's replacement for argparse to inspect the arguments.
	* Auto discovery of file arguments: run a command with a unique value for
	  an argument and see if it tries to open such file. Tricky, Dangerous.
	* Auto discovery for jars? Think ec2 tools.
	* Anyway, there must be a way to specify externally argument types and
	  objects (file/url/pid... see bash doc about completion for more types)
	* Learn from interaction. Example "curl URL" -> curl has argument of type URL.
		* Provide easy access to modify/delete/blacklist learned commands' arguments.
		* Shortcut key to define object type. Example "curl [SHORTCUT_KEY_PRESSES]" ->
		  menu with object types -> remember the selection.
		* Use same format for learned and pre-defined arguments, allowing easy adding
		  to the ze package and interchange between people.
			* The format (future feature, low priority) will include version detection
			  and which switches are supported in which versions.
			* The format will include how to do completion for specific arguments.
			  Think "ec2kill" < "ec2din ...".

* Define which commands will run where when using hosts group. Think "ec2..." on
  a group of machines which include all ec2 machines: "management" machine, web, app,
  etc. servers.

* hosts groups should be organized in a stack ( pushd/popd style )

Current state
=============
Currently the project is at features/requirements gathering and design phase.

How to run POC
==============

Following instructions should work (tested on Debian)
* `cd small-poc`
* `mkdir ssl`
* `cd ssl`
* `openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout mysitename.key -out mysitename.crt`
* `cd ..`
* `npm install`
* `nodejs server.js`
* Go to <a href="https://127.0.0.1:8443/main.html">https://127.0.0.1:8443/main.html</a>
	* Commands to try:
		* `ls`
		* `pr` - a long process with progress bar
		* `sleep` - a process that sleeps for 5 seconds
		* `fail` - a progress that fails

Discussion / requests / comments
================================

* If you are totally unsure - open GitHub issues.
* Feel free to fork/edit/pull-request this document.
