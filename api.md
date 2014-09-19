Ze API
======

Overview
--------

REST+JSON and maybe REST+plain.

Endpoint
--------

The ze shell API endpoint will be provided in `ZE_URL` environment variable.


Authentication
--------------

* `X-ze-auth` - authentication token, provided by `ZE_AUTH` environment variable.

Authentication will be for specific job so access could be granted for not-so-trusted/external/closed-source components.

Authorization
-------------

* TBD, fine grained, see Authentication above. Thinking token -> permissions map. Permissions could be URL pattern + methods.

Objects
-------

* Job
	* Types: command line, cloud operation, etc...

Format
------

Default: JSON (or maybe react to `Accept: application/json` header)

* Also support: `Accept: text/plain` header.

Polling
-------

TBD. Support some kind of polling for example for UI.

Versioning
----------

TBD. Have versioned URLs?

CORS
----

For web UI. TBD.

URLs
----

In order that we think of them.

	/ - returns valid routes' patterns (POST adds new?)

	/plugins - list of running plugins

	/auth/tokens [GET+POST] - list all tokens / create new token

	/auth/tokens/ID
		name - Human readable explanation
		rules - array of:
			method - GET/POST/...
			url - regex
			action - allow/deny
		created - timestamp
		expires - timestamp

	/jobs [GET+POST]
	/jobs/ID
	/jobs/ID/control
	/jobs/ID/control/start      [POST]
	/jobs/ID/control/pause      [POST]
	/jobs/ID/control/continue   [POST]
	/jobs/ID/control/terminate  [POST]
	/jobs/ID/cmd
	/jobs/ID/args
	/jobs/ID/status
		[TBD: maybe /jobs/ID/status/HOST]
		[TBD: how to externalize the ok and fail groups]
		id - sequential number (history number) ??
		started/paused/continued/finished/terminated/last_output - timestamp float
		cmd - structured data with commands, pipes, etc.
		state - starting/running/paused/done
		progress - 0..100 float
		exit_code - 0..255 int
		waiting_for_input: bool  (not sure whether can be implemented.
		                   things like read() system call are probably
						   visible outside but what about select() of
						   an event loop?) having no TTY fds is a good
						   clue for "false" here :)
	/jobs/ID/stdin              [POST]
	/jobs/ID/stdout
	/jobs/ID/stderr

	/scripts - API for uploading and running scripts
	/scripts/ID [POST]
	/scripts/ID/status
	/scripts/ID/text
	/scripts/ID/control/start      [POST]
	/scripts/ID/control/pause      [POST]
	/scripts/ID/control/continue   [POST]
	/scripts/ID/control/terminate  [POST]

	[TBD: graphical programs interface/embedding ... just a tought - start]
	/jobs/ID/xvfb
	/jobs/ID/events
	[TBD: graphical programs interface/embedding ... just a tought - end]

	/hosts [GET+POST]
	/hosts/ID
	/hosts/ID/control
	/hosts/ID/control/start      [POST]
	/hosts/ID/control/pause      [POST] ??
	/hosts/ID/control/continue   [POST]
	/hosts/ID/control/terminate  [POST]
	/hosts/ID/command
	/hosts/ID/status
		hostname [SEARCHABLE]
		state - running/paused/terminated
		groups - list of groups this host belongs to
		

	/hosts-groups [GET+POST]
	/hosts-groups/ID
		name [SEARCHABLE]

	/commands [GET+POST]
	/commands/CMD [SEARCHABLE]
		name [SEARCHABLE]
		description - short description [SEARCHABLE]
		arguments - structured info about arguments and their
		            completion (how to complete) [SEARCHABLE]
					[We'll have a lot of sh*t to eat here]
		manual [SEARCHABLE]

	/history - current session command history [GET+POST] [SEARCHABLE]
	/history/SEQ
		time - timestamp
		user? - user that started the command (will be needed for session
		        sharing later).
		object_type - command/file/url/etc.
		str

	/users - TBD

	/permissions - TBD
	/permissions/TOKEN - TBD

	/search - TBD

	/sessions [GET+POST]
	/sessions/ID

	/vars - session variables
		[TBD: stack for each var]
	/vars/VARNAME - [GET+PUT+DELETE]
		JSON object containing the variable value
		[TBD: interesting ways to get/set variables' sub-parts
		      such as accessing /vars/VARNAME/myInstances/0
			  also maybe support push() / pop() for arrays ]
	(special, automatic vars, maybe shortcuts will be implemented
	 by syntax modules)
	/vars/LAST_JOB_ID
	/vars/LAST_SCRIPT_ID
	/vars/SESSION_ID
	/vars/TOKEN_PATH [TBD]
