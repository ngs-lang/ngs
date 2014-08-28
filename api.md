Ze API
======

Overview. REST+JSON and maybe REST+plain.

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

Default: JSON.

* Also support: `...?format=plain` (or cookie maybe)

Polling
-------

TBD. Support some kind of polling for example for UI.

Versioning
----------

TBD. Have versioned URLs?

URLs
----

	/ - returns valid routes' patterns (POST adds new?)

	/jobs [GET+POST]
	/jobs/ID
	/jobs/ID/control
	/jobs/ID/control/start      [POST]
	/jobs/ID/control/pause      [POST]
	/jobs/ID/control/continue   [POST]
	/jobs/ID/control/terminate  [POST]
	/jobs/ID/command
	/jobs/ID/status
		seq - sequential number (history number) ??
		started/paused/continued/finished/terminated - timestamp float
		command - structured data with commands, pipes, etc.
		state - running/paused/done
		progress - 0..100 float
		exit_code - 0..255 int

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
