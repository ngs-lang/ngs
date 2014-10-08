Ze shell architecture
=====================

Overview
--------

Core and plugins communicating via authenticated pub/sub using REST. (TCP and/or UNIX socket. I guess TCP first, UNIX socket later)


Core
----

Responsible for communicating between plugins.

* REST API
* Authentication / authorization
* Pub/Sub mechanism


Plugins
-------

### State plugin

Responsible for keeping current state:

* Servers and their groups
* Managed processes and their states
* Session
	* Commands history
	* ...

### Storage plugin

Responsible for storing the state between sessions (between shell process runs).

### IO plugins

* TTY
* Web

### Processes manager

Responsible for starting processes, reporting their progress, status and exit codes.

Additional responsibilities:

* Report CPU, disk IO, network IO utilization in real time (when requested?)
* Remote run support (where machines list to run on comes from cloud plugins, transparently)

### Cloud plugins

Responsible for keeping manipulating machines, disks, network, etc and tracking the status of all these resources and operations on them.
