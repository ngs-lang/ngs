#!/usr/bin/env python

"""
Attempt to figure out the model
"""

import subprocess


class Host(object):
    """ A host to execute commands on """


class CommandIO(object):
    pass


class CommandInput(CommandIO):
    pass


class CommandOutput(CommandIO):
    pass


class Command(object):

    def __init__(self, argv=None):
        self.host = None
        self.argv = argv
        self.exit_code = None
        self.pid = None
        self.env = None
        self.inputs = {}
        self.outputs = {}
        self.start_time = None
        self.end_time = None

    def __str__(self):
        return "<Command {0}>".format([str(a) for a in self.argv])

    def popen(self, **kw_args):
        return subprocess.Popen(self.argv, **kw_args)


class Pipe(object):

    def __init__(self, command_io_a, command_io_b):
        self.command_io_a = command_io_a
        self.command_io_b = command_io_b


class Job(object):

    def __init__(self, commands=[]):
        self.commands = commands

    def withCommand(self, c):
        self.commands.append(c)
        return self

    def __str__(self):
        return "<Job object at {0}, commands={1}>".format(
            hex(id(self)), [str(c) for c in self.commands])

    def run(self):
        commands_count = len(self.commands)
        last_popen = None
        for i, c in enumerate(self.commands):
            kw_args = {}
            if i < commands_count - 1:
                kw_args['stdout'] = subprocess.PIPE
            if i > 0:
                kw_args['stdin'] = last_popen.stdout
            last_popen = c.popen(**kw_args)
        last_popen.communicate()


class HistoryList(object):
    pass


class EnteredHistoryList(HistoryList):
    pass


class MenthionedHistoryList(HistoryList):
    pass


if __name__ == '__main__':
    j = Job()
    j.withCommand(Command(argv=['ps', '-ef']))
    j.withCommand(Command(argv=['grep', 'vim']))
    # TODO: connect the commands with a pipe
    print j
    j.run()
