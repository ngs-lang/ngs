"""
Attempt to figure out the model
"""


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

    def __str__(self):
        return "<Command {0}>".format([str(a) for a in self.argv])


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


class HistoryList(object):
    pass


class EnteredHistoryList(HistoryList):
    pass


class MenthionedHistoryList(HistoryList):
    pass


j = Job()
j.withCommand(Command(argv=['ps', '-ef']))
j.withCommand(Command(argv=['grep', 'vim']))
# TODO: connect the commands with a pipe
print j
