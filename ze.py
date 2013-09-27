class Command(object):

    def __init__(self, argv=None):
        self.argv = argv
        self.exit_code = None
        self.pid = None
        self.env = None

    def __str__(self):
        return "<Command {0}>".format([str(a) for a in self.argv])


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


j = Job().withCommand(Command(argv=['ps', '-ef'])).withCommand(Command(argv=['grep', 'vim']))
print j
