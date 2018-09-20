% NGSWHY(1) NGS User Manual
% Ilya Sher
% 2018

# NAME

ngswhy - Motivation behind Next Generation Shell.

# TLDR

Because systems engineers deserve better.

# NGS BIRTH

I was using bash and Python for systems administration tasks, each one where it was a better fit for the task at hand. Neither felt like the best possible tool for the job.

While bash is a reasonable choice for running processes and handling files, it has serious flaws. Top ones are missing data structures (beyond strings, flat lists, and flat hashes), poor error handling, and syntax. Regarding data structures, one might say "just use `jq`". My opinion: `jq` is not nearly as convenient as having data structures in the language.

Python, on the other hand, has data structures and proper error handling. I was using Python mainly for talking to AWS API (using `boto`). But Python is a general-purpose programming language. That means it's not focused on systems administration tasks. Running external commands and manipulating files is not nearly as convenient as in bash. Note that Ruby, Perl, and Go are similar to Python in this aspect. None of these languages can support system tasks as a language that would be built ground-up for system tasks.

I was looking at my scripts and scripts that were written by other people for systems administration and thinking: I can't believe it is the best we can do today, this is intolerable crap.

That was how I started working on NGS.

# OTHER SHELLS

Yes, I am aware that other people are also working on shells. I have seen many other projects. None of them was "Oh! That's exactly how I see the future of shells and systems programming; I should stop working on NGS and join this project".

# WHY NGS?

NGS is optimized by design to perform easily typical systems administration tasks. For common tasks NGS has either a syntax or a set of features to make them easy:

* Running external commands
	* Facility for easier construction of command line arguments (the `Argv` facility)
	* pipes and redirects
	* handling of external commands' exit codes
* Manipulating data, including tables
* Simple remote execution (not implemented yet)

# EXAMPLES

Running external command + data manipulation:

	ngs -pi '``aws ec2 describe-instances``.InstanceId'

	# Output:
	Array of size 37
	[0] = i-0a0xxxxxxxxxxxxxx
	[1] = i-04dxxxxxxxxxxxxxx
	...

You might want to process the output above with an external tool but it's in a human-readable and not-machine-parsable format. No problem, instead of the **-pi** (print inspect) use the **-pj** (print JSON) switch:

	ngs -pj '``aws ec2 describe-instances``.InstanceId'

	# Output:
	[ "i-0a0xxxxxxxxxxxxxx", "i-04dxxxxxxxxxxxxxx", ... ]

We all know that life is not that simple so here is a bit more complex situation where the field might or might not appear in the data (also outputting space delimited items):

	ngs -p '``aws ec2 describe-instances``.map({A.PublicIpAddress tor "-"}).join(" ")'

	# Output:
	52.58.XXX.XX 52.59.XX.XX 52.29.XXX.XX 52.57.XXX.XXX - 52.57.XX.XXX ...


# SEE ALSO

* [https://news.ycombinator.com/item?id=16276911](https://news.ycombinator.com/item?id=16276911)
* [https://ilya-sher.org/2017/07/07/why-next-generation-shell/](https://ilya-sher.org/2017/07/07/why-next-generation-shell/)
