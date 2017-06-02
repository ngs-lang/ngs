% NGS(1) NGS User Manual
% Ilya Sher,  Zeeev Glozman
% 2017

# NAME

ngstut - Simple Tutorial how to get started with NGS-

# LAUNCHING SCRIPTS 

There are multiple switches, possible, dont be affraid. The example will print the work example. The "-p" option will print the result of the entire function. 

**echo** "example" | **ngs** -e "data=read(); echo(data)" # will print "example" 
**echo** "example" | **ngs** -p "read()"                  # will print "example"
**echo** "example" | **ngs** -pj '{"data":  read()}'      # will print json format  

curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=5' | ngs -p "fetch().sha" # will print out the sha



**ngs** [**-e**|**-E**|**-p**|**-pi**|**-pj**|**-pl**|**-pjl**] *expression*

# DESCRIPTION

**ngs** is a Next GeNeRAtION Shell. It has two main parts: the language and the interactive shell.

This project contains the language part which is under development. The interactive part will be dealt with later. This manual covers both implemented and unimplemented parts.

# OPTIONS

Given *script_name* runs the script.

Using *expression* is equivalent to running a script that consists of `{` *expression* `}`.

**-e** evaluates the *expression*.

**-E** prevents loading of **stdlib.ngs** and evaluates the *expression*.

**-p** prints the resulting *expression* value in a human readable NGS format.

**-pi** prints `inspect(...)` (detailed information) of the resulting *expression* value.

**-pj** prints the resulting *expression* value as JSON.

**-pl** prints elements of the result, one per line (mnemonic "print lines")

**-pjl** prints elements of the result, one per line as JSON (mnemonic "print JSON lines")

