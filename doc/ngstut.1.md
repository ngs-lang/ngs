% NGS(1) NGS User Manual
% Ilya Sher,  Zeev Glozman (@zglozman)
% 2017

# NAME

ngstut - Simple Tutorial how to get started with NGS

# ABOUT THIS DOCUMENT

This is a not a replacement for proper NGS syntax training, but rather an attempt to create a short introduction with some cool code that you can cut-n-paste and modify. I will also try to make accents on things that were not obvious to me, which are some of the coolest shortest syntax in my opinion.

# NGS SWITCHES

-e CODE
: "Execute" -- Will just execute the expressions

-p CODE
: "Print" -- Will print out the result of the last expression

-pl CODE
: "Print Lines" -- Will print out the result of the last expression, element per line

-pj CODE
: "Print JSON" -- Will print out the result of the last expression in JSON format

-pjl CODE
: "Print JSON Lines" -- Will print out the result of the last expression, element per line, in JSON format (each line is a complete JSON)

-pi CODE
: "Print Inspected" -- Will print out the result of the last expression, with more information about it

Examples:

	echo "example" | ngs -p 'read()'
	echo "example" | ngs -e 'data=read(); echo(data)'
	echo "example" | ngs -pj '{"data":  read()}'

# Working with a JSON web service filters and iterators

One of the ways in which NGS shines is how it works with external web services, especially JSON based.

## Query GitHub and return the SHA signatures of the commits

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=5' | ngs -pl 'fetch().sha'

Output:

	b4bbde8a2ebb902bde4129b27c8be5b4290b19c9
	c3cfeebc0e1815359d6995634e7903b2c977a882
	367e90d1733b09c9873db15c8edae79603fd8d4c
	6a76589af548f18c954d1fb2998a099e1d6937d2
	f750d7cc025eeac943c86c492c736992a745bd17


`fetch()` will read and parse the JSON. Then we select **sha** property of the object.

## Query GitHub and return the SHA signatures of commits matching a criteria

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -e 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha.each(F(sha) echo(sha))'

which is the same as

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pl 'fetch().commit.filter(F(commit) commit.author.name.has("Serge")).tree.sha'

Output in both cases:

	8a47a4c41e5377fa647c8c9c70f35e05e4a846df
	472bbe87adbba7fb05ee67d76616e91f0c51707b

Here is how the output from `curl` looks like:


	[
	  {
	    "sha": "b4bbde8a2ebb902bde4129b27c8be5b4290b19c9",
	    "commit": {
	      "author": {
	        "name": "Ilya Sher",
	        "email": "xxxxxxxxxxxxxxxxxxxxxxxxxxx",
	        "date": "2017-07-13T20:47:20Z"
	      },
		  ...
		}
		...
	  },
	  {
	    "sha": "c3cfeebc0e1815359d6995634e7903b2c977a882",
	    "commit": {
	      "author": {
	        "name": "Qi Xiao",
	        "email": "xxxxxxxxxxxxxxxxxxxxxxxxxxx",
	        "date": "2017-07-10T23:31:44Z"
	      },
		  ...
		}
		...
	]

The function

	F(commit) commit.author.name.has("Serge")

returns `true` for commits made by Serge and `false` for others.

* `fetch()` will read and parse the JSON. The JSON is an array at top level so `fetch()` evaluates to an array at top level.
* Then we select **commit** properties from each element of the array. This requires explanation. `array.property` evaluates to `[array[0].property, array[1].property, ..., array[N].property]`.
* `filter()` filters the **commit** array, leaving only elements that the function above "approves".
* `.tree.sha` extracts the SHA of the commits
* `.each(F(sha) echo(sha))` prints each SHA on it's own line. This is piece of code is equivalent to using **-pl** switch instead of **-e** switch.


## Query GitHub and show commits matching a criteria

In this example we will iterate over all results of the filter of all commits belonging to a specific user, in our case 'Serge', and then execute `git show $sha` with the SHA on all the selected commits.

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -e 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha.each(F(sha) echo(`git show $sha`))'

## Query GitHub and show specific properties of selected commits

In this example we complicate it a little further, and we run over all the commits where `author.name` matches Serge, and then make a map, and output it to a string, or as shown in the second example, a JSON structure.

Command:

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pl 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) "${commit.author.name} -- ${commit.message}")'

Output:

	Serge Bruno -- Link directories
	Serge Bruno -- Add path /usr/local/include to CMakeList.txt


Command:

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pj 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) {"name": commit.author.name, "message":  commit.message})' | jq .

Output:

	[
	  {
	    "name": "Serge Bruno",
	    "message": "Link directories"
	  },
	  {
	    "name": "Serge Bruno",
	    "message": "Add path /usr/local/include to CMakeList.txt"
	  }
	]



	To make one JSON structure as an array.

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pj 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) {"name": commit.author.name, "message":  commit.message})'

	To make multiple JSON objects on each new line.


## Get X.509 certificate from an HTTPS site

In this example we will demonstrate predicate ranges, for example download a certificate from facebook.com using openssl, and then cut between "-----BEGIN CERTIFICATE-----" and  "-----END CERTIFICATE-----" lines.


Command (two dots for range):

	openssl s_client -connect www.facebook.com:443 -servername www.facebook.com  < /dev/null 2>/dev/null | ngs -pl 'read().lines()["-----BEGIN CERTIFICATE-----".."-----END CERTIFICATE-----"]'

Output:

	MIIH5DCCBsygAwIBAgIQDACZt9eJyfZmJjF+vOp8HDANBgkqhkiG9w0BAQsFADBw
	...
	EP0UhYknI1B6LBecJuj7jI26eXZdX35CYkpI/SZA9KK+OYKHh6vCxKqnRZ9ZQUOj
	XnIWKQeV5Hg=

Command (three dots for range):

	openssl s_client -connect www.facebook.com:443 -servername www.facebook.com  < /dev/null 2>/dev/null | ngs -pl 'read().lines()["-----BEGIN CERTIFICATE-----"..."-----END CERTIFICATE-----"]'

Output:

	-----BEGIN CERTIFICATE-----
	MIIH5DCCBsygAwIBAgIQDACZt9eJyfZmJjF+vOp8HDANBgkqhkiG9w0BAQsFADBw
	...
	EP0UhYknI1B6LBecJuj7jI26eXZdX35CYkpI/SZA9KK+OYKHh6vCxKqnRZ9ZQUOj
	XnIWKQeV5Hg=
	-----END CERTIFICATE-----


`array[range_start..range_end]` gives all elements after element that satisfies `range_start` and before the element that satisfies `range_end`. Both `range_start` and `range_end` can be functions, strings, regular expressions and more.


## Show last Phrack magazine issue

In previous examples we did short one liners, now we will write a little script, to download the latest issue of the Phrack magazine.

So it should be pretty self explanatory, but this shows a bit more of the special NGS magic. The first piece of NGS magic is the usage of the regex. `~` will return one match, and `~~` will return all matches.

		m = phrak ~ /href="(.*?)" title="Issues"/

So first we run curl to download the main webpage. Then we extract the links to the issues, and just in the layout of the page, the first link is the link to the last issue. `m[1]` will be equal to the link to the last issue.

Then we will get all matches to regex, identifying pages.

		pages = issue ~~ /href=".*?(issues\/\d+\/\d+.html)#article/


This returns one occurrence of the link from the HTML returned by the page.

Then we construct the URL with base URL + m[1], and perform another query. Then we will use ~~ to take all matching links.

		pages.map(X[1])

In this example we also introduced the usage of `pmap`, in my opinion its one of the most powerful features ever. Run a bunch of threads and wait for all of them to finish and returns the results in an array. Just awesome, for downloads, file processing tasks. I have used it this way with python scripts, etc.



	#!/usr/local/bin/ngs

	{
		base_url = "http://www.phrack.org/"
		phrak = `curl -s $base_url`
		m = phrak ~ /href="(.*?)" title="Issues"/
		issue = `curl -s "$base_url/${m[1]}"`;
		pages = issue ~~ /href=".*?(issues\/\d+\/\d+.html)#article/
		rel_pages_urls = pages.map(X[1])
		abs_pages_urls = base_url + rel_pages_urls
		pages_texts = abs_pages_urls.pmap(F(url)  `lynx -dump $url`);
		pages_texts % echo
	}



# Loops and iterators

## Simple loop

	ngs -e 'for(i=0;i<10;i+=1) echo(i*10)'

## Same for loop, for with shorter syntax

	ngs -e 'for(i;10) echo(i*10)'

## Same for loop with an expression

	ngs -e 'for(i;10) { j=i+1; echo(j*10) }'

## Create a Range, and launches each function for each element in the Range

`{ ... }` is lambda definition, which equivalent to `F(A = null, B = null, C = null) { ... }` .

	ngs -e '(0..10).each({echo(A*10)})'

## Create a Range inclusive of the number on the right

	ngs -e '(1...10).each({echo(A*10)})'

## Short loop

	ngs -e '10.each({echo(A*10)})'

`each` will call the inner function defined inside of '{}' with numbers from 0-10 (excluding 10). As explained above, the A parameter is automatically filled in.

## For ... in

	ngs -e 'arr = [1,2,3]; for elt in arr echo(elt)'
	ngs -e 'range = 1...3; for elt in range echo(elt)'

# Data manipulation

## filterk - filter hash key-value pairs by key

	ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty"}; h.filterk(Int)'

Output:

	Hash of size 2
	  [10] = ten
	  [20] = twenty


## filterv - filter hash key-value pairs by value

Command:

	ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty"}; h.filterv(Int)'

Output:

	Hash of size 2
	  [a] = 1
	  [b] = 2


Command:

	ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty", 30: "thirty"}; h.filterv(Str).filterv(/e/)'

Output:

	Hash of size 3
	  [c] = letter c
	  [10] = ten
	  [20] = twenty

# AWS

## Get more convenient data structures from AWS CLI - background

NGS has double backtick syntax. It means to run a command and parse the output. It can detect and parse JSON so you can run arbitrary commands that return JSON and have it parsed. For AWS CLI commands double backtick does more meaningful transformations of the data.

## Get more convenient data structures from AWS CLI - volumes example

AWS CLI commands return JSON data structures. Unfortunately, these are not very convenient to work with.

Command:

	aws ec2 describe-volumes

Output:

	{
	    "Volumes": [
	        {
	            "Size": 8,
	            "Encrypted": false,
	            "Iops": 100,
	            "SnapshotId": "snap-xxxxxxxx",
	            "VolumeType": "gp2",
	            "AvailabilityZone": "eu-central-1b",
	            "State": "available",
	            "Attachments": [],
	            "CreateTime": "2015-08-26T21:51:07.681Z",
	            "VolumeId": "vol-vvvvvvvv"
	        },
	        {
	            "Size": 8,
	            "Encrypted": false,
	            "Iops": 100,
	            "SnapshotId": "snap-yyyyyyyy",
				...
			},
			...
	}

The interesting information is in the "Volumes" of course. This is how NGS can help a bit: have the interesting information at the top level of your data structure

Command:

	ngs -pj '``aws ec2 describe-volumes``' | jq .

Output:

	[
	  {
	    "VolumeType": "gp2",
	    "State": "available",
	    "Iops": 100,
	    "VolumeId": "vol-vvvvvvvv",
	    "CreateTime": "2015-08-26T21:51:07.681Z",
	    "AvailabilityZone": "eu-central-1b",
	    "Encrypted": false,
	    "Attachments": [],
	    "SnapshotId": "snap-xxxxxxxx",
	    "Size": 8
	  },
	  {
	    "VolumeType": "gp2",
	    "State": "in-use",
		...
	   },
	 ...
	 ]

Suppose you want to do some more processing of this information. Maybe in bash. One JSON per line can help:

Command:

	ngs -pjl '``aws ec2 describe-volumes``'

Output:

	{ "State": "available", "Encrypted": false, "Size": 8, ... }
	{ "State": "in-use", "Encrypted": false, "Size": 8, ... }
	...


## Get more convenient data structures from AWS CLI - instances example

Even more unfortunately, the `aws ec2 describe-instances` CLI returns even less friendly data structure:

Command:

	aws ec2 describe-instances

Output:

	{
	    "Reservations": [
	        {
	            "Instances": [
	                {
	                    "State": {
	                        "Name": "stopped",
	                        "Code": 80
	                    },
						...
					}
					...
				]
			}
			...
		]
	}

With NGS, you get the contents of "Instances" at the top level:

Command:

	ngs -pj '``aws ec2 describe-instances``' | jq .

Output:

	[
	  {
	    "ProductCodes": [],
	    "ImageId": "ami-xxxxxxxx",
	    "PrivateDnsName": "ip-10-XX-XX-XX.eu-central-1.compute.internal",
	    "Hypervisor": "xen",
		...
	  },
	  ...
	]

Again, we can do the one JSON per line variant.

Command:

	ngs -pjl '``aws ec2 describe-instances``'

Output:

	{ "VpcId": "vpc-vvvvvvvv", "ProductCodes": [ ], "SubnetId": "subnet-aaaaaaaa", ... }
	{ "VpcId": "vpc-vvvvvvvv", "ProductCodes": [ ], "SubnetId": "subnet-bbbbbbbb", ... }
	...

## Get more convenient data structures from AWS CLI - tags example

We are getting at what bothered me for years. The tags. I hope it will take some of your pain away too :)

Command:

	ngs -pj '``aws ec2 describe-instances``[10].Tags'

Output:

	{ "status": "Setup done", "role": "insta-server", "env": "staging", "Name": "beame-login" }

Yes! Not a list of hashes that look like `{ "Key": ..., "Value": ... }` each, just one simple hash!


## Get all distinct roles of machines

Command:

	ngs -pl '``aws ec2 describe-instances``.Tags.filter("role" in X).role.uniq().sort()'

Output:

	...
	docker-builder
	edge
	elk
	graphing
	...

Explanation:

	``aws ec2 describe-instances``.Tags # Returns array of hashes. One hash per instance
	.filter("role" in X)                # Only keep instances that have "role" tag
	.role                               # Returns array of values of "role" tags

Alternative command:

	ngs -pl '``aws ec2 describe-instances``.Tags.get("role").uniq().sort()'

Explanation:

	.get("role")  # for all elements of the array on the left (array of tags hashes)
	              # it returns the value for "role", if it exists
				  # Note that using simply .role as in the previous example
				  # would throw exception for any instance without the "role" tag.
				  # That's why the previous example uses filter().

## NGS AWS library - background

I manipulate infrastructure using scripts (some parts using CloudFormation). To make handling infrastructure more convenient for me, I made NGS AWS library. Examples follow.

## NGS AWS library - list instances in all regions

The following command uses built-in parallelism to query all regions at once.

Command:

	ngs -pjl 'AWS::Instance(regions="*")'

Output:

	{ "Tags": { "Name": "edge", "status": "Setup done", ... }
	{ "LaunchTime": "2017-07-19T08:05:06.000Z", "PublicIpAddress": "35.XXX.XXX.XXX", ... }
	{ "LaunchTime": "2016-10-02T10:15:26.000Z", "PublicIpAddress": "52.YY.YY.YYY", ... }
	...


## NGS AWS library - ELB examples

Delete ELB:

	ngs -e 'AWS::Elb("prod-mobile-redirect").delete()'

Disconnect all instances from ELB:

	ngs -e 'AWS::Elb("dev-provision").converge(Instances=[])'

Make sure instances are connected to ELB.

	ngs -e 'AWS::Elb("dev-provision").converge(Instances=AWS::Instance({"env": "dev", "role": "provision"}).expect())

Delete unused ELBs:

	ngs -e 'AWS::Elb().reject(X.Instances).delete()'

	AWS::Elb()            # references all load balancers
	.reject(X.Instances)  # filters out load balancers that have instances attached to them
