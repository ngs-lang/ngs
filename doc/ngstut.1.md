% NGS(1) NGS User Manual
% Ilya Sher,  Zeev Glozman (@zglozman)

% 2017

# NAME

ngstut - Simple Tutorial how to get started with NGS-

# BASIC USAGE OF NGS SWITCHES 
This is a not a replacement for proper NGS syntax training, but rather an attemp to create a short introduction with some 
cool code that you can cut-n-paste and modify. I will also try to make an accents on things that were not obious to me, 
which are some of the coolest shortest synax in my opinion.

## Basics  
There are multiple switches, possible, don't be afraid. In this seciton we will focus on the two most useful concetps. 
	a. running a script 
	b. running it from command line for immidiate results. 

## Running short comamnds from command line 
	echo "example" | ngs -p 'read()'
	echo "example" | ngs -e 'data=read(); echo(data)' 
	echo "example" | ngs -pj '{"data":  read()}'

Understanding the "-p, -pl, -pj, e"

	-e  -- Will just execute the expressions
	-p  -- Will print out the result of the last expression 
	-pl -- Will print out the result of the last expression element by element  
	-pj -- Will print out the result of the last expression in json format

# Working with a JSON web service filters and iterators	

## Example 1:

One of the way in which NGS shines is in the way it can work with external web services, especially JSON based.
In the example below we will query github and return the sha signatures of the commits 
	
	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=5' | ngs -p 'fetch().sha'

fetch() will read and parse the json and then select **sha** elemenet from the object.

Lets complicate this a bit:
	
	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pl 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha' 
	

## Example 2
This example will first read and parse using the fetch() command then preform filter of the commit objects by calling filter function. 
The filter accepts a function that has to return true/false, then we add conditional logic by F(commit) commit.author.name.has("Serge") 
and take tree.sha and print it as a list because we launched it using "-pl" argument.

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -e 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha.each(F(sha) echo(sha))'
	
	same as 
	
	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -e 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha.each(echo)'

each accepts a funciton so echo is a function so it can accept it, which offers for a a little shorter syntax.


## Example 3 

In this example we will iterate over all results of the filter of all commits belonging to a specic user in our case 'Serge', and then execute `git show $sha` with the sha on all the selected commits. 
	
	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -e 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).tree.sha.each(F(sha) echo(`git show $sha`))'

## Example 4
In this example we complicate it a little further, and we run over all the commits where `author.name` matches Serge, and then make a map, and output it to a string, or as shown in the second example a json structure. 

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pl 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) "${commit.author.name} -- ${commit.message}")'	
	
	OR 

	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pj 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) {"name": commit.author.name, "message":  commit.message})' 

	To make one json structure as an array.	
	
	curl -s 'https://api.github.com/repos/ilyash/ngs/commits?per_page=2000' | ngs -pj 'fetch().commit.filter(F(commit) commit.author.name.has("Serge") ).map(F(commit) {"name": commit.author.name, "message":  commit.message})' 
	
	To make multiple json objects on each new line.


## Example 5

In this example we will demonstrate predicate conditions, for example download a certificate from facebook.com using openssl, and then cut the between "-----BEGIN CERTIFICATE-----" and  "-----END CERTIFICATE-----"

	openssl s_client -connect www.facebook.com:443 -servername www.facebook.com  < /dev/null | ngs -e 'read().lines()["-----BEGIN CERTIFICATE-----".."-----END CERTIFICATE-----"].join("\n").echo()'
	then we will join the string by eliminating the "\n" and print the result using .echo"


## Example 6 
The privous examples we did short one liners, now we will write a little script, to download the latest issue of the phrack maganzine. 
So it should be pretty self explanatory, but this shows a bit more of the special ngs magic. The first peace of NGS magic is the usage of the regex. `~` will return one match, and `~~` will return all matches. 

		m = phrak ~ /href="(.*?)" title="Issues"/

So first we run curl to download the the main webpage. Then we will extract the links to the issues, and just in the layout of the page, the first link is the link to the last issue. m[1] will be equal to the link to the last issue.  
Then we will get all matches to regex, identifing pages. 

		pages = issue ~~ /href=".*?(issues\/\d+\/\d+.html)#article/


this will return one occurence of the link from the HTML return by the page.
then we construct the url with baseurl + m[1], and perform another query. Then we will use ~~ to take all matching links.

		pages.map(X[1])

In this example we also introduced the usage of pmap, in my opinon its one of the most powerful features ever. Run a bunch of forks and waits for all of them to finish and returs the results in an array. Just awesome, for downloads, file proccessing tasks. I have used it this way with python scripts, etc. 



	#!/usr/local/bin/ngs

	{
	1:		base_url = "http://www.phrack.org/"
	2:		phrak = `curl -s $base_url`
	3:		m = phrak ~ /href="(.*?)" title="Issues"/
	4:		issue = `curl -s "$base_url/${m[1]}"`;
	5:		pages = issue ~~ /href=".*?(issues\/\d+\/\d+.html)#article/
	6:		rel_pages_urls = pages.map(X[1])
	7:		abs_pages_urls = base_url + rel_pages_urls
	8:		pages_texts = abs_pages_urls.pmap(F(url)  `lynx -dump $url`  );
	9:		pages_texts % echo  
	}



## Example 7

	ngs -pi 'CHARS.hexdigits_lowercase.each(echo)'
 
  [ascii_lowercase] = abcdefghijklmnopqrstuvwxyz
  [ascii_uppercase] = ABCDEFGHIJKLMNOPQRSTUVWXYZ
  [ascii_letters] = abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
  [digits] = 0123456789
  [hexdigits_uppercase] = 0123456789ABCDEF
  [hexdigits_lowercase] = 0123456789abcdef
  [octdigits] = 01234567
  [base64] = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
  [base64url] = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_


# Loop and iterators

## Simple loop
	ngs -e 'for(i=0;i<10;i+=1) echo(i*10)'


## Same for loop, for with shorter syntax
	ngs -e 'for(i;10) echo(i*10)'

## Same for loop with an expression 
	ngs -e 'for(i;10) { j=i+1; echo(j*10) }'

## Creates a Range, and launches each function,  { } is lamdbda defintion, which equivalent to F(A = null, B = null,C = null) 
	ngs -e '(0..10).each({echo(A*10)})'

## Creates a Range inclusive of the number on the right
	ngs -e '(1...10).each({echo(A*10)})'

## This is the shortest loop in the world.
	ngs -e '10.each({echo(A*10)})'

	What this actually does is it calls each 10 times, each will call the inner function defined inside of '{}' with numbers from 0-10, passing index as a parameter. 
	As explained above, the A parameter is automatically filled in. 
	
	{
		for(i;10){ 
			{ echo(A) }  
		}
	};

## 
ngs -e '"abc".each(echo)'

ngs -e 'arr = [1,2,3]; for elt in arr echo(elt)'

ngs -e 'arr = [1,2,3]; arr.each(echo)'

ngs -e 'range = 1...3; for elt in range echo(elt)'

ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty"}; h.filterk(Int)'

ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty"}; h.filterv(Int)'

ngs -pi 'h = {"a": 1, "b": 2, "c": "letter c", 10: "ten", 20: "twenty"}; h.filterv(Str).filterv(/e/)'
