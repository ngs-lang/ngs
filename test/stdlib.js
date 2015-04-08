// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var BUFFER_SIZE = 1024*1024;

var assert = require('assert');
var fs = require('fs');

var vm = require('../vm');
var nm = require('../vm-native-methods');
var compile = require('../compile').compile;

var code_snippets = [
	// string functions
	'{ "abcd" ~ 4 }',
	// __match(Array,Number)
	'{ [3,4] ~ 2 }',
	// not(...)
	'{ not(true) == false }',
	'{ not(false) == true }',
	// Bool(Array)
	'{ if [] {1}{2} == 2 }',
	'{ if [7] {1}{2} == 1 }',
	// Bool(String)
	'{ if "" {1}{2} == 2 }',
	'{ if "x" {1}{2} == 1 }',
	'{ Bool([]) == false }',
	'{ Bool({}) == false }',
	// __neq(...)
	'{ 1 != 2 }',
	// __eq(a:Array, b:Array)
	'{ [1,2] == [1,2] }',
	'{ not([1,2] == [2,3]) }',
	// __get_item()
	'{ t = [10,20,30][[1,2]]; t == [20,30] }',
	'{ t = [1,2,3,4][[-1,-2]]; t == [4, 3] }',
	// min(...)
	'{ min(3,5) == 3 }',
	// max(...)
	'{ max(3,5) == 5 }',
	// startsWith(...)
	'{ startsWith("abc", "a") }',
	'{ not(startsWith("cd", "cde")) }',
	// endsWith(...)
	'{ endsWith("abc", "c") }',
	'{ not(endsWith("abc", "bd")) }',
	// in, not in
	'{ 1 in [0,1] }',
	'{ not(2 in [0,1]) }',
	'{ 2 not in [0,1] }',
	'{ [0,1].has(1) }',
	'{ [0,1].has(2).not() }',
	'{ "abc".has("b") }',
	// read json
	'{ c = read("test/test.json"); c.did_it }',
	// spawn()
	'{ Bool($(ls)) }',
	'{ not(Bool($(-f NOSUCHFILE))) }',
	// functional
	'{ map([0,1,2], Bool) == [false, true, true] }',
	'{ map([0,1,2], lambda (x) {x+1} ) == [1, 2, 3] }',
	'{ map([3,4], F mymapper (x) {x*2} ) == [6, 8] }',
	'{ map([3,"a"], [F (x:Number) {1}, F(x:String) {"A"}]) == [1, "A"] }',
	'{ all([7,8], __gt, 5)}',
	'{ any([7,8], __eq, 7)}',
	'{ none([7,8], __gt, 10)}',
	'{ filter([1,2,3,4], __gt, 2) == [3,4] }',
	'{ filter([0,1,null,false]) == [0,1,false] }',
	'{ r=[]; [1,10].each(F(x) {r.push(x*2)}); r == [2,20] }',
	'{ [1,10].each(F(x) {0}) == [1,10] }',
	'{ h={"kk": 7, "xx": 8}; h[["kk"]] == {"kk": 7} }',
	'{ h={"kk": 7, "xx": 8}; h[["xx"]] != {"kk": 7} }',
	'{ Hash(["a","b"], "x") == {"a": "x", "b": "x"} }',
	'{ ["a", "b"].Hash(F(x) { x*2 }) == {"a": "aa", "b": "bb" } }', // also tests String * Number
	// meta
	'{ a=1; a.meta("x", 2); a.meta("y", 3) == a and a.meta("x") == 2 and a.has_meta("x") }',
	// String
	'{ String({"x": 7}) == "{x: 7}" }',
	'{ "abc.txt" - ".txt" == "abc" }',
	'{ " abc\\n\\n".strip() == "abc" }',
	'{ split("a::bc::d", "::") == ["a", "bc", "d"] }', // split(Seq, Seq), uses pos(Seq, Seq)
	'{ split("a bc de") == ["a", "bc", "de"] }', // split(String)
	'{ Number("123") == 123 }', // Number(String), uses native ord()
	// Pipe
	'{ ([1, 2] | X*2) == [2,4] }',
	'{ ({"a": "x"} | X[0] + X[1]) == ["ax"]}',
];

// console.log("+ Loading stdlib");
var f = fs.readSync(fs.openSync('stdlib.ngs', 'r'), BUFFER_SIZE);
var stdlib_code = compile(f[0].toString()).compiled_code;
// console.log("+ Loaded stdlib");

code_snippets.forEach(function(code_snippet, idx) {
	describe('Running code snippet', function(){
		it('Code #' + idx + ': ' + code_snippet.slice(0, 20), function(done) {
			var v = new vm.VM();
			var c = v.setupContext();
			var code = compile(code_snippet, {leave_value_in_stack: true}).compiled_code;
			v.useCode(stdlib_code);
			v.useCode(code);
			v.start(function() {
				assert.equal(c.stack.length, 1);
				assert.equal(c.stack[0].type, 'Bool');
				assert.equal(c.stack[0].data, true);
				done();
			});
		});
	});
});

