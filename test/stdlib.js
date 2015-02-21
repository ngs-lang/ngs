// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var BUFFER_SIZE = 1024*1024;

var assert = require('assert');
var fs = require('fs');

var vm = require('../vm');
var nm = require('../vm-native-methods');
var compile = require('../compile').compile;

var code_snippets = [
	// __mul(s:String, n:Number)
	'{ "ab" * 3 == "ababab" }',
	// not(...)
	'{ not(true) == false }',
	'{ not(false) == true }',
	// __neq(...)
	'{ 1 != 2 }',
	// __eq(a:Array, b:Array)
	'{ [1,2] == [1,2] }',
	'{ not([1,2] == [2,3]) }',
	// __get_item(a:Array, idxs:Array)
	'{ t = [10,20,30][[1,2]]; t == [20,30] }',
	// min(...)
	'{ min(3,5) == 3 }',
	// max(...)
	'{ max(3,5) == 5 }',
	// startsWith(...)
	'{ startsWith("abc", "a") }',
	'{ not(startsWith("cd", "cde")) }',
	// in, not in
	'{ 1 in [0,1] }',
	'{ not(2 in [0,1]) }',
	'{ 2 not in [0,1] }',
	// read json
	'{ c = read("test/test.json"); c.did_it }',
	// spawn()
	'{ Bool($(ls)) }',
	'{ not(Bool($(-f NOSUCHFILE))) }',
	// functional
	'{ map([0,1,2], Bool) == [false, true, true] }',
	'{ all([7,8], __gt, 5)}',
	'{ any([7,8], __eq, 7)}',
	'{ none([7,8], __gt, 10)}',
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
				assert.deepEqual(c.stack, [['Bool', true]]);
				done();
			});
		});
	});
});

