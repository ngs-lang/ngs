// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var BUFFER_SIZE = 1024*1024;

var assert = require('assert');
var fs = require('fs');

var vm = require('../vm');
var nm = require('../vm-native-methods');
var compile = require('../compile').compile;

var code_snippets = [
	'{ "ab" * 3 == "ababab" }',
	'{ t = [10,20,30][[1,2]]; t == [20,30] }',
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

