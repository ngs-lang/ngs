// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var assert = require('assert');

var vm = require('../vm');
var compile = require('../compile').compile;

var code_vs_stack = [
  ['{1}', [1]],
  ['{1;2}', [2]],
  ['{1+2}', [3]],
  [' { 7 - 3 } ', [4]],
  ['a = [1, 2]', [[1,2]]],
  ['{[1, 2] + [3, 4]}', [[1,2,3,4]]],
];

var code_vs_exec_args = [
  ['ls', ['ls']],
  ['a=["x", "y"]; ls zz $*a ww', ['ls', 'zz', 'x', 'y', 'ww']],
];

// TODO: deduplicate tests code

code_vs_stack.forEach(function(code_stack, idx) {
  describe('Running code should result correct stack', function(){
	it('Code #' + idx + ': ' + code_stack[0].slice(0, 20), function(done) {
	  var v = new vm.VM();
	  var code = compile(code_stack[0]).compiled_code;
	  v.useCode(code);
	  v.start(function() {
		assert.deepEqual(v.context.stack, code_stack[1]);
		done();
	  });
	});
  });
});

code_vs_exec_args.forEach(function(code_args, idx) {
  describe('Running code should result correct exec arguments', function(){
	it('Code #' + idx + ': ' + code_args[0].slice(0, 20), function(done) {
	  var v = new vm.VM();
	  v.register_method('exec', function(args) {
		console.log('exec args', args);
		assert.deepEqual(args, code_args[1]);
	  });
	  var code = compile(code_args[0]).compiled_code;
	  v.useCode(code);
	  v.start(function() {
		done();
	  });
	});
  });
});
