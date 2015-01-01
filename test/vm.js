// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var assert = require('assert');

var vm = require('../vm');
var compile = require('../compile').compile;

var code_vs_stack = [
  ['{1}', [["Number",1]]],
  ['{1;2}', [["Number",2]]],
  ['{1+2}', [["Number",3]]],
  [' { 7 - 3 } ', [["Number",4]]],
  ['{[5]}', [["Array",[["Number",5]]]]],
  ['a = [1, 2]', [["Array",[["Number",1],["Number",2]]]]],
  ['{[1, 2] + [3, 4]}', [["Array",[["Number",1],["Number",2],["Number",3],["Number",4]]]]],
  ['{a=1; a}', [["Number",1]]],
  ['{ defun f() { return 77; }; 1 + f(); }', [["Number",78]]],
  ['{ defun f(x, y) { return x - y; }; f(5, 2); }', [["Number",3]]],
];

var code_vs_exec_args = [
  ['ls', ["Array",[["String","ls"]]]],
  ['a=["x", "y"]; ls zz $*a ww', ["Array",[["String","ls"],["String","zz"],["String","x"],["String","y"],["String","ww"]]]],
  ['{exec("blah");}', ["Array",[["String","blah"]]]],
];

// TODO: deduplicate tests code

code_vs_stack.forEach(function(code_stack, idx) {
  describe('Running code should result correct stack', function(){
	it('Code #' + idx + ': ' + code_stack[0].slice(0, 20), function(done) {
	  var v = new vm.VM();
	  var c = v.setupContext();
	  var code = compile(code_stack[0], {leave_value_in_stack: true}).compiled_code;
	  v.useCode(code);
	  v.start(function() {
		assert.deepEqual(c.stack, code_stack[1]);
		done();
	  });
	});
  });
  describe('Running code should result empty stack with leave_value_in_stack=false', function() {
	it('Code #' + idx + ': ' + code_stack[0].slice(0, 20), function(done) {
	  var v = new vm.VM();
	  var c = v.setupContext();
	  var code = compile(code_stack[0], {leave_value_in_stack: false}).compiled_code;
	  v.useCode(code);
	  v.start(function() {
		assert.deepEqual(c.stack, []);
		done();
	  });
	});
  });
});

code_vs_exec_args.forEach(function(code_args, idx) {
  describe('Running code should result correct exec arguments', function(){
	it('Code #' + idx + ': ' + code_args[0].slice(0, 20), function(done) {
	  var v = new vm.VM();
	  var c = v.setupContext();
	  c.registerMethod('exec', function(args) {
		console.log('exec args', args);
		assert.deepEqual(args, code_args[1]);
		return {'something': 'that', 'exec': 'returns'};
	  });
	  var code = compile(code_args[0]).compiled_code;
	  v.useCode(code);
	  v.start(function() {
		done();
	  });
	});
  });
});
