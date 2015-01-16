// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var assert = require('assert');

var vm = require('../vm');
var nm = require('../vm-native-methods');
var compile = require('../compile').compile;

var code_vs_stack = [
	// * basics ***
	['{1}', [["Number", 1]]],
	['{1;2}', [["Number", 2]]],
	['{1+2}', [["Number", 3]]],
	[' { 7 - 3 } ', [["Number", 4]]],
	['{[5]}', [["Array", [["Number", 5]]]]],
	['a = [1, 2]', [["Array", [["Number", 1], ["Number", 2]]]]],
	['{[1, 2] + [3, 4]}', [["Array", [["Number", 1], ["Number", 2], ["Number", 3], ["Number", 4]]]]],
	['{a=1; a}', [["Number", 1]]],

	// *** defun ***
	['{ defun f() { return 77; }; 1 + f(); }', [["Number", 78]]],
	['{ defun f(x, y) { return x - y; }; f(5, 2); }', [["Number", 3]]],
	['{ defun f(x:String) { return 1; }; defun f(x:Number) { return 2; }; [f("a"), f(100)] }',
	 [["Array", [["Number", 1], ["Number", 2]]]]],

	// *** if ***
	['{ if{[]}{1}{2} }', [["Number", 2]]],
	['{ if{[7]}{1}{2} }', [["Number", 1]]],
	['{ if{0}{1} }', [["Null", null]]],

	// *** Bool() ***
	['{ [ 1 < 2, 2 < 1] }', [["Array", [["Bool", true], ["Bool", false]]]]],

	// *** while ***
	['{a = 0; r = []; while {a < 2} {push(r, a); a = a + 1;}; r;}', [["Array", [["Number", 0], ["Number", 1]]]]],
	['{a = 0; r = []; while not {1 < a} {push(r, a); a = a + 1;}; r;}', [["Array", [["Number", 0], ["Number", 1]]]]],

	// *** while - break ***
	['{a = 0; r = []; while {a < 2} {break; push(r, a); a = a + 1;}; r;}', [["Array", []]]],

	// *** while - continue ***
	['{a = 0; r = []; while {a < 5} {a = a + 1; if { a < 3 } {continue;}; push(r, a);}; r;}',
	 [["Array", [["Number", 3], ["Number", 4], ["Number", 5]]]]],

	// *** for/continue/break ***
	['{a = 0; r = []; for {a=0} {a<5} {a=a+1} {if{a==1} { continue; }; if{a==3} { break; }; push(r, a);}; r}',
	 [["Array", [["Number", 0], ["Number", 2]]]]],
	['{Bool($(ls))}', [["Bool", true]]],
	['{Bool($(ls NOSUCHFILE))}', [["Bool", false]]],

	[
		'{\n'+
			'	a = 1\n'+
			'	r = []\n'+
			'	for {a=0} {a\n'+
			'	<5} {a=a+1}\n'+
			'	{\n'+
			'			if {a==1} {\n'+
			'				continue\n'+
			'			}\n'+
			'		  if {a==3} { break }\n'+
			'		  push(r, a)\n'+
			'	}\n'+
			'	while\n'+
			'	{\n'+
			'		a<10\n'+
			'	} {\n'+
			'	  a =\n'+
			'	  a+1\n'+
			'	}\n'+
			'	push(r, a);\n'+
			'	push(r,\n'+
			'		if {1} {\n'+
			'	   	   100\n'+
			'		}\n'+
			'	)\n'+
			'	r\n'+
			'}\n',
		[["Array",[["Number",0],["Number",2],["Number",10],["Number",100]]]]
	],

	// *** parentheses ***
	['{1+2*3}', [['Number', 7]]],
	['{(1+2)*3}', [['Number', 9]]],

	// *** Same precedence operators proper order ***
	['{100-10-1}', [['Number', 89]]],
	['{1000-100-10-1}', [['Number', 889]]],
	['{1000-100+10-1}', [['Number', 909]]],

	// *** Arrays ***
	['{a=[]; a[1]=7; a}', [['Array', [['Null', null], ['Number', 7]]]]],
	['{a=[10,20,30,40]; b=[1]; a[b[0]]}', [['Number', 20]]],

	// *** Guard ***
	['{defun f(x) {return 1}; defun f(x) {guard {x==10}; return 20} [f(8), f(10)]}',
	 [["Array", [["Number", 1], ["Number", 20]]]]],
];

var code_vs_exec_args = [
	['ls', ["Array", [["String", "ls"]]]],
	['a=["x", "y"]; ls zz $*a ww', ["Array", [["String", "ls"], ["String", "zz"], ["String", "x"], ["String", "y"], ["String", "ww"]]]],
	['{exec("blah");}', ["Array", [["String", "blah"]]]],
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
			c.registerNativeMethod('exec', nm.Args().rest_pos('args').get(), function(scope) {
				// console.log('exec args', scope.args);
				assert.deepEqual(scope.args, code_args[1]);
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
