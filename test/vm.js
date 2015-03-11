// apt-get install mocha
// # brings version 1.20.1-1 on Debian

var _ = require('underscore');

var assert = require('assert');

var vm = require('../vm');
var nm = require('../vm-native-methods');
var compile = require('../compile').compile;

CYCLES_LIMIT = 5000; // Empirical: 485 is enough as of 2015-03

var code_vs_stack = [
	// * basics ***
	['{1}', [1]],
	['{1;2}', [2]],
	['{1+2}', [3]],
	[' { 7 - 3 } ', [4]],
	['{[5]}', [[5]]],
	['a = [1, 2]', [[1, 2]]],
	['{[1, 2] + [3, 4]}', [[1, 2, 3, 4]]],
	['{a=1; a}', [1]],

	// *** defun ***
	['{ defun f() { return 77; }; 1 + f(); }', [78]],
	['{ defun f(x, y) { return x - y; }; f(5, 2); }', [3]],
	['{ defun f(x:String) { return 1; }; defun f(x:Number) { return 2; }; [f("a"), f(100)] }',
	 [[1, 2]]],

	// *** if ***
	['{ if 0 {1} }', [null]],

	// *** Bool() ***
	['{ [ 1 < 2, 2 < 1] }', [[true, false]]],

	// *** while ***
	['{a = 0; r = []; while a < 2 {r.push(a); a = a + 1;}; r;}', [[0, 1]]],
	['{a = 0; r = []; while not 1 < a {push(r, a); a = a + 1;}; r;}', [[0, 1]]],

	// *** while - break ***
	['{a = 0; r = []; while a < 2 {break; push(r, a); a = a + 1;}; r;}', [[]]],

	// *** while - continue ***
	['{a = 0; r = []; while a < 5 {a = a + 1; if a < 3 {continue;}; push(r, a);}; r;}',
	 [[3, 4, 5]]],

	// *** for/continue/break ***
	['{a = 0; r = []; for(a=0;a<5;a=a+1) {if a==1 { continue; }; if a==3 { break; }; push(r, a);}; r}',
	 [[0, 2]]],

	// *** throw ... catch ***
	['{ catch (F() { 1 })() }', [[true,1]]],
	['{ catch (F() { throw 2 })() }', [[false, 2]]],

	// *** locals ***
	['{ x=1; y=1; l = {"x": 2}; f = (F() { y = 3; x = 4 }).locals(l); f(); [x, y, l["x"]]}',
	 [[1, 3, 4]]],

	['{spawn=native_spawn; Bool($(ls).wait())}', [true]],
	['{spawn=native_spawn; Bool($(ls NOSUCHFILE).wait())}', [false]],

	[
		'{\n'+
			'	a = 1\n'+
			'	r = []\n'+
			'	for ( a=0 ; a\n'+
			'	<5 ; a=a+1 ) \n'+
			'	{\n'+
			'			if a==1 {\n'+
			'				continue\n'+
			'			}\n'+
			'		  if a==3 { break }\n'+
			'		  push(r, a)\n'+
			'	}\n'+
			'	while\n'+
			'	\n'+
			'		a<10\n'+
			'	{\n'+
			'	  a =\n'+
			'	  a+1\n'+
			'	}\n'+
			'	push(r, a);\n'+
			'	push(r,\n'+
			'		if 1 {\n'+
			'	   	   100\n'+
			'		}\n'+
			'	)\n'+
			'	r\n'+
			'}\n',
		[[0,2,10,100]]
	],

	// *** parentheses ***
	['{1+2*3}', [7]],
	['{(1+2)*3}', [9]],

	// *** Same precedence operators proper order ***
	['{100-10-1}', [89]],
	['{1000-100-10-1}', [889]],
	['{1000-100+10-1}', [909]],

	// *** Arrays ***
	['{a=[]; a[1]=7; a}', [[null, 7]]],
	['{a=[10,20,30,40]; b=[1]; a[b[0]]}', [20]],

	// *** Hashes ***
	['{ {"a": 7, "b": 8} }', [{"a":7,"b":8}]],
	['{ h = { "k" : 7, "x": 99 }; h["k"]}', [7]],
	['{ { "k" : 77, "x": 99 }["k"] }', [77]],

	// *** Guard ***
	['{defun f(x) {return 1}; defun f(x) {guard x==10; return 20}; [f(8), f(10)]}',
	 [[1, 20]]],

	// *** Comments ***
	['{7 # mycomment1\n}', [7]],
	['{7 // mycomment2\n}', [7]],
	['# something', []],

	// *** Empty function ***
	['{defun f() { #xx\n}; f()}', [null]],

	// *** Match ***
	['{match(100) {(n:Number) {1} (s:String) {2}}}', [1]],
	['{match("X") {(n:Number) {1} (s:String) {2}}}', [2]],

	// *** __super ***
	['{defun f(x) {x*2}; defun f(y) { __super(y) * 3}; f(5)}', [30]],
	['{defun f(x) {x*2}; defun f(y) { __super(y) * 3}; defun f(z:String) { "DOESNT MATTER" }; f(5)}', [30]],

	// *** meta ***
	['{a=1; a.meta()["x"] = 8; [a, a.meta()["x"]]}', [[1,8]]],

	// *** Boolean operators ***
	['{ 0 and 2 }', [0]],
	['{ 1 and 2 }', [2]],
	['{ 0 or 2 }', [2]],
	['{ 1 or 2 }', [1]],

	// *** Thread ***
	['{ r=[]; t=thread(F() { r.push(1); }); r.push(2); t.wait(); r.len() }', [2]],
	['{ t=thread(F() { thread().locals()["x"] = 7 }); t.wait().locals()["x"]; }', [7]],

	// *** id ***
	['{ x=1; y=2; id(y) - id(x) > 0 and id(y) - id(x) < 10}', [true]], // As of 2015-03, the difference is 2

	// *** inheritance ***
	['{ __TYPES = {"T1": {"inherits": ["Hash"]}, "T2": {"inherits": ["T1"]}}; ' +
	 'defun init(x:T1) {x.__super(); x.a=7; x}; defun init(x:T2) {x.__super(); x.b=8; x}; o=obj("T2").init(); [o["a"], o["b"]]}', [[7,8]]],
	['{ __TYPES = {"T1": {"inherits": ["Array"]}}; o=obj("T1").init(); o[0] = 7; o[0]}', [7]],

];

var code_vs_spawn_args = [
	['ls', ["ls"]],
	['a=["x", "y"]; ls zz $*a ww', ["ls","zz","x","y","ww"]],
	['{spawn("blah");}', ["blah"]],
];

var how = [
	['Running code should result correct stack', true],
	['Running code should result empty stack with leave_value_in_stack=false', false],
];

function to_js_object(val) {
	if(val.type === 'Array') {
		return val.data.map(to_js_object);
	}
	if(val.type === 'Hash') {
		var ret = {}
		_.keys(val.data).forEach(function (k) { ret[k] = to_js_object(val.data[k]) });
		return ret;
	}
	return val.data;
}

code_vs_stack.forEach(function(code_stack, idx) {
	how.forEach(function(h) {
		describe(h[0], function(){
			it('Code #' + idx + ': ' + code_stack[0].slice(0, 20), function(done) {
				var v = new vm.VM();
				var c = v.setupContext(CYCLES_LIMIT);
				var code = compile(code_stack[0], {leave_value_in_stack: h[1]}).compiled_code;
				v.useCode(code);
				v.start(function() {
					assert.deepEqual(c.stack.map(to_js_object), h[1] ? code_stack[1] : []);
					done();
				});
			});
		});
	});
});

code_vs_spawn_args.forEach(function(code_args, idx) {
	describe('Running code should result correct spawn arguments', function(){
		it('Code #' + idx + ': ' + code_args[0].slice(0, 20), function(done) {
			var v = new vm.VM();
			var c = v.setupContext(CYCLES_LIMIT);
			c.registerNativeMethod('spawn', nm.Args().rest_pos('args').get(), function(scope) {
				assert.deepEqual(to_js_object(scope.args), code_args[1]);
				return {'something': 'that', 'spawn': 'returns'};
			});
			var code = compile(code_args[0]).compiled_code;
			v.useCode(code);
			v.start(function() {
				done();
			});
		});
	});
});
