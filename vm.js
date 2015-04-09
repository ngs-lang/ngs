'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var _ = require('underscore');

var debug_match_params = !!process.env.NGS_DEBUG_PARAMS;
var debug_delay = !!process.env.NGS_DEBUG_DELAY;

var util = require('util');
var native_methods = require('./vm-native-methods');
var data = require('./vm-data');

var Args = native_methods.Args;
var to_ngs_object = native_methods.to_ngs_object;

for(var k in data) {
	global[k] = data[k];
}

function ReturnLater() {}

function Return(v) { this.v = v; }


function _repr_depth(depth) {
	var ret = '';
	for(var i=0;i<depth;i++) {
		ret = ret + '  ';
	}
	return ret;
}


function inspect(x, depth) {
	// TODO: output meta data
	// TODO: format for n columns
	depth = depth || 0;
	var t = get_type(x);
	// var pfx = _repr_depth(depth);
	var pfx = '';
	if(t == 'Number') {
		return pfx + get_num(x);
	}
	if(t == 'String') {
		return pfx + '"' + get_str(x) + '"';
	}
	if(t == 'Array') {
		var ret = pfx + '['
		var a = get_arr(x);
		for(var i=0; i<a.length; i++) {
			ret = ret + inspect(a[i], depth+1);
			if(i<a.length-1) {
				ret = ret + ', '
			}
		}
		ret = ret + pfx + ']'
		return ret;
	}
	if(t == 'Lambda') {
		var l = get_lmb(x);
		var params = get_arr(l.args);
		var s = [];
		var pt;
		for(var i=0; i<params.length; i++) {
			// TODO: arg_rest_pos & friends representation
			var param_type = get_type(get_arr(params[i])[2]);
			if(param_type == 'Null') {
				pt = '';
			} else {
				pt = ':' + get_str(get_arr(params[i])[2]);
			}
			s.push(get_str(get_arr(params[i])[0]) + pt);
		}
		var code = get_str(l.name) || 'anonymous';
		if(get_type(l.code_ptr) == 'NativeMethod') {
			code += '@native:' + get_nm(l.code_ptr).name;
		} else {
			code += '@' + get_num(l.code_ptr);
		}
		return pfx + '<Lambda '+ code +'(' +s.join(', ') + ')>';
		// console.log(params, );
	}
	if(t == 'Bool') {
		return pfx + String(get_boo(x));
	}
	if(t == 'Null') {
		return pfx + 'null\n';
	}

	if(t === 'Scopes') {
		return '<Scopes len ' + x.data.length + '>';
	}

	return pfx + '<' + t + ' ' + util.inspect(x.data) + '>';
}

function inspect_stack(stack) {
	var ret = [];
	for(var i=0; i<stack.length; i++) {
		ret = ret + String(i) + ': ' + inspect(stack[i]) + '\n';
	}
	return ret;
}

function Frame() {
	return this.initialize();
}

Frame.prototype.initialize = function() {
	this.ip = 0;
	this.scopes = [];
}

function Context(global_scope, cycles_limit) {
	return this.initialize(global_scope, cycles_limit);
}

var context_id = 1;

Context.prototype.initialize = function(global_scope, cycles_limit) {

	this.id = 'thread_' + (context_id++);
	this.state = 'running';
	this.stack = [];

	this.frame = new Frame();
	this.frame.scopes = [global_scope];
	this.frames = [this.frame];
	this.cycles = 0;
	this.cycles_limit = cycles_limit || null;
	this.thrown = false;
	this.waiting_for_me = [];
	this.thread_locals = {};
	this.meta = {};

	var get_context_ip = function() {
		return this.frame.ip;
	}.bind(this);

	// Don't want pop() method to be listed and printed in console.log()
	Object.defineProperty(this.stack, 'pop', {
		'value': function vm_pop() {
			if(this.length == 0) {
				throw new Error("Stack underflow at " + (get_context_ip()-1));
			}
			return Array.prototype.pop.call(this);
		}
	});

	if(!_.has(global_scope, 'inspect')) {
		// TODO: some better solution. Native methods are shared among contexts.
		//       Their regsitration should probably be done elsewhere.
		native_methods.register_native_methods.call(this);
		this.registerNativeMethod('inspect', native_methods.Args().pos('x', null).get(), function vm_inspect_p_any(scope) {
			return NgsValue('String', inspect(scope.x));
		});
	}

	return this;
}

Context.prototype.find_var_lexical_scope = function(varname) {
	var scopes = this.frame.scopes;
	var target = null;
	for(var i=scopes.length-1; i>=0; i--) {
		// Not very elegant target scope marker
		if((target === null) && (scopes[i] === true)) {
			target = i-1;
			continue;
		}
		if(Object.prototype.hasOwnProperty.call(scopes[i], varname)) {
			return [true, scopes[i]];
		}
	}
	if(target === null) {
		target = scopes.length - 1;
	}

	return [false, scopes[target]];
}

Context.prototype.get_var = function(name) {
	var r = this.find_var_lexical_scope(name);
	if(!r[0]) {
		this.thr(to_ngs_object(["programming", "Using undefined variable '" + name + "'"]));
		return;
	}
	return r[1][name];
}

Context.prototype.set_loc_var = function(name, val) {
	var var_scope = this.find_var_lexical_scope(name);
	if(var_scope[1] == this.frame.scopes[0]) {
		// Do not set global
		this.frame.scopes[this.frame.scopes.length-1][name] = val;
		return;
	}
	var_scope[1][name] = val;
}


Context.prototype.set_glo_var = function(name, val) {
	this.frame.scopes[0][name] = val;
}


Context.prototype.getCallerLexicalScopes = function() {
	// Should only be exposed to internal functions for security reasons.
	return this.frames[this.frames.length-2].scopes;
}

Context.prototype.context_finished = function(vm) {
	vm.finish_context();
	this.waiting_for_me.forEach(function(ctx) {
		vm.unsuspend_context(ctx);
	});
}

Context.prototype.wait = function(other_context, vm) {
	other_context.waiting_for_me.push(this);
	vm.suspend_context();
}

function VM() {
	return this.initialize();
}

VM.prototype.initialize = function() {
	this.code = [];
	this.global_scope = {};
	this.runnable_contexts = [];
	this.suspended_contexts = [];
	this.finished_contexts = [];
	this.context = null;
	this.finished = false;

	return this;
}

VM.prototype.useCode = function(c) {
	// TODO: assert that there are no cotext pointing to existing this.code
	//		 (which is being replaced)
	this.code.pop(); // remove the halt that we've added
	this.code = this.code.concat(c);
	this.code.push(['halt']);
	return this;
}

VM.prototype.useCodeWithRet = function(c) {
	var ptr = this.code.length;
	this.code = this.code.concat(c);
	this.code.push(['ret']);
	return ptr;
}

VM.prototype.setupContext = function(cycles_limit) {
	var c = new Context(this.global_scope, cycles_limit);
	this.runnable_contexts.push(c);
	return c;
}

VM.prototype.start = function(finished_callback) {
	this.finished_callback = finished_callback;
	this.mainLoop();
}

VM.prototype.mainLoop = function() {
	var stack_debug = process.env.NGS_DEBUG_STACK;
	while(this.runnable_contexts.length) {
		if(this.finished) {
			throw new Exception("Finished VM is not finished");
		}
		// console.log('DEBUG DELAY PRE', this.runnable_contexts.length);
		this.context = this.runnable_contexts[0];

		if(typeof(this.context.frame.ip) == 'string') {
			var f = this.context[this.context.frame.ip];
			if(!f) {
				throw new Error("Context method '" + this.context.frame.ip + "' not found");
			}
			f.call(this.context, this);
			continue;
		}

		var op = this.code[this.context.frame.ip];
		this.context.frame.ip++;
		if(stack_debug) {
			console.log('ST', inspect_stack(this.context.stack));
			console.log('FRAMES_N', this.context.frames.length);
			console.log('OP', op, '@', this.context.frame.ip-1, 'CYCLES', this.context.cycles);
			console.log('');
		}
		if(op[0] === 'comment') {
			continue;
		}
		if(!(op[0] in this.opcodes)) {
			throw new Error("Illegal opcode: " + op[0] + " at " + (this.context.frame.ip-1));
		}
		this.opcodes[op[0]].call(this, op[1]);
		this.context.cycles++;
		if(this.context.cycles_limit && (this.context.cycles > this.context.cycles_limit)) {
			// TODO: make it NGS exception. Still make sure works for tests
			throw new Error("Cycles limit reached: " + this.context.cycles_limit);
		}
		// Here, we might be with another stack, ip, etc

		// Warning: in future, some operations might change `this.context`
		if(debug_delay && this.context.state === 'running') {
			console.log('DEBUG DELAY', this.runnable_contexts.length);
			var ctx = this.context;
			this.suspend_context();
			setTimeout(function() {
				if(ctx.state === 'suspended') {
					// console.log('CALLING UNSUSPEND_CONTEXT');
					this.unsuspend_context(ctx);
				}
			}.bind(this), 100);
			break;
		}
	}
	if(!this.runnable_contexts.length && !this.suspended_contexts.length && !this.finished) {
		this.finished = true;
		this.finished_callback(this);
	}
	// console.log('NRC', this.runnable_contexts.length, this.suspended_contexts.length, this.finished_contexts.length);
}

VM.prototype.suspend_context = function() {
	// console.log('DEBUG DELAY B', this.runnable_contexts.length);
	var ctx = this.runnable_contexts.shift();
	if(!ctx) {
		throw new Error("VM.suspend_context: no runnable contexts.");
	}
	ctx.state = 'suspended';
	this.suspended_contexts.push(ctx);
	// console.log('suspend_context', ctx);
}

VM.prototype.suspend_context_till = function(obj, ev) {
	var vm = this;
	var ctx = this.runnable_contexts[0];
	function suspend_context_till_handler() {
		obj.removeListener(ev, suspend_context_till_handler);
		vm.unsuspend_context(ctx);
	}
	obj.on(ev, suspend_context_till_handler);
	vm.suspend_context();
}

VM.prototype.finish_context = function() {
	var ctx = this.runnable_contexts.shift();
	if(!ctx) {
		throw new Error("VM.finish_context: no runnable contexts.");
	}
	ctx.state = 'finished';
	this.finished_contexts.push(ctx);
}


VM.prototype.unsuspend_context = function(ctx, make_it_first) {
	var i = this.suspended_contexts.indexOf(ctx);
	if(i === -1) {
		if(this.runnable_contexts.indexOf(ctx) !== -1) {
			console.warn("VM.unsuspend_context() on context which is already runnable: " + ctx);
		} else {
			throw new Error("VM.unsuspend_context() on context which is not suspended: " + ctx);
		}
	}
	this.suspended_contexts.splice(i, 1);
	ctx.state = 'running';
	this.runnable_contexts[make_it_first?'unshift':'push'](ctx);
	setTimeout(function() {
		this.mainLoop();
	}.bind(this), 0);
}

Context.prototype.registerMethod = function(name, f, global) {
	var r;
	if(global) {
		var s = this.frame.scopes[0];
		r = [_.has(s, name), s];
	} else {
		r = this.find_var_lexical_scope(name);
	}
	if(!r[0]) {
		r[1][name] = NgsValue('Array', [f]);
		return;
	}
	r[1][name].data.push(f);
}

Context.prototype.registerNativeMethod = function(name, args, f) {
	var m = NgsValue('Lambda', {
		scopes: NgsValue('Scopes', this.frame.scopes),
		args: args,
		code_ptr: NgsValue('NativeMethod', f),
		name: NgsValue('String', name),
	});
	this.registerMethod(name, m);
}

Context.prototype.type_types = function(type_name, ret) {
	var ret = ret || {};
	ret[type_name] = true;
	var __TYPES = get_hsh(this.get_var('__TYPES'));
	if(!_.has(__TYPES, type_name)) {
		return ret;
	}
	var type_ = get_hsh(__TYPES[type_name]);
	if(!_.has(type_, 'inherits')) {
		return ret;
	}
	var inherits = get_arr(type_['inherits']);
	inherits.forEach(function(i) { this.type_types(get_str(i), ret) }.bind(this));
	// console.log('type_types', type_name, ret);
	return ret;
}

Context.prototype.is_callable = function(v, max_depth_one) {
	var _types = this.type_types(get_type(v));
	if(_.has(_types, 'Lambda')) {
		return true;
	}
	if(max_depth_one) {
		return false;
	}
	if(_.has(_types, 'Array')) {
		// XXX: Not very correct, might have user-defined __get_item()
		var a = get_arr(v);
		if(a.length == 0) {
			return false;
		}
		var elt = a[a.length-1];
		return this.is_callable(elt, true);
	}
}

function match_params(ctx, lambda, args, kwargs) {
	var l = get_lmb(lambda);
	if(l.args instanceof Args) {
		throw new Error("You forgot to use .get() in the end of Args().x().y().z() sequence when defining: " + l)
	}
	var params = get_arr(l.args);
	var scope = {};
	var positional_idx = 0;
	if(debug_match_params) {
		console.log('match_params args', args);
		console.log('match_params kwargs', kwargs);
		console.log('match_params params', util.inspect(params, {depth: 20}));
	}

	var p = get_arr(args);
	var n = get_hsh(kwargs);
	for(var i=0; i<params.length; i++) {
		// params: 0:name, 1:mode, 2:type, 3:default_value
		var cur_param = get_arr(params[i]);
		var cur_param_name = get_str(cur_param[0]);
		var cur_param_mode = get_str(cur_param[1]);
		var cur_param_default;
		// console.log('D', cur_param_default, cur_param);

		if(cur_param_mode === 'arg_rest_pos') {
			scope[cur_param_name] = NgsValue('Array', p.slice(i));
			positional_idx += (p.length - i);
			break;
		}
		if(cur_param_mode == 'arg_nam') {
			if(p.length-1 < positional_idx) {
				scope[cur_param_name] = cur_param[3]; // param default value
				continue;
			}
			cur_param_mode = 'arg_pos';
		}
		if(cur_param_mode == 'arg_pos') {
			var cur_param_type;

			if(p.length-1 < positional_idx) {
				return [false, {}, 'not enough pos args'];
			}

			if(get_type(cur_param[2]) !== 'Null') {
				cur_param_type = get_str(cur_param[2]);
			} else {
				cur_param_type = null;
			}

			if(cur_param_type) {
				var tt = ctx.type_types(get_type(p[positional_idx]));
				if(cur_param_type == 'F') {
					if(!ctx.is_callable(p[positional_idx])) {
						return [false, {}, 'pos args type mismatch at ' + positional_idx];
					}
				} else {
					if(!_.has(tt, cur_param_type)) {
						return [false, {}, 'pos args type mismatch at ' + positional_idx];
					}
				}
			}
			scope[cur_param_name] = p[positional_idx++];
		}
	}
	if(p.length > positional_idx) {
		return [false, {}, 'too much pos args'];
	}
	return [true, scope, 'all matched'];
}

Context.prototype.invoke_or_throw = function(methods, args, kwargs, vm, do_catch) {
	var status = this.invoke(methods, args, kwargs, vm, do_catch || false);
	if(!status[0]) {
		console.log(args);
		var types = get_arr(args).map(get_type);
		throw new Error("Invoke: appropriate method for types (" + types.join(',') + "), args " + inspect(args) + ", kwargs " + inspect(kwargs) + " not found for in " + inspect(methods));
	}
}


Context.prototype.invoke = function(methods, args, kwargs, vm, do_catch) {
	var ms;
	if(get_type(methods) == 'Lambda') {
		ms = [methods]
	} else {
		ms = get_arr(methods);
	}

	for(var l=ms.length-1, i=l; i>=0; i--) {
		var m = ms[i];

		var lambda = get_lmb(m);
		var scope = match_params(this, m, args, kwargs);
		if(!scope[0]) {
			continue;
		}
		var call_type = get_type(lambda.code_ptr);

		// __super, __args, __kwargs for new frame
		// TODO: make these invalid arguments
		var vars = scope[1];
		vars['__super'] = NgsValue('Array', ms.slice(0, i));
		vars['__args'] = args;
		vars['__kwargs'] = kwargs;

		var old_frame = this.frame;
		this.frame = new Frame();
		this.frame.do_catch = do_catch || false;
		this.frames.push(this.frame);
		this.frame.scopes = get_scp(lambda.scopes).concat(vars)
		old_frame.stack_len = this.stack.length;


		if(call_type === 'Number') {
			this.frame.ip = get_num(lambda.code_ptr);
			return [true];
		}
		if(call_type === 'NativeMethod') {
			var nm = get_nm(lambda.code_ptr);
			// console.log('NATIVEMETHOD', nm);
			try {
				this.thrown = false;
				var v = nm.call(this, scope[1], vm);
				if(!this.thrown) {
					this.stack.push(v);
					this.ret(1);
				}
			} catch(e) {
				if(e instanceof ReturnLater) {
					// all good
				} else {
					throw e;
				}
			}
			return [true];
		}
		throw new Error("Don't know how to call matched method: " + m);
	}

	return [false, 'no_method'];

}

Context.prototype.ret = function(stack_delta) {
	var deeper_frame = this.frames.pop();
	this.frame = this.frames[this.frames.length - 1];
	if(stack_delta === null) {
	} else {
		if(this.stack.length != this.frame.stack_len + stack_delta) {
			console.log('STACK', this.stack.length, this.frame.stack_len, stack_delta);
			throw new Error("Returning with wrong stack size");
		}
	}
	if(deeper_frame.do_catch) {
		var v = this.stack.pop();
		this.stack.push(NgsValue('Array', [to_ngs_object(true), v]));
	}
}

Context.prototype.thr = function(v) {
	this.thrown = true;
	while(this.frames.length > 0) {
		var deeper_frame = this.frames.pop();
		this.frame = this.frames[this.frames.length - 1];
		if(deeper_frame.do_catch) {
			this.stack.length = this.frame.stack_len;
			this.stack.push(NgsValue('Array', [to_ngs_object(false), v]));
			return;
		}
	}
	throw new Error("Uncaught exception" + inspect(v));
}

Context.prototype.guard = function(vm) {
	var methods = this.get_var('__super');
	var args = this.get_var('__args');
	var kwargs = this.get_var('__kwargs');
	this.ret(0);
	var m = get_arr(methods);
	this.invoke_or_throw(methods, args, kwargs, vm);
}

VM.prototype.opcodes = {

	'halt': function() {
		this.context.state = 'finished';
		this.finished_contexts.push(this.runnable_contexts.shift());
	},

	'src_pos': function(offset) {
		// console.log(offset);
	},

	// stack: ... -> ... value
	'push': function(v) {
		// ideally v is a scalar
		this.context.stack.push(v);
	},

	// stack: ... -> ... ip of the next instruction
	'push_ip': function(v) {
		this.context.stack.push(NgsValue('Number', this.context.frame.ip));
	},

	'push_num': function(v) { this.context.stack.push(NgsValue('Number', v)); },
	'push_str': function(v) { this.context.stack.push(NgsValue('String', v)); },
	'push_arr': function(v) { this.context.stack.push(NgsValue('Array', [])); },
	'push_hsh': function(v) { this.context.stack.push(NgsValue('Hash', {})); },
	'push_nul': function(v) { this.context.stack.push(NgsValue('Null', null)); },
	'push_boo': function(v) { this.context.stack.push(NgsValue('Bool', v)); },

	// stack: ... value -> ...
	'pop': function() {
		this.context.stack.pop();
	},

	// stack: ... x -> ... x x
	'dup': function() {
		var st = this.context.stack;
		var v = st.pop();
		st.push(v);
		st.push(v);
	},

	'xchg': function() {
		var st = this.context.stack;
		var v1 = st.pop();
		var v2 = st.pop();
		st.push(v1);
		st.push(v2);
	},

	// stack: ... var_name -> ... var_value
	'get_var': function() {
		var st = this.context.stack;
		var name = get_str(st.pop());
		var v = this.context.get_var(name);
		if(this.thrown) {
			return;
		}
		this.context.stack.push(v);
	},

	// stack: ... value varname -> ...
	'set_loc_var': function() {
		var st = this.context.stack;
		var name = st.pop();
		var val = st.pop();
		name = get_str(name);
		this.context.set_loc_var(name, val);
	},

	// stack: ... value varname -> ...
	'set_glo_var': function() {
		var st = this.context.stack;
		var name = st.pop();
		var val = st.pop();
		name = get_str(name);
		this.context.set_glo_var(name, val);
	},

	// stack: ... args kwargs methods -> ... X
	'invoke': function() {
		var st = this.context.stack;
		var methods = st.pop();
		var kwargs = st.pop();
		var args = st.pop();
		this.context.invoke_or_throw(methods, args, kwargs, this, false);
	},

	// stack: ... args kwargs methods -> ... X
	'invoke_catch': function() {
		var st = this.context.stack;
		var methods = st.pop();
		var kwargs = st.pop();
		var args = st.pop();
		this.context.invoke_or_throw(methods, args, kwargs, this, true);
	},

	// stack: ... arg1 arg2 methods -> ... X
	'invoke2': function() {
		var st = this.context.stack;
		var methods = st.pop();
		var arg2 = st.pop();
		var arg1 = st.pop();
		var args = NgsValue('Array', [arg1, arg2]);
		this.context.invoke_or_throw(methods, args, NgsValue('Hash', {}), this);
	},
	'invoke3': function() {
		var st = this.context.stack;
		var methods = st.pop();
		var arg3 = st.pop();
		var arg2 = st.pop();
		var arg1 = st.pop();
		var args = NgsValue('Array', [arg1, arg2, arg3]);
		this.context.invoke_or_throw(methods, args, NgsValue('Hash', {}), this);
	},

	// stack: ... v -> ... v
	'ret': function() {
		var c = this.context;
		if(c.frames[c.frames.length-2].stack_len === c.stack.length) {
			// We need to return a value but there isn't one in the stack
			// return Null in these rare cases.
			c.stack.push(NgsValue('Null', null));
		}
		this.context.ret(1);
	},

	// stack: ... v -> ... v
	'thr': function() {
		this.context.thr(this.context.stack.pop());
	},

	// guard: ... v
	'guard': function() {
		var v = get_boo(this.context.stack.pop())
		if(v) {
			return;
		}
		this.context.guard(this);
	},

	'jump': function(offset) {
		this.context.frame.ip += offset;
	},

	'jump_if_true': function(offset) {
		var v = this.context.stack.pop();
		if(get_boo(v)) {
			this.context.frame.ip += offset;
		}
	},

	'jump_if_false': function(offset) {
		var v = this.context.stack.pop();
		if(!get_boo(v)) {
			this.context.frame.ip += offset;
		}
	},
};


exports.VM = VM;
