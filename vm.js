'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var _ = require('underscore');

var debug_match_params = !!process.env.NGS_DEBUG_PARAMS;
var profiling = !!process.env.NGS_PROFILE;

var util = require('util');
var native_methods = require('./vm-native-methods');
var data = require('./vm-data');

var Args = native_methods.Args;
var to_ngs_object = native_methods.to_ngs_object;

for(var k in data) {
	global[k] = data[k];
}

function _repr_depth(depth) {
	var ret = '';
	for(var i=0;i<depth;i++) {
		ret = ret + '  ';
	}
	return ret;
}


function Frame() {
	return this.initialize();
}

Frame.prototype.initialize = function() {
	this.ip = 0;
	this.prev_ip = 0;
	this.scopes = [];
	this.func_name = null;
}

function Context(vm, global_scope, cycles_limit) {
	return this.initialize(vm, global_scope, cycles_limit);
}

var context_id = 1;

Context.prototype.initialize = function(vm, global_scope, cycles_limit) {

	this.vm = vm;
	this.id = 'thread_' + (context_id++);
	this.state = 'new';
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

	var ctx = this;

	// Don't want pop() method to be listed and printed in console.log()
	Object.defineProperty(this.stack, 'pop', {
		'value': function vm_pop() {
			if(this.length == 0) {
				print_backtrace(ctx.get_backtrace());
				throw new Error("Stack underflow at " + (ctx.frame.prev_ip));
			}
			return Array.prototype.pop.call(this);
		}
	});

	if(!_.has(global_scope, 'inspect')) {
		// TODO: some better solution. Native methods are shared among contexts.
		//       Their regsitration should probably be done elsewhere.
		native_methods.register_native_methods.call(this);
		this.registerNativeMethod('inspect', native_methods.Args(this.vm.types).pos('x', null).get(), function vm_inspect_p_any(scope) {
			return NgsValue(this.vm.types.String, this.vm.inspectNgsValue(scope.x));
		}.bind(this));
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
		this.thr(to_ngs_object(this.vm.types, ["programming", "Using undefined variable '" + name + "'"]));
		return NgsValue(this.vm.types.Null, null);
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

	this.opcodes_stats = {};
	this.match_params_stats = {'time':0, 'total': {'ok': 0, 'fail': 0}}
	this.types = {};

	var mytypes = this.types;
	function register_my_type(t) {
		mytypes[t.data.name] = t;
	}

	register_my_type(NgsType('Type', 'Type'));
	mytypes.Type.type = mytypes.Type;
	register_my_type(NgsType('F', mytypes.Type, []))
	register_my_type(NgsType('NativeMethod', mytypes.Type, []))

	types.forEach(function(t) {
		if(t !== 'Type') {
			register_my_type(NgsType(t, mytypes.Type, []));
		}
	});

	mytypes.Array.data.parents.push(mytypes.Seq);
	mytypes.String.data.parents.push(mytypes.Seq);
	mytypes.File.data.parents.push(mytypes.Path);

	return this;
}

VM.prototype.inspectNgsValue = function(x, depth) {
	// TODO: output meta data
	// TODO: format for n columns
	depth = depth || 0;
	// console.log('INSPECT/'+depth, x);
	var t = get_type(x);
	// console.log('TYPE', t);
	// var pfx = _repr_depth(depth);
	var pfx = '';
	if(t == this.types.Number) {
		return pfx + get_num(x);
	}
	if(t == this.types.String) {
		var s = get_str(x);
		if(s.length > 128) {
			s = s.slice(0, 128) + '...';
		}
		return pfx + '"' + s + '"';
	}
	if(t == this.types.Array) {
		var ret = pfx + '['
		var a = get_arr(x);
		for(var i=0; i<a.length; i++) {
			ret = ret + this.inspectNgsValue(a[i], depth+1);
			if(i<a.length-1) {
				ret = ret + ', '
			}
		}
		ret = ret + pfx + ']'
		return ret;
	}
	if(t == this.types.Lambda) {
		var l = get_lmb(x);
		var params = get_arr(l.args);
		var s = [];
		var pt;
		for(var i=0; i<params.length; i++) {
			// TODO: arg_rest_pos & friends representation
			var param_type;
			if(get_arr(params[i])[2]) {
				param_type = get_type(get_arr(params[i])[2]);
			} else {
				param_type = this.types.Null;
			}
			if(param_type === this.types.Null) {
				pt = '';
			} else {
				// console.log('COLON', get_arr(params[i])[2]);
				pt = ':' + this.inspectNgsValue(get_arr(params[i])[2]);
			}
			s.push(get_str(get_arr(params[i])[0]) + pt);
		}
		var code = get_str(l.name) || 'anonymous';
		if(get_type(l.code_ptr) == this.types.NativeMethod) {
			code += '@native:' + get_nm(l.code_ptr).name;
		} else {
			code += '@' + get_num(l.code_ptr);
		}
		return pfx + '<Lambda '+ code +'(' +s.join(', ') + ')>';
		// console.log(params, );
	}
	if(t == this.types.Bool) {
		return pfx + String(get_boo(x));
	}
	if(t == this.types.Null) {
		return pfx + 'null';
	}
	if(t == this.types.Code) {
		return '<Code len ' + x.data.length + '>';
	}

	if(t === this.types.Scopes) {
		return '<Scopes len ' + x.data.length + '>';
	}

	if(t === this.types.Thread) {
		return '<Thread ' + x.data.id + '>';
	}

	if(t === this.types.Type) {
		return '<Type ' + x.data.name + '>';
	}

	if(t === 'Type') {
		return '<Type Type>';
	}

	_.keys(x.data).forEach(function(k) {
		console.log('K', k);
		console.log('V', x.data[k]);
	});
	console.log('TYPE', t, 'DATA', x.data);
	return pfx + '<' + this.inspectNgsValue(t) + ' ' + util.inspect(x.data) + '>';
}

VM.prototype.inspect_stack = function(stack) {
	var ret = [];
	for(var i=0; i<stack.length; i++) {
		ret = ret + String(i) + ': ' + this.inspectNgsValue(stack[i]) + '\n';
	}
	return ret;
}

VM.prototype._prepareCode = function(c) {
	var vm = this;
	return c.map(function prepare_op(op) {
		// console.log('OP', op);
		if(!(op[1] in vm.opcodes)) {
			throw new Error("Illegal opcode: " + op[1]);
		}
		var f = vm.opcodes[op[1]];
		f.opcode_name = op[1];
		return [op[0], f, op[2]];
	});
}

VM.prototype.useCode = function(c) {
	// TODO: assert that there are no cotext pointing to existing this.code
	//		 (which is being replaced)
	this.code.pop(); // remove the halt that we've added
	this.code = this.code.concat(this._prepareCode(c));
	this.code.push([null, this.opcodes.halt]);
	return this;
}

VM.prototype.useCodeWithRet = function(c) {
	var ptr = this.code.length;
	this.code = this.code.concat(this._prepareCode(c));
	this.code.push([null, this.opcodes.ret]);
	return ptr;
}

VM.prototype.makeContext = function(cycles_limit) {
	return new Context(this, this.global_scope, cycles_limit);
}

VM.prototype.setupContext = function(cycles_limit) {
	var c = this.makeContext(cycles_limit);
	c.state = 'running';
	this.runnable_contexts.push(c);
	return c;
}

VM.prototype.start = function(finished_callback) {
	this.finished_callback = finished_callback;
	this.mainLoop();
}

VM.prototype.mainLoop = function() {
	var stack_debug = process.env.NGS_DEBUG_STACK;
	var op, ip;
	while(this.runnable_contexts.length) {
		if(this.finished) {
			throw new Exception("Finished VM is not finished");
		}
		this.context = this.runnable_contexts[0];

		if(typeof(this.context.frame.ip) == 'string') {
			var f = this.context[this.context.frame.ip];
			if(!f) {
				throw new Error("Context method '" + this.context.frame.ip + "' not found");
			}
			f.call(this.context, this);
			continue;
		}

		ip = this.context.frame.ip;
		op = this.code[ip];
		this.context.frame.prev_ip = ip;
		this.context.frame.ip++;
		if(stack_debug) {
			// console.log('ST', this.inspect_stack(this.context.stack));
			// console.log('DEPTH', this.context.frames.length);
			console.log('OP', op[1].opcode_name, '@', this.context.frame.ip-1, this.context._ip_to_backtrace_item(this.context.frame.ip-1), 'CYCLES', this.context.cycles);
			console.log('');
		}
		this.context.thrown = false;
		if(profiling) {
			var opcode_name = op[1].opcode_name;
			this.opcodes_stats[opcode_name] = (this.opcodes_stats[opcode_name] || 0) + 1;
			var time1 = process.hrtime();
		}
		op[1].call(this, op[2]);
		if(this.context.stack.length && this.context.stack[this.context.stack.length-1] === 'Type') {
			this.context.stack[this.context.stack.length-1] = to_ngs_object(this.types, 'TYPE_WAS_HERE');
			this.context.thr(to_ngs_object(this.types, ['intrenal', 'Type in stack']));
			// throw new Error('FUCK');
		}
		if(profiling) {
			var opcode_name = op[1].opcode_name;
			var time2 = process.hrtime();
			var delta = (time2[0] * 1000000000 + time2[1]) - (time1[0] * 1000000000 + time1[1])
			this.opcodes_stats[opcode_name + '_t'] = (this.opcodes_stats[opcode_name + '_t'] || 0) + delta / 1000000000;
		}
		this.context.cycles++;
		if(this.context.cycles_limit && (this.context.cycles > this.context.cycles_limit)) {
			// TODO: make it NGS exception. Still make sure works for tests
			throw new Error("Cycles limit reached: " + this.context.cycles_limit);
		}
		// Here, we might be with another stack, ip, etc

	}
	if(!this.runnable_contexts.length && !this.suspended_contexts.length && !this.finished) {
		this.finished = true;
		this.finished_callback(this);
	}
}

VM.prototype.suspend_context = function() {
	var ctx = this.runnable_contexts.shift();
	if(!ctx) {
		throw new Error("VM.suspend_context: no runnable contexts.");
	}
	ctx.state = 'suspended';
	this.suspended_contexts.push(ctx);
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
		r[1][name] = NgsValue(this.vm.types.Array, [f]);
		return;
	}
	// New type system - start
	// console.log('TYPES', name, this.vm.types);
	if((r[1][name].type === this.vm.types.Type) || (r[1][name].type === 'Type')) {
		// Hack to allow methods with the same name as the type itself
		// such as String, Array, etc..
		r[1][name].data.init_methods.push(f);
		return;
	}
	// New type system - end
	// console.log('TYPE', r[1][name].type);
	r[1][name].data.push(f);
}

Context.prototype.registerNativeMethod = function(name, args, f) {
	var m = NgsValue(this.vm.types.Lambda, {
		scopes: NgsValue(this.vm.types.Scopes, this.frame.scopes),
		args: args,
		code_ptr: NgsValue(this.vm.types.NativeMethod, f),
		name: NgsValue(this.vm.types.String, name),
	});
	this.registerMethod(name, m);
}

Context.prototype.type_types = function(t, ret) {
	if(_.isString(t)) {
		throw new Error("Old style usage of type_types: " + t);
	}
	ret = ret || [];
	if(_.include(ret, t)) { return ret; }
	ret.push(t);
	t.data.parents.forEach(function(p) {
		this.type_types(p, ret);
	}.bind(this));
	return ret;
}

Context.prototype.is_callable = function(v, max_depth_one) {
	// console.log('TYPE_TYPES/FROM_IS_CALLABLE');
	var _types = this.type_types(get_type(v));
	// console.log('IS_CALLABLE', '_types', _types);
	if(_.contains(_types, this.vm.types.Lambda)) {
		return true;
	}
	// New type system - start
	if(_.contains(_types, this.vm.types.Type)) {
		return true;
	}
	// New type system - end
	if(max_depth_one) {
		return false;
	}
	if(_.contains(_types, this.vm.types.Array)) {
		// XXX: Not very correct, might have user-defined __get_item()
		var a = get_arr(v);
		if(a.length == 0) {
			return false;
		}
		var elt = a[a.length-1];
		return this.is_callable(elt, true);
	}
	return false;
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
	// var n = get_hsh(kwargs);
	for(var i=0; i<params.length; i++) {
		// params: 0:name, 1:mode, 2:type, 3:default_value
		var cur_param = get_arr(params[i]);
		var cur_param_name = get_str(cur_param[0]);
		var cur_param_mode = get_str(cur_param[1]);
		var cur_param_default;
		// console.log('D', cur_param_default, cur_param);

		if(cur_param_mode === 'arg_rest_pos') {
			scope[cur_param_name] = NgsValue(ctx.vm.types.Array, p.slice(i));
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

			if(get_type(cur_param[2]) !== ctx.vm.types.Null) {
				// console.log('CUR_PARAM/TYPE', cur_param);
				cur_param_type = cur_param[2];
			} else {
				cur_param_type = null;
			}

			if(cur_param_type) {
				if(cur_param_type === ctx.vm.types.F) {
					if(!ctx.is_callable(p[positional_idx])) {
						return [false, {}, 'pos args type mismatch at ' + positional_idx];
					}
				} else {
					var tt = get_type(p[positional_idx])
					if(cur_param_type !== tt) {
						tt = ctx.type_types(get_type(p[positional_idx]));
						if(!_.contains(tt, cur_param_type)) {
							return [false, {}, 'pos args type mismatch at ' + positional_idx];
						}
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
		// console.log(args);
		var vm = this.vm;
		var types = get_arr(args).map(get_type).map(function(t) {return vm.inspectNgsValue(t);});
		this.thr(to_ngs_object(this.vm.types, [
			'programming',
			"Invoke: appropriate method for types (" + types.join(',') + "), args " + this.vm.inspectNgsValue(args) + ", kwargs " + this.vm.inspectNgsValue(kwargs) + " not found for in " + this.vm.inspectNgsValue(methods)
		]));
	}
}


Context.prototype.invoke = function(methods, args, kwargs, vm, do_catch) {
	var ms;
	if(get_type(methods) == this.vm.types.Lambda) {
		ms = [methods]
	} else {
		if(get_type(methods) == this.vm.types.Type) {
			// Hack to allow methods with the same name as the type itself
			// such as String, Array, etc..
			ms = methods.data.init_methods;
		} else {
			// console.log('methods', methods);
			ms = get_arr(methods);
		}
	}

	for(var l=ms.length-1, i=l; i>=0; i--) {
		var m = ms[i];

		var lambda = get_lmb(m);
		if(profiling) {
			var match_params_time1 = process.hrtime();
		}
		var scope = match_params(this, m, args, kwargs);
		if(profiling) {
			var match_params_time2 = process.hrtime();
			var delta = (match_params_time2[0] * 1000000000 + match_params_time2[1]) - (match_params_time1[0] * 1000000000 + match_params_time1[1])
			this.vm.match_params_stats['time'] = (this.vm.match_params_stats['time'] || 0) + delta / 1000000000;

			this.vm.match_params_stats['total'][scope[0]?'ok':'fail']++;
			var match_params_iter = 'iter_' + (l-i+1);
			if(!this.vm.match_params_stats[match_params_iter]) {
				this.vm.match_params_stats[match_params_iter] = {'ok': 0, 'fail': 0}
			}
			this.vm.match_params_stats[match_params_iter][scope[0]?'ok':'fail']++;
		}
		if(!scope[0]) {
			continue;
		}
		var call_type = get_type(lambda.code_ptr);

		// __super, __args, __kwargs for new frame
		// TODO: make these invalid arguments
		var vars = scope[1];
		vars['__super'] = NgsValue(this.vm.types.Array, ms.slice(0, i));
		vars['__args'] = args;
		vars['__kwargs'] = kwargs;

		var old_frame = this.frame;
		this.frame = new Frame();
		this.frame.do_catch = do_catch || false;
		this.frames.push(this.frame);
		this.frame.scopes = get_scp(lambda.scopes).concat(vars)
		this.frame.func_name = get_str(lambda.name);
		this.frame.native_func_name = null;
		old_frame.stack_len = this.stack.length;


		if(call_type === this.vm.types.Number) {
			this.frame.ip = get_num(lambda.code_ptr);
			return [true];
		}
		if(call_type === this.vm.types.NativeMethod) {
			var nm = get_nm(lambda.code_ptr);
			this.frame.native_func_name = nm.name;
			var v = nm.call(this, scope[1], vm);
			if(!this.thrown) {
				this.stack.push(v);
				this.ret(1);
			}
			return [true];
		}
		throw new Error("Don't know how to call matched method: " + this.vm.inspectNgsValue(m));
	}

	return [false, 'no_method'];

}

Context.prototype.ret = function(stack_delta) {
	var deeper_frame = this.frames.pop();
	this.frame = this.frames[this.frames.length - 1];
	if(stack_delta === null) {
	} else {
		if(this.stack.length != this.frame.stack_len + stack_delta) {
			// console.log('STACK', this.stack.length, this.frame.stack_len, stack_delta);
			throw new Error(
				"Returning with wrong stack size. Expected: " +
					(this.frame.stack_len + stack_delta) +
					' Actual: ' +
					this.stack.length
			);
		}
	}
	if(deeper_frame.do_catch) {
		var v = this.stack.pop();
		this.stack.push(NgsValue(this.vm.types.Array, [to_ngs_object(this.vm.types, true), v]));
	}
}

Context.prototype._ip_to_backtrace_item = function(ip) {
	var vm = this.vm;
	if(ip === 0) {
		return '?'
	}
	var op = vm.code[ip];
	var debug_info = op[0]; // (diff to 'src_file', line, col)
	if(!debug_info) {
		return '?';
	}
	var src_file = vm.code[ip - debug_info[0]][2];
	return src_file + ':' + debug_info[1] + ':' + debug_info[2];
}

Context.prototype.get_backtrace = function() {
	// console.log('get_backtrace0', this.frames.length);
	return this.frames.map(function(frame) {
		if(frame.native_func_name) {
			return 'native:?:? (' + frame.func_name + ') (native ' + frame.native_func_name + ')';
		}
		var ret = this._ip_to_backtrace_item(frame.prev_ip);
		if(frame.func_name) {
			ret = ret + ' (' + frame.func_name + ')'
		}
		return ret;
	}.bind(this));
}

function print_backtrace(bt) {
	console.log('Backtrace (deepest frame last):')
	bt.forEach(function(frame_info) { console.log('  ' + frame_info) })
}

Context.prototype.thr = function(v) {
	this.thrown = true;
	var bt = this.get_backtrace();
	while(this.frames.length > 0) {
		var deeper_frame = this.frames.pop();
		this.frame = this.frames[this.frames.length - 1];
		if(deeper_frame.do_catch) {
			this.stack.length = this.frame.stack_len;
			this.stack.push(NgsValue(this.vm.types.Array, [to_ngs_object(this.vm.types, false), v]));
			return;
		}
	}
	print_backtrace(bt);
	console.log('Uncaught NGS exception:', this.vm.inspectNgsValue(v));
	process.exit(1);
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

	'comment': function() {},
	'src_file': function() {},

	// stack: ... -> ... value
	'push': function(v) {
		// ideally v is a scalar
		this.context.stack.push(v);
	},

	// stack: ... -> ... ip of the next instruction
	'push_ip': function(v) {
		this.context.stack.push(NgsValue(this.types.Number, this.context.frame.ip));
	},

	'push_num': function(v) { this.context.stack.push(NgsValue(this.types.Number, v)); },
	'push_str': function(v) { this.context.stack.push(NgsValue(this.types.String, v)); },
	'push_arr': function(v) { this.context.stack.push(NgsValue(this.types.Array, [])); },
	'push_hsh': function(v) { this.context.stack.push(NgsValue(this.types.Hash, {})); },
	'push_nul': function(v) { this.context.stack.push(NgsValue(this.types.Null, null)); },
	'push_boo': function(v) { this.context.stack.push(NgsValue(this.types.Bool, v)); },

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
		if(this.context.thrown) {
			this.context.thrown = false;
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
		var args = NgsValue(this.types.Array, [arg1, arg2]);
		this.context.invoke_or_throw(methods, args, NgsValue(this.types.Hash, {}), this);
	},
	'invoke3': function() {
		var st = this.context.stack;
		var methods = st.pop();
		var arg3 = st.pop();
		var arg2 = st.pop();
		var arg1 = st.pop();
		var args = NgsValue(this.types.Array, [arg1, arg2, arg3]);
		this.context.invoke_or_throw(methods, args, NgsValue(this.types.Hash, {}), this);
	},

	// stack: ... v -> ... v
	'ret': function() {
		var c = this.context;
		if(c.frames[c.frames.length-2].stack_len === c.stack.length) {
			// We need to return a value but there isn't one in the stack
			// return Null in these rare cases.
			c.stack.push(NgsValue(this.types.Null, null));
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
