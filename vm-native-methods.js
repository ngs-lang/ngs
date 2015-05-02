"use strict";

var fs = require('fs');
var tty = require('tty');
var child_process = require('child_process');
var util = require('util');

var _ = require('underscore');

var data = require('./vm-data');
var compile = require('./compile');

var process_ngs_id = 1;

function to_ngs_object(types, v, depth) {
	depth = depth || 0
	if(depth > 10) {
		throw new Error("to_ngs_object failed due to depth:" + v);
	}
	if(_.isNull(v)) {
		return NgsValue(types.Null, null);
	}
	if(_.isBoolean(v)) {
		return NgsValue(types.Bool, v);
	}
	if(_.isNumber(v)) {
		return NgsValue(types.Number, v);
	}
	if(_.isString(v)) {
		return NgsValue(types.String, v);
	}
	if(_.isArray(v)) {
		return NgsValue(types.Array, v.map(function(x) {
			return to_ngs_object(types, x, depth+1);
		}));
	}
	if(_.isObject(v)) {
		var h = {}
		_.keys(v).forEach(function(k) {
			h[k] = to_ngs_object(types, v[k], depth+1);
		});
		return NgsValue(types.Hash, h);
	}
	throw new Error("to_ngs_object() failed for " + v);
}

function Args(types) {
	if(!types) {
		throw new Error("Args(...) requires types");
	}
	if(!this || this == global) {
		return new Args(types);
	}
	this.args = [];
	this.types = types;
};


Args.prototype.general = function(name, typ, arg_type, dflt) {
	var r = [
		NgsValue(this.types.String, name),
		NgsValue(this.types.String, arg_type),
		typ ? typ : NgsValue(this.types.Null, null)
	];
	if(dflt) { r.push(dflt); }
	this.args.push(NgsValue(this.types.Array,r));
	return this;
}
Args.prototype.pos = function(name, typ) {
	return this.general(name, typ, 'arg_pos')
}
Args.prototype.rest_pos = function(name) {
	return this.general(name, this.types.Array /* not used */, 'arg_rest_pos');
}
Args.prototype.named = function(name, typ, dflt) {
	return this.general(name, typ, 'arg_nam', dflt);
}
Args.prototype.get = function() {
	return NgsValue(this.types.Array, this.args);
}

function p_args(types) {
	var ret = Args(types);
	for(var i=1; i<arguments.length; i+= 2) {
		ret.pos(arguments[i], arguments[i+1]);
	}
	return ret.get();
}

function register_native_methods() {
	// Expecting:
	// this - Context object

	var that = this;

	_.values(this.vm.types).forEach(function(t) {
		// console.log('REGISTERING', t.data.name);
		that.set_glo_var(t.data.name, t);
	});

	this.registerNativeMethod('Array', p_args(this.vm.types), function vm_Array() {
		return NgsValue(this.vm.types.Array, new Array());
	});

	this.registerNativeMethod('Hash', p_args(this.vm.types), function vm_Hash() {
		return NgsValue(this.vm.types.Hash, new Object());
	});

	this.registerNativeMethod('Bool', p_args(this.vm.types, 'x', null), function vm_Bool_p_any(scope) {
		// Anything that was not processed by any other Bool() method
		// will have it's JS boolean value as result.
		return NgsValue(this.vm.types.Bool, !!scope.x.data); // XXX broken get_TYP(v) data access abstraction
	});

	this.registerNativeMethod('Bool', p_args(this.vm.types, 'p', this.vm.types.Process), function vm_Bool_p_process(scope) {
		var p = get_prc(scope.p);
		if(p.state !== 'done') {
			// TODO: throw GuardYield or something like that,
			//		 indicating guard failure
			throw new Error("Can't Bool() unfinished process");
		}
		// exit_code can be null if process was terminated by a signal
		return NgsValue(this.vm.types.Bool, p.exit_code === 0);
	});

	this.registerNativeMethod('__get_attr', p_args(this.vm.types, 'o', null, 'attr', this.vm.types.String), function vm___get_attr_p_any_str(scope) {
		var a = get_str(scope.attr);
		var o = scope.o.data;
		if(!_.has(o, a)) {
			this.thr(to_ngs_object(this.vm.types, ['runtime', "Attribute not found: " + a]));
		}
		return o[a];
	})

	this.registerNativeMethod('__set_attr', p_args(this.vm.types, 'o', null, 'attr', this.vm.types.String, 'v', null), function vm___set_attr_p_any_str_any(scope) {
		var a = get_str(scope.attr);
		var o = scope.o.data;
		o[a] = scope.v;
		return scope.o;
	})

	this.registerNativeMethod('__get_attr', p_args(this.vm.types, 't', this.vm.types.Type, 'attr', this.vm.types.String), function vm___get_attr_p_any_str(scope) {
		var a = get_str(scope.attr);
		var t = scope.t.data;
		if(a==='name') {
			return to_ngs_object(this.vm.types, t.name)
		}
		this.thr(to_ngs_object(this.vm.types, ['runtime', 'Getting type attributes other then name is not supported yet']));
		return scope.t;
	})

	this.registerNativeMethod('__set_attr', p_args(this.vm.types, 't', this.vm.types.Type, 'attr', this.vm.types.String, 'v', null), function vm___set_attr_p_any_str_any(scope) {
		this.thr(to_ngs_object(this.vm.types, ['runtime', 'Setting type attributes is not supported yet']));
		return scope.t;
	})

	this.registerNativeMethod('push', p_args(this.vm.types, 'a', this.vm.types.Array, 'x', null), function vm_push(scope) {
		var a = get_arr(scope.a);
		a.push(scope.x);
		return scope.a;
	});

	this.registerNativeMethod('__add', p_args(this.vm.types, 'a', this.vm.types.Number, 'b', this.vm.types.Number), function vm___add_p_num_num(scope) {
		return NgsValue(this.vm.types.Number, get_num(scope.a) + get_num(scope.b));
	});

	this.registerNativeMethod('__add', p_args(this.vm.types, 'a', this.vm.types.String, 'b', this.vm.types.String), function vm___add_p_str_str(scope) {
		return NgsValue(this.vm.types.String, get_str(scope.a) + get_str(scope.b));
	});

	this.registerNativeMethod('__add', p_args(this.vm.types, 'a', this.vm.types.Array, 'b', this.vm.types.Array), function vm___add_p_arr_arr(scope) {
		return NgsValue(this.vm.types.Array, get_arr(scope.a).concat(get_arr(scope.b)));
	});

	this.registerNativeMethod('__sub', p_args(this.vm.types, 'a', this.vm.types.Number, 'b', this.vm.types.Number), function vm___sub(scope) {
		return NgsValue(this.vm.types.Number, get_num(scope.a) - get_num(scope.b));
	});

	this.registerNativeMethod('__mul', p_args(this.vm.types, 'a', this.vm.types.Number, 'b', this.vm.types.Number), function vm___sub(scope) {
		return NgsValue(this.vm.types.Number, get_num(scope.a) * get_num(scope.b));
	});

	this.registerNativeMethod('__lt', p_args(this.vm.types, 'a', this.vm.types.Number, 'b', this.vm.types.Number), function vm___lt(scope) {
		return NgsValue(this.vm.types.Bool, get_num(scope.a) < get_num(scope.b));
	});
	this.registerNativeMethod('__gt', p_args(this.vm.types, 'a', this.vm.types.Number, 'b', this.vm.types.Number), function vm___gt(scope) {
		return NgsValue(this.vm.types.Bool, get_num(scope.a) > get_num(scope.b));
	});

	this.registerNativeMethod('__eq', p_args(this.vm.types, 'a', null, 'b', null), function vm___eq(scope) {
		return NgsValue(this.vm.types.Bool, scope.a.eq(scope.b));
	});

	this.registerNativeMethod('__set_item', p_args(this.vm.types, 'a', this.vm.types.Array, 'idx', this.vm.types.Number, 'v', null), function vm___set_item_p_arr_num_any(scope) {
		var a = get_arr(scope.a);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(a.length < i) {
			for(var j=a.length;j<i;j++) {
				a[j] = NgsValue(this.vm.types.Null, null);
			}
		}
		a[i] = scope.v;
		return scope.v;
	});

	this.registerNativeMethod('keys', p_args(this.vm.types, 'h', this.vm.types.Hash), function vm_keys_p_hsh(scope) {
		var h = get_hsh(scope.h);
		return to_ngs_object(this.vm.types, _.keys(h));
	});

	this.registerNativeMethod('__set_item', p_args(this.vm.types, 'h', this.vm.types.Hash, 'k', this.vm.types.String, 'v', null), function vm___set_item_p_hsh_str_any(scope) {
		var h = get_hsh(scope.h);
		var k = get_str(scope.k)
		h[k] = scope.v;
		return scope.v;
	});

	this.registerNativeMethod('remove', p_args(this.vm.types, 'h', this.vm.types.Hash, 'k', this.vm.types.String), function vm___remove_p_hsh_str(scope) {
		var h = get_hsh(scope.h);
		var k = get_str(scope.k)
		delete h[k];
		return scope.h;
	});

	this.registerNativeMethod('__get_item', p_args(this.vm.types, 'a', this.vm.types.Array, 'idx', this.vm.types.Number), function vm___get_item_p_arr_num(scope) {
		var a = get_arr(scope.a);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(i<0 || i>a.length-1) {
			this.thr(to_ngs_object(this.vm.types, ['runtime', "Accessing out of bounds. Array: " + a + ". Index: " + i]));
		}
		return a[i];
	});

	this.registerNativeMethod('__get_item', p_args(this.vm.types, 's', this.vm.types.String, 'idx', this.vm.types.Number), function vm___get_item_p_str_num(scope) {
		var s = get_str(scope.s);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(i<0 || i>s.length-1) {
			this.thr(to_ngs_object(this.vm.types, ['runtime', "Accessing string out of bounds. String: " + s + ". Index: " + i]));
		}
		return NgsValue(this.vm.types.String, s[i]);
	});

	this.registerNativeMethod('len', p_args(this.vm.types, 's', this.vm.types.Seq), function vm_len_p_seq(scope) {
		return NgsValue(this.vm.types.Number, get_seq(scope.s).length);
	});

	this.registerNativeMethod('len', p_args(this.vm.types, 'h', this.vm.types.Hash), function vm_len_p_hsh(scope) {
		return NgsValue(this.vm.types.Number, _.size(get_hsh(scope.h)));
	});

	this.registerNativeMethod('echo', p_args(this.vm.types, 's', this.vm.types.String), function vm_echo(scope) {
		// console.log('ECHO', util.inspect(get_arr(scope.p), {depth: 20}), scope.n);
		console.log(get_str(scope.s));
		return scope.s;
	});

	this.registerNativeMethod('write', p_args(this.vm.types, 's', this.vm.types.String), function vm_write(scope) {
		process.stdout.write(get_str(scope.s));
		return scope.s;
	});

	this.registerNativeMethod('__get_lexical_scopes', p_args(this.vm.types), function vm___get_lexical_scopes() {
		// Need lexical scopes of caller, not ours.
		// Dirty scopes hack
		return NgsValue(this.vm.types.Scopes, this.getCallerLexicalScopes());
	});

	this.registerNativeMethod('__lambda', p_args(this.vm.types, 'scopes', this.vm.types.Scopes, 'args', this.vm.types.Array, 'ip', this.vm.types.Number, 'name', this.vm.types.String), function vm___lambda(scope) {
		return NgsValue(this.vm.types.Lambda, {scopes: scope.scopes, args: scope.args, code_ptr: scope.ip, name: scope.name});
	});

	this.registerNativeMethod('__register_method', p_args(this.vm.types, 'lambda', this.vm.types.Lambda, 'name', this.vm.types.String, 'global', this.vm.types.Bool), function vm___register_method(scope) {
		var name = get_str(scope.name);
		// The method is created in _caller_ lexical scops, not in ours.
		// Dirty lexical_scopes hack start
		var t = this.frame.scopes;
		this.frame.scopes = this.getCallerLexicalScopes();
		this.registerMethod(name, scope.lambda, get_boo(scope.global));
		this.frame.scopes = t;
		// Dirty lexical_scopes hack end
		return scope.lambda;
	});

	this.registerNativeMethod('__throw', p_args(this.vm.types, 'e', null), function vm___throw(scope, vm) {
		this.throw_(scope.e, vm);
		return NgsValue(this.vm.types.Null, null);
	});

	this.registerNativeMethod('native_spawn', Args(this.vm.types).rest_pos('args').get(), function ngs_runtime_spawn(scope, v) {
		var args = get_arr(scope.args);
		// TODO: make the next id a language global variable. It will help with serialization later.
		var props = {
			'id': 'process_' + (process_ngs_id++),
			'pid': null,
			'cmd': get_str(args[0]),
			'state': 'running',
			'args': args.slice(1).map(get_str),
			'error': null,
			'exit_code': null,
			'signal': null,
			'stdout': '',
			'stderr': '',
			'threads_waiting': [],
			'_finish_events': 2 // want both exit and close events
		};
		var ngs_runtime_spawn_finish_callback = function(force) {
			if(!props._finish_events) {
				// already finished
				return;
			}
			props._finish_events--;
			if(force) {
				props._finish_events = 0;
			}
			if(!props._finish_events) {
				props.state = 'done';
				props.threads_waiting.forEach(function(ctx) {
					v.unsuspend_context(ctx);
				});
			}
		}.bind(this);
		// console.log('start_external()', props);
		// TODO: process working directory should be
		//		 inherited from parent process.
		// TODO: XXX cwd must be flexible, not process.cwd
		var p = child_process.spawn(props.cmd, props.args, {
			cwd: process.cwd()
		});
		props.pid = p.pid;
		// TODO: maybe store output with it's timestamp?
		(['stdout', 'stderr']).forEach(function(output_channel_name) {
			p[output_channel_name].on('data', function ngs_runtime_spawn_on_data(data) {
				// console.log('DATA', data);
				props[output_channel_name] += data;
			});
		});
		p.on('error', function(e) {
			props.error = e;
			ngs_runtime_spawn_finish_callback(true);
		});
		p.on('exit', function(exit_code, signal) {
			props.exit_code = exit_code;
			props.signal = signal;
			ngs_runtime_spawn_finish_callback();
		});
		p.on('close', ngs_runtime_spawn_finish_callback);

		return NgsValue(this.vm.types.Process, props);
	});
	this.registerNativeMethod('wait', p_args(this.vm.types, 'p', this.vm.types.Process), function vm___get_attr(scope, v) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var p = get_prc(scope.p);
		if(p.state === 'done') {
			return scope.p;
		}
		v.suspend_context();
		p.threads_waiting.push(this);
		return scope.p;
	})
	this.registerNativeMethod('__get_attr', p_args(this.vm.types, 'p', this.vm.types.Process, 'attr', null), function vm___get_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var p = get_prc(scope.p);
		if(!_.has(p, a)) {
			throw new Error("Process object does not have attribute " + a);
		}
		return to_ngs_object(this.vm.types, p[a]);
	})
	this.registerNativeMethod('__get_item', p_args(this.vm.types, 'h', this.vm.types.Hash, 'attr', this.vm.types.String), function vm___get_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var h = get_hsh(scope.h);
		if(!_.has(h, a)) {
			throw new Error("Hash does not have attribute " + a);
		}
		return h[a];
	})
	this.registerNativeMethod('__set_attr', p_args(this.vm.types, 'h', this.vm.types.Hash, 'attr', this.vm.types.String, 'v', null), function vm___set_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var h = get_hsh(scope.h);
		h[a] = scope.v;
		return scope.h;
	})
	this.registerNativeMethod('from_json', p_args(this.vm.types, 's', this.vm.types.String), function vm_from_json(scope) {
		var json;
		var ret;
		json = get_str(scope.s);
		try {
			ret = JSON.parse(json);
		} catch(e) {
			this.thr(to_ngs_object(this.vm.types, ['runtime', 'failed to parse JSON']));
			return;
		}
		return to_ngs_object(this.vm.types, ret);
	})
	this.registerNativeMethod('fetch_file', p_args(this.vm.types, 'f', this.vm.types.String), function vm_read(scope) {
		// TODO: handle encoding later
		return NgsValue(this.vm.types.String, fs.readFileSync(get_str(scope.f), {encoding: 'UTF-8'}));
	})
	this.registerNativeMethod('__match', p_args(this.vm.types, 's', this.vm.types.String, 'regex', this.vm.types.Regexp), function vm___match(scope) {
		var r = get_rgx(scope.regex);
		return to_ngs_object(this.vm.types, r.exec(get_str(scope.s)));
	})
	this.registerNativeMethod('sort', p_args(this.vm.types, 'a', this.vm.types.Array), function vm_sort(scope) {
		var a = get_arr(scope.a);
		return NgsValue(this.vm.types.Array, _.sortBy(a, function(elt) { return elt.data }));
	});
	this.registerNativeMethod('uniq', p_args(this.vm.types, 'a', this.vm.types.Array), function vm_uniq(scope) {
		var a = get_arr(scope.a);
		return NgsValue(this.vm.types.Array, _.uniq(a, false, function(elt) {return elt.data}));
	});

	// stdin, stdout, ...
	['stdin', 'stdout', 'stderr'].forEach(function(s) {
		this.set_glo_var(s, NgsValue(this.vm.types.Stream, s));
	}.bind(this))
	this.registerNativeMethod('istty', p_args(this.vm.types, 's', this.vm.types.Stream), function vm_istty(scope) {
		var s = get_stm(scope.s);
		return NgsValue(this.vm.types.Bool, s.isTTY);
	});
	this.registerNativeMethod('Readline', p_args(this.vm.types), function vm_readline(scope) {
		var readline = require('readline');
		var rl = readline.createInterface(process.stdin, process.stdout);
		return NgsValue(this.vm.types.Readline, rl);
	});
	this.registerNativeMethod('read', p_args(this.vm.types, 'rl', this.vm.types.Readline, 'prompt', this.vm.types.String), function vm_read_p_readline(scope, v) {
		var rl = get_rl(scope.rl);
		var ctx = this;
		function line_handler(line) {
			ctx.stack.pop();
			ctx.stack.push(NgsValue(this.vm.types.String, line));
			v.unsuspend_context(ctx);
			rl.removeListener('line', line_handler);
		}
		rl.on('line', line_handler);
		v.suspend_context();
		rl.setPrompt(get_str(scope.prompt));
		rl.prompt();
		return NgsValue(this.vm.types.String, 'READLINE-TO-BE-READ');
	});
	this.registerNativeMethod('pause', p_args(this.vm.types, 'rl', this.vm.types.Readline), function vm_pause_p_readline(scope, v) {
		var rl = get_rl(scope.rl);
		v.suspend_context_till(rl, 'pause');
		rl.pause();
		return NgsValue(this.vm.types.Null, null);
	});
	this.registerNativeMethod('resume', p_args(this.vm.types, 'rl', this.vm.types.Readline), function vm_resume_p_readline(scope, v) {
		var rl = get_rl(scope.rl);
		v.suspend_context_till(rl, 'resume');
		rl.resume();
		return NgsValue(this.vm.types.Null, null);
	});
	this.registerNativeMethod('close', p_args(this.vm.types, 'rl', this.vm.types.Readline), function vm_close_p_readline(scope, v) {
		var rl = get_rl(scope.rl);
		v.suspend_context_till(rl, 'close');
		rl.close();
		return NgsValue(this.vm.types.Null, null);
	});
	this.registerNativeMethod('compile', p_args(this.vm.types, 's', this.vm.types.String, 'fname', this.vm.types.String), function vm_compile_p_str(scope) {
		var s = get_str(scope.s);
		var fname = get_str(scope.fname);
		var out;
		try {
			out = compile.compile(s, fname, {leave_value_in_stack: true});
			return NgsValue(this.vm.types.Code, out.compiled_code);
		} catch(e) {
			// console.log(e);
			// throw e;
			this.thr(to_ngs_object(this.vm.types, ['compile', e.toString()]));
		}
	});
	this.registerNativeMethod('load', p_args(this.vm.types, 'c', this.vm.types.Code), function vm_load_p_cod(scope, v) {
		// TODO: handle scopes correcty
		var c = get_cod(scope.c);
		var ptr = v.useCodeWithRet(c);
		var m = NgsValue(this.vm.types.Lambda, {
			scopes: NgsValue(this.vm.types.Scopes, this.frame.scopes.slice(0, this.frame.scopes.length-1)),
			args: p_args(this.vm.types),
			code_ptr: to_ngs_object(this.vm.types, ptr),
			name: to_ngs_object(this.vm.types, '-loaded-code-wrapper-'),
		});

		return m;
	});
	this.registerNativeMethod('String', p_args(this.vm.types, 'n', this.vm.types.Number), function vm_string_p_num(scope, v) {
		return to_ngs_object(this.vm.types, get_num(scope.n).toString());
	});
	this.registerNativeMethod('typeof', p_args(this.vm.types, 'x', null), function vm_typeof_p_any(scope, v) {
		return get_type(scope.x);
	});
	// Not very elegant solution :/
	// Injects additional scope and marks it as target for set_var instead of the deepest scope.
	// Allows saving variables between code calls in CLI:
	//   code = compile(l)
	//   ...
	//   lambda_ = load(code[1]).locals(local_scope)
	this.registerNativeMethod('locals', p_args(this.vm.types, 'l', this.vm.types.Lambda, 'h', this.vm.types.Hash), function vm_locals_p_lmb_hsh(scope, v) {
		var lambda = get_lmb(scope.l);
		var locals = get_hsh(scope.h);
		return NgsValue(this.vm.types.Lambda, {
			scopes: NgsValue(this.vm.types.Scopes, scope.l.data.scopes.data.concat(locals, true)),
			args: lambda.args,
			code_ptr: lambda.code_ptr,
			name: lambda.name
		});
	});
	this.registerNativeMethod('globals', p_args(this.vm.types), function vm_globals(scope, v) {
		var scopes = this.getCallerLexicalScopes();
		return NgsValue(this.vm.types.Hash, scopes[0]);
	});
	this.registerNativeMethod('meta', p_args(this.vm.types, 'x', null), function vm_meta(scope, v) {
		// to_ngs_object(this.vm.types, ) can not handle it properly
		return NgsValue(this.vm.types.Hash, data.get_meta(scope.x));
	});
	this.registerNativeMethod('thread', p_args(this.vm.types), function vm_thread(scope, v) {
		return NgsValue(this.vm.types.Thread, this, this.meta);
	});
	this.registerNativeMethod('thread', p_args(this.vm.types, 'f', this.vm.types.Lambda), function vm_thread_p_lmb(scope, v) {
		var ctx = v.makeContext();
		ctx.frame.ip = 'context_finished';
		ctx.invoke_or_throw(scope.f, to_ngs_object(this.vm.types, []), to_ngs_object(this.vm.types, {}), v, true);
		this.vm.suspended_contexts.push(ctx);
		return NgsValue(this.vm.types.Thread, ctx, ctx.meta);
	});
	this.registerNativeMethod('run', p_args(this.vm.types, 't', this.vm.types.Thread), function vm_run_p_thr(scope) {
		var t = get_thr(scope.t);
		if(t.state !== 'new') {
			this.thr(to_ngs_object(this.vm.types, ['programming', 'Trying to run() non-new thread ' + t.id]));
			return scope.t;
		}
		this.vm.unsuspend_context(t);
		return scope.t;
	});
	this.registerNativeMethod('String', p_args(this.vm.types, 't', this.vm.types.Thread), function vm_string_p_thread(scope) {
		var t = get_thr(scope.t);
		return NgsValue(this.vm.types.String, "<Thread " + t.id.toString() + ":" + t.state + ">");
	});
	this.registerNativeMethod('__get_attr', p_args(this.vm.types, 't', this.vm.types.Thread, 'k', this.vm.types.String), function vm___get_attr_p_thr(scope) {
		var attrs = ['id', 'state', 'cycles'];
		var t = get_thr(scope.t);
		var k = get_str(scope.k);
		if(!_.contains(attrs, k)) {
			this.thr(to_ngs_object(this.vm.types, ["programming", "Thread does not have attribute " + k]));
			return;
		}
		return to_ngs_object(this.vm.types, t[k]);
	});
	this.registerNativeMethod('wait', p_args(this.vm.types, 't', this.vm.types.Thread), function vm_wait_p_thread(scope, v) {
		this.wait(get_thr(scope.t), v);
		return scope.t;
	});
	this.registerNativeMethod('locals', p_args(this.vm.types, 't', this.vm.types.Thread), function vm_locals_p_thread(scope) {
		return NgsValue(this.vm.types.Hash, get_thr(scope.t).thread_locals);
	});
	this.registerNativeMethod('kill', p_args(this.vm.types, 't', this.vm.types.Thread), function vm_kill_p_thread(scope, v) {
		// Does not work yet: ngs_runtime_spawn_finish_callback performs additional unsuspend_context on finished_context
		// console.log(scope.t.data)
		// console.log('R', v.runnable_contexts.indexOf(scope.t.data));
		// console.log('S', v.suspended_contexts.indexOf(scope.t.data));
		// console.log('F', v.finished_contexts.indexOf(scope.t.data));
		v.unsuspend_context(scope.t.data, true);
		v.finish_context();
		return scope.t;
	});
	this.registerNativeMethod('id', p_args(this.vm.types, 'x', null), function vm_id_p_any(scope) {
		return to_ngs_object(this.vm.types, get_id(scope.x));
	});
	this.registerNativeMethod('obj', p_args(this.vm.types, 'type', this.vm.types.Type), function vm_obj_p_str(scope) {
		return NgsValue(scope.type, {});
	});
	this.registerNativeMethod('init', p_args(this.vm.types, 'n', this.vm.types.Number), function vm_init_p_num(scope) {
		scope.n.data = 0;
		return scope.n;
	});
	this.registerNativeMethod('init', p_args(this.vm.types, 'a', this.vm.types.Array), function vm_init_p_arr(scope) {
		scope.a.data = [];
		return scope.a;
	});
	this.registerNativeMethod('init', p_args(this.vm.types, 'h', this.vm.types.Hash), function vm_init_p_hsh(scope) {
		scope.h.data = {};
		return scope.h;
	});

	this.registerNativeMethod('Regexp', p_args(this.vm.types, 'pattern', this.vm.types.String, 'flags', this.vm.types.String), function vm_regexp_p_str_str(scope) {
		return NgsValue(this.vm.types.Regexp, new RegExp(get_str(scope.pattern), get_str(scope.flags)));
	});
	this.registerNativeMethod('slice', p_args(this.vm.types, 's', this.vm.types.String, 'start', this.vm.types.Number, 'count', this.vm.types.Number), function vm_slice_p_str_num_num(scope) {
		var start = get_num(scope.start);
		var count = get_num(scope.count);
		return NgsValue(this.vm.types.String, get_seq(scope.s).slice(start, start+count));
	});
	this.registerNativeMethod('slice', p_args(this.vm.types, 'a', this.vm.types.Array, 'start', this.vm.types.Number, 'count', this.vm.types.Number), function vm_slice_p_arr_num_num(scope) {
		var start = get_num(scope.start);
		var count = get_num(scope.count);
		return NgsValue(this.vm.types.Array, get_seq(scope.a).slice(start, start+count));
	});
	this.registerNativeMethod('ord', p_args(this.vm.types, 's', this.vm.types.String), function vm_ord_p_str(scope) {
		return NgsValue(this.vm.types.Number, get_str(scope.s).charCodeAt(0));
	});
	this.registerNativeMethod('Path', p_args(this.vm.types, 's', this.vm.types.String), function vm_path_p_str(scope) {
		return NgsValue(this.vm.types.Path, {name: scope.s});
	});
	this.registerNativeMethod('File', p_args(this.vm.types, 's', this.vm.types.String), function vm_file_p_str(scope) {
		return NgsValue(this.vm.types.File, {name: scope.s});
	});
	this.registerNativeMethod('__get_attr', p_args(this.vm.types, 'f', this.vm.types.File, 'attr', this.vm.types.String), function vm___get_attr_p_fil_str(scope) {
		var a = get_str(scope.attr);
		var f = get_fil(scope.f);
		if(!_.has(f, a)) {
			throw new Error("File object does not have attribute " + a);
		}
		return f[a];
	});

	// TODO: Make s Seq/buffer maybe.
	// TODO: Actually appends so 'write' may not be the best name. To consider.
	this.registerNativeMethod('write', p_args(this.vm.types, 'f', this.vm.types.File, 's', this.vm.types.String), function vm_write_p_fil_str(scope, v) {
		var filename = get_str(scope.f.data.name);
		var data = get_str(scope.s);
		var ctx = this;
		fs.writeFile(filename, data, {flag: 'a'}, function write_done(err) {
			// TODO: Error handling
			// console.log('ERR', err);
			v.unsuspend_context(ctx);
		});
		v.suspend_context();
		return scope.f;
	});
	this.registerNativeMethod('read', p_args(this.vm.types, 'fd', this.vm.types.Number), function vm_read_p_num(scope, v) {
		// TODO: Read specified number of bytes, not 1.
		var fd = get_num(scope.fd);
		var ctx = this;
		var buf = new Buffer(1);
		// console.log('read()', fd);
		fs.read(fd, buf, 0, 1, null, function read_done(err, bytesRead, buffer) {
			// TODO: Error handling
			// console.log('ERR', err, bytesRead, buffer);
			ctx.stack.pop();
			ctx.stack.push(NgsValue(this.vm.types.String, ''+buffer));
			v.unsuspend_context(ctx);
		});
		v.suspend_context();
		return null;
	});
	this.set_glo_var('ARGV', to_ngs_object(this.vm.types, process.argv));

	// TODO: Lock tests, maybe move locks methods to be Context methods
	this.registerNativeMethod('Lock', p_args(this.vm.types), function vm_lock(scope, v) {
		return NgsValue(this.vm.types.Lock, {
			acquired: to_ngs_object(this.vm.types, false),
			// holding_context: to_ngs_object(this.vm.types, null),
			waiting_contexts: to_ngs_object(this.vm.types, [])
		});
	});
	this.registerNativeMethod('acquire', p_args(this.vm.types, 'l', this.vm.types.Lock), function vm_acquire_p_lck(scope, v) {
		// TODO: maybe add "waiting_for_lock" state for Context
		var l = get_lck(scope.l);
		if(get_boo(l.acquired)) {
			v.suspend_context();
			get_arr(l.waiting_contexts).push(this);
		} else {
			l.acquired.data = true;
		}
		return to_ngs_object(this.vm.types, true);
	});
	this.registerNativeMethod('release', p_args(this.vm.types, 'l', this.vm.types.Lock), function vm_release_p_lck(scope, v) {
		var l = get_lck(scope.l);
		if(get_arr(l.waiting_contexts).length == 0) {
			l.acquired.data = false;
			return;
		}
		var ctx = get_arr(l.waiting_contexts).shift();
		v.unsuspend_context(ctx);
		return to_ngs_object(this.vm.types, null);
	});
	this.registerNativeMethod('as', p_args(this.vm.types, 'v', null, 't', this.vm.types.Type), function vm_as_p_nul_str(scope, v) {
		// TODO: check that t is super-type of type of v (in stdlib)
		// This is becoming dangerous I think.
		var t = scope.t;
		if(get_type(t) !== this.vm.types.Type) {
			this.thr(to_ngs_object(this.vm.types, ['programming', 'as(v, t) expects second argument to be a type']));
		}
		return NgsValue(scope.t, scope.v.data, scope.v.meta);
	});
	this.registerNativeMethod('Type', p_args(this.vm.types, 'name', this.vm.types.String), function vm_type_p_str(scope, v) {
		var name = get_str(scope.name);
		// console.log('TYPE', NgsType(name, this.vm.types.Type));
		return NgsType(name, this.vm.types.Type);
	});
	this.registerNativeMethod('inherits', p_args(this.vm.types, 'child', this.vm.types.Type, 'parent', this.vm.types.Type), function vm_inherits_p_typ_typ(scope, v) {
		var child = get_typ(scope.child);
		if(get_type(scope.parent) !== this.vm.types.Type) {
			this.thr(to_ngs_object(this.vm.types, ['programming', 'inherits(child, parent) expects second argument to be a type']));
			return NgsValue(this.vm.types.Null, null);
		}
		child.parents.push(scope.parent);
		return scope.child;
	});
}

exports.Args = Args.bind(null);
exports.register_native_methods = register_native_methods;
exports.to_ngs_object = to_ngs_object;
