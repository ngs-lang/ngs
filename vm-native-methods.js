"use strict";

var fs = require('fs');
var child_process = require('child_process');
var util = require('util');

var _ = require('underscore');

var data = require('./vm-data');

function to_ngs_object(v) {
	if(_.isNull(v)) {
		return ['Null', null];
	}
	if(_.isBoolean(v)) {
		return ['Bool', v];
	}
	if(_.isNumber(v)) {
		return ['Number', v];
	}
	if(_.isString(v)) {
		return ['String', v];
	}
	if(_.isArray(v)) {
		return ['Array', v.map(to_ngs_object)];
	}
	if(_.isObject(v)) {
		var h = {}
		_.keys(v).forEach(function(k) {
			h[k] = to_ngs_object(v[k]);
		});
		return ['Hash', h];
	}
	throw new Error("to_ngs_object() failed for " + v);
}

function Args() {
	if(!this || this == global) {
		return new Args();
	}
	this.args = [];
};


Args.prototype.general = function(name, typ, arg_type) {
	this.args.push(
		[
			'Array',
			[
				['String', name],
				['String', arg_type],
				[(typ ? 'String' : 'Null'), typ]
			]
		]
	);
	return this;
}
Args.prototype.pos = function(name, typ) {
	return this.general(name, typ, 'arg_pos')
}
Args.prototype.rest_pos = function(name) {
	return this.general(name, 'Array' /* not used */, 'arg_rest_pos')
}
Args.prototype.named = function(name, typ) {
	return this.general(name, typ, 'arg_nam')
}
Args.prototype.get = function() {
	return ['Array', this.args];
}

function p_args() {
	var ret = Args();
	for(var i=0; i<arguments.length; i+= 2) {
		ret.pos(arguments[i], arguments[i+1]);
	}
	return ret.get();
}

function register_native_methods() {
	// Expecting:
	// this - Context object

	this.registerNativeMethod('Array', p_args(), function vm_Array() {
		return ['Array', new Array()];
	});

	this.registerNativeMethod('Hash', p_args(), function vm_Array() {
		return ['Hash', new Object()];
	});

	this.registerNativeMethod('Bool', p_args('x', null), function vm_Bool_p_any(scope) {
		// Anything that was not processed by any other Bool() method
		// will have it's JS boolean value as result.
		return ['Bool', !!scope.x[1]]; // XXX broken get_TYP(v) data access abstraction
	});

	this.registerNativeMethod('Bool', p_args('p', 'Process'), function vm_Bool_p_process(scope) {
		var p = get_prc(scope.p);
		if(p.state !== 'done') {
			// TODO: throw GuardYield or something like that,
			//		 indicating guard failure
			throw new Error("Can't Bool() unfinished process");
		}
		// exit_code can be null if process was terminated by a signal
		return ['Bool', p.exit_code === 0];
	});

	// TODO: move to NSL - Ngs Standard Library
	this.registerNativeMethod('Bool', p_args('a', 'Array'), function vm_Bool_p_arr(scope) {
		return ['Bool', get_arr(scope.a).length != 0];
	});

	this.registerNativeMethod('push', p_args('a', 'Array', 'x', null), function vm_push(scope) {
		var a = get_arr(scope.a);
		a.push(scope.x);
		return scope.a;
	});

	this.registerNativeMethod('__add', p_args('a', 'Number', 'b', 'Number'), function vm___add_p_num_num(scope) {
		return ['Number', get_num(scope.a) + get_num(scope.b)];
	});

	this.registerNativeMethod('__add', p_args('a', 'String', 'b', 'String'), function vm___add_p_str_str(scope) {
		return ['String', get_str(scope.a) + get_str(scope.b)];
	});

	this.registerNativeMethod('__add', p_args('a', 'Array', 'b', 'Array'), function vm___add_p_arr_arr(scope) {
		return ['Array', get_arr(scope.a).concat(get_arr(scope.b))];
	});

	this.registerNativeMethod('__sub', p_args('a', 'Number', 'b', 'Number'), function vm___sub(scope) {
		return ['Number', get_num(scope.a) - get_num(scope.b)];
	});

	this.registerNativeMethod('__mul', p_args('a', 'Number', 'b', 'Number'), function vm___sub(scope) {
		return ['Number', get_num(scope.a) * get_num(scope.b)];
	});

	this.registerNativeMethod('__lt', p_args('a', 'Number', 'b', 'Number'), function vm___lt(scope) {
		return ['Bool', get_num(scope.a) < get_num(scope.b)];
	});
	this.registerNativeMethod('__gt', p_args('a', 'Number', 'b', 'Number'), function vm___lt(scope) {
		return ['Bool', get_num(scope.a) > get_num(scope.b)];
	});

	this.registerNativeMethod('__eq', p_args('a', null, 'b', null), function vm___eq(scope) {
		// XXX: incorrect implementation, uses JS comparison
		// XXX: does not use get_TYP data access abstraction
		return ['Bool', scope.a[1] === scope.b[1]];
	});

	this.registerNativeMethod('__set_item', p_args('a', 'Array', 'idx', 'Number', 'v', null), function vm___set_item_p_arr_num_any(scope) {
		var a = get_arr(scope.a);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(a.length < i) {
			for(var j=a.length;j<i;j++) {
				a[j] = ['Null', null];
			}
		}
		a[i] = scope.v;
		return scope.v;
	});

	this.registerNativeMethod('keys', p_args('h', 'Hash'), function vm_keys_p_hsh(scope) {
		var h = get_hsh(scope.h);
		return to_ngs_object(_.keys(h));
	});

	this.registerNativeMethod('__set_item', p_args('h', 'Hash', 'k', 'String', 'v', null), function vm___set_item_p_hsh_str_any(scope) {
		var h = get_hsh(scope.h);
		var k = get_str(scope.k)
		h[k] = scope.v;
		return scope.v;
	});

	this.registerNativeMethod('__get_item', p_args('a', 'Array', 'idx', 'Number'), function vm___get_item_p_arr_num(scope) {
		var a = get_arr(scope.a);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(i<0 || i>a.length-1) {
			throw new Error("Accessing out of bounds. Array: " + a + ". Index: " + i);
		}
		return a[i];
	});

	this.registerNativeMethod('__get_item', p_args('s', 'String', 'idx', 'Number'), function vm___get_item_p_str_num(scope) {
		var s = get_str(scope.s);
		var i = get_num(scope.idx)
		// TODO: assert i is integer
		if(i<0 || i>s.length-1) {
			throw new Error("Accessing out of bounds. Array: " + a + ". Index: " + i);
		}
		return ['String', s[i]];
	});

	this.registerNativeMethod('len', p_args('a', 'Array'), function vm_len_p_arr(scope) {
		return ['Number', get_arr(scope.a).length];
	});

	this.registerNativeMethod('len', p_args('s', 'String'), function vm_len_p_str(scope) {
		return ['Number', get_str(scope.s).length];
	});

	this.registerNativeMethod('len', p_args('h', 'Hash'), function vm_len_p_hsh(scope) {
		return ['Number', _.size(get_hsh(scope.h))];
	});

	this.registerNativeMethod('echo', Args().rest_pos('p').get(), function vm_echo(scope) {
		console.log('ECHO', util.inspect(get_arr(scope.p), {depth: 20}), scope.n);
		return ['Null', null];
	});

	this.registerNativeMethod('__deftype', p_args('name', 'String', 'fields', null), function vm___deftype(scope, vm) {
		// TODO: maybe allow redefining type (for extending)
		var name = get_str(scope.name);
		var fields_defs = get_arr(scope.fields);
		// console.log('__deftype', name, fields_defs);
		var order = [];
		var fields = {};
		for(var i=0; i<fields_defs.length; i++) {
			fields[fields_defs[i][0]] = fields_defs[i][1];
			order.push(fields_defs[i][0]);
		}
		vm.types[name] = {
			name: name,
			fields: fields,
			order: order,
		}
		// console.log(util.inspect(vm.types, {depth: 20}));
		return 'DUNNO-YET';
	});

	this.registerNativeMethod('__get_lexical_scopes', p_args(), function vm___get_lexical_scopes() {
		// Need lexical scopes of caller, not ours.
		// Dirty scopes hack
		return ['Scopes', this.getCallerLexicalScopes()];
	});

	this.registerNativeMethod('__lambda', p_args('scopes', 'Scopes', 'args', 'Array', 'ip', 'Number'), function vm___lambda(scope) {
		// console.log('__lambda', scope.scopes.length, scope.ip);
		return ['Lambda', ['Array', [scope.scopes, scope.args, scope.ip]]];
	});

	this.registerNativeMethod('__register_method', p_args('lambda', 'Lambda', 'name', 'String'), function vm___register_method(scope) {
		var name = get_str(scope.name);
		// The method is created in _caller_ lexical scops, not in ours.
		// Dirty lexical_scopes hack start
		var t = this.lexical_scopes;
		this.lexical_scopes = this.getCallerLexicalScopes();
		this.registerMethod(name, scope.lambda);
		this.lexical_scopes = t;
		// Dirty lexical_scopes hack end
		return scope.lambda;
	});

	this.registerNativeMethod('__throw', p_args('e', null), function vm___throw(scope, vm) {
		this.throw_(scope.e, vm);
		return ['Null', null];
	});

	this.registerNativeMethod('native_spawn', Args().rest_pos('args').get(), function ngs_runtime_spawn(scope, v) {
		var args = get_arr(scope.args);
		var props = {
			'cmd': get_str(args[0]),
			'state': 'running',
			'args': args.slice(1).map(get_str),
			'error': null,
			'exit_code': null,
			'signal': null,
			'stdout': '',
			'stderr': '',
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
				v.unsuspend_context(this);
			}
		}.bind(this);
		// console.log('start_external()', props);
		// TODO: process working directory should be
		//		 inherited from parent process.
		// TODO: XXX cwd must be flexible, not process.cwd
		var p = child_process.spawn(props.cmd, props.args, {
			cwd: process.cwd()
		});
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

		v.suspend_context();
		return ['Process', props];
	});
	this.registerNativeMethod('__get_attr', p_args('p', 'Process', 'attr', null), function vm___get_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var p = get_prc(scope.p);
		if(!_.has(p, a)) {
			throw new Error("Process object does not have attribute " + a);
		}
		return to_ngs_object(p[a]);
	})
	this.registerNativeMethod('__get_item', p_args('h', 'Hash', 'attr', 'String'), function vm___get_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var h = get_hsh(scope.h);
		if(!_.has(h, a)) {
			throw new Error("Hash does not have attribute " + a);
		}
		return h[a];
	})
	this.registerNativeMethod('__set_attr', p_args('h', 'Hash', 'attr', 'String', 'v', null), function vm___get_attr(scope) {
		// TODO (maybe): store all fields as NGS objects in the first place
		var a = get_str(scope.attr);
		var h = get_hsh(scope.h);
		h[a] = scope.v;
		return scope.h;
	})
	this.registerNativeMethod('from_json', p_args('s', 'String'), function vm_from_json(scope) {
		var json;
		var ret;
		json = get_str(scope.s);
		try {
			ret = [true, JSON.parse(json)];
		} catch(e) {
			ret = [false, null];
		}
		return to_ngs_object(ret);
	})
	this.registerNativeMethod('fetch_file', p_args('f', 'String'), function vm_read(scope) {
		// TODO: handle encoding later
		return ['String', fs.readFileSync(get_str(scope.f), {encoding: 'UTF-8'})];
	})
	this.registerNativeMethod('__tilde', p_args('s', 'String', 'regex', 'String'), function vm___tilde(scope) {
		var r = new RegExp(get_str(scope.regex));
		return to_ngs_object(get_str(scope.s).match(r));
	})
	this.registerNativeMethod('crash', p_args('s', 'String'), function vm_crash(scope) {
		throw new Error("CRASH: " + get_str(scope.s));
	});
	this.registerNativeMethod('sort', p_args('a', 'Array'), function vm_sort(scope) {
		var a = get_arr(scope.a);
		a.sort();
		return ['Array', a];
	});
	this.registerNativeMethod('uniq', p_args('a', 'Array'), function vm_sort(scope) {
		var a = get_arr(scope.a);
		return ['Array', _.uniq(a, false, function(elt) {return elt[1]})];
	});
}

exports.Args = Args.bind(null);
exports.register_native_methods = register_native_methods;
exports.to_ngs_object = to_ngs_object;
