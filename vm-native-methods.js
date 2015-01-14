"use strict";

var child_process = require('child_process');
var util = require('util');

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
	
	// stack: ... -> array
	this.registerNativeMethod('Array', p_args(), function vm_Array() {
		return ['Array', new Array()];
	});

	// stack: ... v -> Boolean
	this.registerNativeMethod('Bool', p_args('x', null), function vm_Bool_p_any(scope) {
		// Anything that was not processed by any other Bool() method
		// will have it's JS boolean value as result.
		return ['Bool', !!scope.x[1]]; // XXX broken get_TYP(v) data access abstraction
	});

	// stack: ... v -> Boolean
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
	// stack: ... v -> Boolean
	this.registerNativeMethod('Bool', p_args('a', 'Array'), function vm_Bool_p_arr(scope) {
		return ['Bool', get_arr(scope.a).length != 0];
	});

	// stack: ... array value -> ... array
	this.registerNativeMethod('push', p_args('a', 'Array', 'x', null), function vm_push(scope) {
		var a = get_arr(scope.a);
		a.push(scope.x);
		return scope.a;
	});

	// stack: ... v1 v2 -> ... v
	this.registerNativeMethod('__add', p_args('a', 'Number', 'b', 'Number'), function vm___add_p_num_num(scope) {
		return ['Number', get_num(scope.a) + get_num(scope.b)];
	});

	// stack: ... v1 v2 -> ... v
	this.registerNativeMethod('__add', p_args('a', 'String', 'b', 'String'), function vm___add_p_str_str(scope) {
		return ['String', get_str(scope.a) + get_str(scope.b)];
	});

	// stack: ... v1 v2 -> ... v
	this.registerNativeMethod('__add', p_args('a', 'Array', 'b', 'Array'), function vm___add_p_arr_arr(scope) {
		return ['Array', get_arr(scope.a).concat(get_arr(scope.b))];
	});

	// stack: ... v1 v2 -> ... v
	this.registerNativeMethod('__sub', p_args('a', 'Number', 'b', 'Number'), function vm___sub(scope) {
		return ['Number', get_num(scope.a) - get_num(scope.b)];
	});

	// stack: ... v1 v2 -> ... v
	this.registerNativeMethod('__mul', p_args('a', 'Number', 'b', 'Number'), function vm___sub(scope) {
		return ['Number', get_num(scope.a) * get_num(scope.b)];
	});

	// stack: ... v1 v2 -> ... bool
	this.registerNativeMethod('__lt', p_args('a', 'Number', 'b', 'Number'), function vm___lt(scope) {
		return ['Bool', get_num(scope.a) < get_num(scope.b)];
	});

	// stack: ... v1 v2 -> ... bool
	this.registerNativeMethod('__eq', p_args('a', null, 'b', null), function vm___eq(scope) {
		// XXX: incorrect implementation, uses JS comparison
		// XXX: does not use get_TYP data access abstraction
		return ['Bool', scope.a[1] === scope.b[1]];
	});

	// stack: ... v -> ...
	this.registerNativeMethod('echo', Args().rest_pos('p').get(), function vm_echo(scope) {
		console.log('ECHO', util.inspect(get_arr(scope.p), {depth: 20}), scope.n);
		return null;
	});

	// stack: ... type_name fields_defs -> ...
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

	// stack: ... -> ... lexical_scopes
	this.registerNativeMethod('__get_lexical_scopes', p_args(), function vm___get_lexical_scopes() {
		// Need lexical scopes of caller, not ours.
		// Dirty scopes hack
		return ['Scopes', this.getCallerLexicalScopes()];
	});

	// stack: ... lexical_scopes code_ptr -> ... lambda-object
	//											 (temporary object repr.)
	this.registerNativeMethod('__lambda', p_args('scopes', 'Scopes', 'args', 'Array', 'ip', 'Number'), function vm___lambda(scope) {
		return ['Lambda', ['Array', [scope.scopes, scope.args, scope.ip]]];
	});

	// stack: ... lambda-object name -> ... lambda-object
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
	this.registerNativeMethod('exec', Args().rest_pos('args').get(), function ngs_runtime_spawn(scope, v) {
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
}

exports.Args = Args.bind(null);
exports.register_native_methods = register_native_methods;
