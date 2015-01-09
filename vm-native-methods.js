"use strict";

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

  // stack: ... array value -> ... array
  this.registerNativeMethod('push', p_args('a', 'Array', 'x', null), function vm_push(scope) {
	var a = get_arr(scope.a);
	a.push(scope.x);
	return scope.a;
  });

  // stack: ... v1 v2 -> ... v
  this.registerNativeMethod('__add', p_args('a', 'Number', 'b', 'Number'), function vm___add(scope) {
	// TODO: when multi-method is implemented, move to another method
	return ['Number', get_num(scope.a) + get_num(scope.b)];
  });

  // stack: ... v1 v2 -> ... v
  this.registerNativeMethod('__add', p_args('a', 'Array', 'b', 'Array'), function vm___add(scope) {
	// TODO: when multi-method is implemented, move to another method
	return ['Array', get_arr(scope.a).concat(get_arr(scope.b))];
  });

  // stack: ... v1 v2 -> ... v
  this.registerNativeMethod('__sub', p_args('a', 'Number', 'b', 'Number'), function vm___sub(scope) {
	// TODO: when multi-method is implemented, move to another method
	return ['Number', get_num(scope.a) - get_num(scope.b)];
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
	console.log('__deftype', name, fields_defs);
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
	console.log(util.inspect(vm.types, {depth: 20}));
	return 'DUNNO-YET';
  });

  // stack: ... -> ... lexical_scopes
  this.registerNativeMethod('__get_lexical_scopes', p_args(), function vm___get_lexical_scopes() {
	// Need lexical scopes of caller, not ours.
	// Dirty scopes hack
	return ['Scopes', this.getCallerLexicalScopes()];
  });

  // stack: ... lexical_scopes code_ptr -> ... lambda-object
  //                                           (temporary object repr.)
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
}

exports.Args = Args.bind(null);
exports.register_native_methods = register_native_methods;
