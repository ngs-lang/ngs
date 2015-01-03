'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var util = require('util');

function make_getter(type) {
  var r = function(data) {
	if(Object.prototype.toString.call(data) !== '[object Array]') {
	  console.log('Got non-data', data);
	  throw new Error('Got non-data: ' + Object.toString(data));
	}
	if(data[0] !== type) {
	  console.log('Got data of unexpcted type. Expected', type, 'got', data[0]);
	  throw new Error('Got non-'+type+': ' + Object.toString(data));
	}
	return data[1];
  }
  return r;
}

var get_num = make_getter('Number');
var get_str = make_getter('String');
var get_arr = make_getter('Array');
var get_hsh = make_getter('Hash');
var get_scp = make_getter('Scopes');
var get_lmb = make_getter('Lambda');

function Context(global_scope) {
  return this.initialize(global_scope);
}

Context.prototype.initialize = function(global_scope) {
  this.ip = 0;
  this.stack = [];
  this.frames = [];
  this.lexical_scopes = [global_scope];

  var get_context_ip = function() {
	return this.ip;
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

  // stack: ... -> array
  this.registerMethod('Array', function vm_Array() {
	return ['Array', new Array()];
  });

  // stack: ... array value -> ... array
  this.registerMethod('push', function vm_push(p) {
	var a = get_arr(p[0]);
	// console.log('push', p);
	a.push(p[1]);
	return p[0];
  });

  // stack: ... array1 array2 -> ... arrayConcat
  this.registerMethod('concat', function vm_cocat(p) {
	var a = get_arr(p[0]);
	var b = get_arr(p[1]);
	return ['Array', a.concat(b)];
  });

  // stack: ... v1 v2 -> ... v
  this.registerMethod('__add', function vm___add(p) {
	// TODO: when multi-method is implemented, move to another method
	if(p[0][0] === 'Array' && p[1][0] === 'Array') {
      return ['Array', p[0][1].concat(p[1][1])];
    }
	return ['Number', get_num(p[0]) + get_num(p[1])];
  });

  // stack: ... v1 v2 -> ... v
  this.registerMethod('__sub', function vm___sub(p) {
	return ['Number', get_num(p[0]) - get_num(p[1])];
  });

  // stack: ... v -> ...
  this.registerMethod('echo', function vm_echo(p, n) {
	console.log('ECHO', p, n);
	return null;
  });

  // stack: ... type_name fields_defs -> ...
  this.registerMethod('__deftype', function vm___deftype(p, n, vm) {
	// TODO: maybe allow redefining type (for extending)
	var name = get_str(p[0]);
	var fields_defs = get_arr(p[1]);
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

  this.registerMethod('__enter_lexical_scope', function vm___enter_lexical_scope() {
	var scope = {};
	this.lexical_scopes.push(scope);
	return scope;
  });

  this.registerMethod('__leave_lexical_scope', function vm___leave_lexical_scope() {
	return this.lexical_scopes.pop();
  });

  // stack: ... -> ... lexical_scopes
  this.registerMethod('__get_lexical_scopes', function vm___get_lexical_scopes() {
	return ['Scopes', this.lexical_scopes];
  });

  // stack: ... lexical_scopes code_ptr -> ... lambda-object
  //                                           (temporary object repr.)
  this.registerMethod('__lambda', function vm___lambda(p) {
	return ['Lambda', p];
  });

  // stack: ... lambda-object name -> ... lambda-object
  this.registerMethod('__register_method', function vm___register_method(p) {
	var name = get_str(p[1]);
	this.registerMethod(name, p[0]);
	return p[0];
  });
  return this;
}

Context.prototype.find_var_lexical_scope = function(varname) {
  var scopes = this.lexical_scopes;
  for(var i=scopes.length-1; i>=0; i--) {
	if(Object.prototype.hasOwnProperty.call(scopes[i], varname)) {
	  return [true, scopes[i]];
	}
  }
  return [false, scopes[0]];
}



function VM() {
  return this.initialize();
}

VM.prototype.initialize = function() {
  this.code = [];
  this.global_scope = {};
  this.types = {};
  this.runnable_contexts = [];
  this.suspended_contexts = [];
  this.finished_contexts = [];
  this.context = null;

  return this;
}

VM.prototype.useCode = function(c) {
  // TODO: assert that there are no cotext pointing to existing this.code
  //       (which is being replaced)
  this.code = c;
  this.code.push(['halt']);
  return this;
}

VM.prototype.setupContext = function() {
  var c = new Context(this.global_scope);
  this.runnable_contexts.push(c);
  return c;
}

VM.prototype.start = function(finished_callback) {
  this.finished_callback = finished_callback;
  this.mainLoop();
}

VM.prototype.mainLoop = function() {
  while(this.runnable_contexts.length) {
	this.context = this.runnable_contexts[0];
	var op = this.code[this.context.ip];
	this.context.ip++;
	console.log('ST', util.inspect(this.context.stack, {depth: 20}));
	console.log('OP', op);
	console.log('');
	if(op[0] === 'comment') {
	  continue;
	}
	if(!(op[0] in this.opcodes)) {
	  throw new Error("Illegal opcode: " + op[0] + " at " + (this.context.ip-1));
	}
	this.opcodes[op[0]].call(this, op[1]);
  }
  if(!this.runnable_contexts.length && !this.suspended_contexts.length) {
	this.finished_callback(this);
  }
}

VM.prototype.suspend_context = function() {
  var ctx = this.runnable_contexts.shift();
  this.suspended_contexts.push(ctx);
  // console.log('suspend_context', ctx);
}

VM.prototype.unsuspend_context = function(ctx) {
  var i = this.suspended_contexts.indexOf(ctx);
  if(i === -1) {
	if(this.runnable_contexts.indexOf(ctx) !== -1) {
	  console.warn("VM.unsuspend_context() on context which is already runnable: " + ctx);
	} else {
	  throw new Error("VM.unsuspend_context() on context which is not suspended: " + ctx);
	}
  }
  this.suspended_contexts.splice(i, 1);
  this.runnable_contexts.push(ctx);
  setTimeout(function() {
	this.mainLoop();
  }.bind(this), 0);
}

Context.prototype.registerMethod = function(name, f) {
  // TODO: types
  var r = this.find_var_lexical_scope(name);
  if(!r[0]) {
	r[1][name] = ['Array', [f]];
	return;
  }
  r[1][name][1].push(f);
}

function match_params(lambda, positional_args, named_args) {
  var l = get_lmb(lambda); // ['Lambda', ['Array', [SCOPES, ARGS, IP]]]
  var l = get_arr(l);
  var params = get_arr(l[1]);
  var scope = {};
  var positional_idx = 0;
  // console.log('match_params', positional_args, named_args, util.inspect(params, {depth: 20}));

  var p = get_arr(positional_args);
  var n = get_hsh(named_args);
  for(var i=0; i<params.length; i++) {
	var cur_param = get_arr(params[i]);
	var cur_param_name = get_str(cur_param[0]);
	var cur_param_type = get_str(cur_param[1]);
	// console.log('params', i, cur_param_name, cur_param_type);
	if(cur_param_type == 'arg_pos') {
	  if(p.length-1 < positional_idx) {
		return [false, {}, 'not enough pos args'];
	  }
	  scope[cur_param_name] = p[positional_idx++];
	}
  }
  if(p.length > positional_idx) {
	return [false, {}, 'too much pos args'];
  }
  return [true, scope, 'all matched'];
}


Context.prototype.invoke = function(methods, positional_args, named_args, vm) {
  // TODO:
  //   * Find appropriate method by parameters matching - walk the array

  var ms = get_arr(methods);

  for(var l=ms.length-1, i=l; i>=0; i--) {
	var m = ms[i];

	// console.log('m', m);

	// 1. Native
	if(typeof m == 'function') {
	  // Maybe later: parameters matching
	  this.stack.push(m.call(this, positional_args, named_args, vm));
	  return;
	}

	// 2. User defined
	if(m[0] === 'Lambda') {
	  // ['Lambda', ['Array', [SCOPES, ARGS, IP]]]
	  var scope = match_params(m, positional_args, named_args);
	  if(!scope[0]) {
		continue;
	  }
	  var lambda = get_arr(get_lmb(m));
	  // 0:scopes, 1:args, 2:ip
	  this.frames.push({
		lexical_scopes: this.lexical_scopes,
		ip: this.ip,
		stack_len: this.stack.length,
	  });
	  this.lexical_scopes = get_scp(lambda[0])
	  this.lexical_scopes = this.lexical_scopes.concat(scope[1]);
	  // console.log('lexical_scopes', this.lexical_scopes);
	  this.ip = get_num(lambda[2]);
	  return;
	}
  }

  throw new Error("Invoke: appropriate method not found");
}

VM.prototype.opcodes = {

  'halt': function() {
	this.finished_contexts.push(this.runnable_contexts.shift());
  },

  // stack: ... -> ... value
  'push': function(v) {
	// ideally v is a scalar
	this.context.stack.push(v);
  },

  // stack: ... -> ... ip of the next instruction
  'push_ip': function(v) {
	this.context.stack.push(['Number', this.context.ip]);
  },

  'push_num': function(v) {	this.context.stack.push(['Number', v]);  },
  'push_str': function(v) {	this.context.stack.push(['String', v]);  },
  'push_arr': function(v) {	this.context.stack.push(['Array', []]);  },
  'push_hsh': function(v) {	this.context.stack.push(['Hash', {}]);  },

  // stack: ... value -> ...
  'pop': function() {
	this.context.stack.pop();
  },

  // stack: ... -> ... marker
  'args_start': function() {
	this.context.stack.push(['ArgsMarker', null]);
  },

  // stack: ... arg1 arg2 ... argN -> ... Array(arg1, arg2, ..., argN)

  'args_end': function() {
	var st = this.context.stack();
	var ret = new Array();
	var item = st.pop();
	while(item[0] !== 'ArgsMarker') {
	  ret.push(item);
	}
	st.push(['Array', ret.reverse()]);
  },

  // stack: ... x -> ... x x
  'dup': function() {
	var st = this.context.stack;
	var v = st.pop();
	st.push(v);
	st.push(v);
  },

  // stack: ... var_name -> ... var_value
  'get_var': function() {
	var st = this.context.stack;
	var name = get_str(st.pop());
	var r = this.context.find_var_lexical_scope(name);
	if(!r[0]) {
	  throw new Error("Using undefined variable '" + name + "'");
	}
	this.context.stack.push(r[1][name]);
  },

  // stack: ... value varname -> ...
  'set_var': function() {
	var st = this.context.stack;
	var name = st.pop();
	var val = st.pop();
	name = get_str(name);
	this.context.find_var_lexical_scope(name)[1][name] = val;
  },

  // stack: ... positional_args named_args methods -> ... X
  'invoke': function() {
	var st = this.context.stack;
	var methods = st.pop();
	var named_args = st.pop();
	var positional_args = st.pop();
	this.context.invoke(methods, positional_args, named_args, this);
  },

  // stack: ... arg1 arg2 methods -> ... X
  'invoke2': function() {
	var st = this.context.stack;
	var methods = st.pop();
	var arg2 = st.pop();
	var arg1 = st.pop();
	var positional_args = [arg1, arg2];
	this.context.invoke(methods, positional_args, {}, this);
  },

  // stack: ... v -> ... v
  'ret': function() {
	var c = this.context;
	var frame = c.frames.pop();
	c.ip = frame.ip;
	c.lexical_scopes = frame.lexical_scopes;
	if(c.stack.length != frame.stack_len + 1) {
	  throw new Error("Returning with wrong stack size");
	}
  },

  'jump': function(offset) {
	this.context.ip += offset;
  },

  'jump_if_true': function(offset) {
	var v = this.context.stack.pop();
	if(v) {
	  this.context.ip += offset;
	}
  },

  'jump_if_false': function(offset) {
	var v = this.context.stack.pop();
	if(!v) {
	  this.context.ip += offset;
	}
  },
};


exports.VM = VM;
