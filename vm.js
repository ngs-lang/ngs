'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var debug_match_params = process.env.NGS_DEBUG_PARAMS;

var util = require('util');
var native_methods = require('./vm-native-methods');
var data = require('./vm-data');

var Args = native_methods.Args;

for(var k in data) {
  global[k] = data[k];
}


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

  native_methods.register_native_methods.call(this);

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

Context.prototype.getCallerLexicalScopes = function() {
  // Should only be exposed to internal functions for security reasons.
  return this.frames[this.frames.length-1].lexical_scopes;
}

Context.prototype.registerNativeMethod


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
  var stack_debug = process.env.NGS_DEBUG_STACK;
  while(this.runnable_contexts.length) {
	this.context = this.runnable_contexts[0];
	var op = this.code[this.context.ip];
	this.context.ip++;
	if(stack_debug) {
	  console.log('ST', util.inspect(this.context.stack, {depth: 20}));
	  console.log('OP', op);
	  console.log('');
	}
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

Context.prototype.registerNativeMethod = function(name, args, f) {
  var m =
	  [
		'Lambda',
		[
		  'Array',
		  [
			['Scopes', []], // Maybe change later and give access to the global scope. Simplicity for now.
			args,
			['NativeMethod', f],
		  ]
		]
	  ];
  this.registerMethod(name, m);
}

function match_params(lambda, positional_args, named_args) {
  var l = get_lmb(lambda); // ['Lambda', ['Array', [SCOPES, ARGS, IP]]]
  var l = get_arr(l);
  if(l[1] instanceof Args) {
	throw new Error("You forgot to use .get() in the end of Args().x().y().z() sequence when defining: " + l)
  }
  var params = get_arr(l[1]);
  var scope = {};
  var positional_idx = 0;
  if(debug_match_params) {
	console.log('match_params positional_args', positional_args);
	console.log('match_params named_args', named_args);
	console.log('match_params params', util.inspect(params, {depth: 20}));
  }

  var p = get_arr(positional_args);
  var n = get_hsh(named_args);
  for(var i=0; i<params.length; i++) {
	var cur_param = get_arr(params[i]);
	var cur_param_name = get_str(cur_param[0]);
	var cur_param_mode = get_str(cur_param[1]);
	var cur_param_type;

	if(cur_param_mode === 'arg_rest_pos') {
	  scope[cur_param_name] = ['Array', p.slice(i)];
	  positional_idx += (p.length - i);
	  break;
	}
	if(get_type(cur_param[2]) !== 'Null') {
	  cur_param_type = get_str(cur_param[2]);
	} else {
	  cur_param_type = null;
	}
	// console.log('params', i, cur_param_name, cur_param_mode);
	if(cur_param_mode == 'arg_pos') {
	  if(p.length-1 < positional_idx) {
		return [false, {}, 'not enough pos args'];
	  }
	  if(cur_param_type) {
		if(get_type(p[positional_idx]) !== cur_param_type) {
		  return [false, {}, 'pos args type mismatch at ' + positional_idx];
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


Context.prototype.invoke = function(methods, positional_args, named_args, vm) {
  var ms = get_arr(methods);

  for(var l=ms.length-1, i=l; i>=0; i--) {
	var m = ms[i];

	var lambda = get_arr(get_lmb(m));
	// 0:scopes, 1:args, 2:ip/native_func
	var scope = match_params(m, positional_args, named_args);
	if(!scope[0]) {
	  continue;
	}
	var call_type = get_type(lambda[2]);
	this.frames.push({
	  lexical_scopes: this.lexical_scopes,
	  ip: this.ip,
	  stack_len: this.stack.length,
	});
	this.lexical_scopes = get_scp(lambda[0])
	this.lexical_scopes = this.lexical_scopes.concat(scope[1]);
	// console.log('lexical_scopes', this.lexical_scopes);
	if(call_type === 'Number') {
	  this.ip = get_num(lambda[2]);
	  return;
	}
	if(call_type === 'NativeMethod') {
	  var nm = get_nm(lambda[2]);
	  this.stack.push(nm.call(this, scope[1], vm));
	  var frame = this.frames.pop();
	  this.lexical_scopes = frame.lexical_scopes;
	  if(this.stack.length != frame.stack_len + 1) {
		throw new Error("Returning with wrong stack size");
	  }
	  return;
	}
	throw new Error("Don't know how to call matched method: " + m);
  }

  console.log(positional_args);
  throw new Error("Invoke: appropriate method not found for in " + util.inspect(ms, {depth: 20}));
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
  'push_nul': function(v) {	this.context.stack.push(['Null', null]);  },

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
	  // console.log('XXX', this.context.lexical_scopes, this.context.frames[0].lexical_scopes);
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
	var positional_args = ['Array', [arg1, arg2]];
	this.context.invoke(methods, positional_args, ['Hash', {}], this);
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
