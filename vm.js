'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var util = require('util');

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
	'value': function() {
	  if(this.length == 0) {
		throw new Error("Stack underflow at " + (get_context_ip()-1));
	  }
	  return Array.prototype.pop.call(this);
	}
  });

  // stack: ... -> array
  this.registerMethod('Array', function() {
	return new Array();
  });

  // stack: ... array value -> ... array
  this.registerMethod('push', function(p, n) {
	p[0].push(p[1]);
	return p[0];
  });

  // stack: ... array1 array2 -> ... arrayConcat
  this.registerMethod('concat', function(p) {
	return p[0].concat(p[1]);
  });

  // stack: ... v1 v2 -> ... v
  this.registerMethod('__add', function(p) {
	// TODO: when multi-method is implemented, move to another method
	if(util.isArray(p[0]) && util.isArray(p[1])) {
      return p[0].concat(p[1]);
    }
	return p[0] + p[1];
  });

  // stack: ... v1 v2 -> ... v
  this.registerMethod('__sub', function(p) {
	return p[0] - p[1];
  });

  // stack: ... v -> ...
  this.registerMethod('echo', function(v) {
	console.log('ECHO', v);
  });

  // stack: ... type_name fields_defs -> ...
  this.registerMethod('__deftype', function(p, n, vm) {
	// TODO: maybe allow redefining type (for extending)
	var name = p[0];
	var fields_defs = p[1];
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
  });

  this.registerMethod('__enter_lexical_scope', function() {
	var scope = {};
	this.lexical_scopes.push(scope);
	return scope;
  });

  this.registerMethod('__leave_lexical_scope', function() {
	return this.lexical_scopes.pop();
  });

  // stack: ... -> ... lexical_scopes
  this.registerMethod('__get_lexical_scopes', function() {
	return this.lexical_scopes;
  });

  // stack: ... lexical_scopes code_ptr -> ... lambda-object
  //                                           (temporary object repr.)
  this.registerMethod('__lambda', function(lexical_scopes, code_ptr) {
	return ['lambda', lexical_scopes, code_ptr];
  });

  // stack: ... lambda-object name -> ... lambda-object
  this.registerMethod('__register_method', function(p) {
	this.registerMethod(p[1], p[0]);
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
	console.log('ST', this.context.stack);
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
	r[1][name] = [f];
	return;
  }
  r[1][name].push(f);
}

Context.prototype.invokeMethod = function(m, args) {
  var r = this.find_var_lexical_scope(m);
  if(!r[0]) {
	throw new Error('Failed to invoke_method ' + m);
  }
  // TODO:
  //   * User defined methods
  //   * Find appropriate method:
  //     * Walk the array
  //     * Walk the lexical scopes chain
  if(r[1][m][0]) {
	r[1][m][0].apply(this, args);
	return;
  }
};

Context.prototype.invoke = function(methods, positional_args, named_args, vm) {
  // TODO:
  //   * User defined methods
  //   * Find appropriate method:
  //     * Walk the array
  //     * Walk the lexical scopes chain
  var m = methods[methods.length-1];

  // console.log('invoke', positional_args, named_args, vm);

  // 1. Native
  if(typeof m == 'function') {
	this.stack.push(m.call(this, positional_args, named_args, vm));
	return;
  }

  // 2. User defined
  if(m[0] === 'lambda') {
	this.frames.push({
	  lexical_scopes: this.lexical_scopes,
	  ip: this.ip,
	  stack_len: this.stack.length,
	});
	this.lexical_scopes = m[1][0];
	this.ip = m[1][1];
  }

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
	this.context.stack.push(this.context.ip);
  },

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

  // stack: ... var_name -> ... var_value
  'get_var': function() {
	var st = this.context.stack;
	var name = st.pop();
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
