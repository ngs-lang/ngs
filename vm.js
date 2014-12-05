'use strict';

// Naive implementation, not optimized.
// Highly likely to be a throw-away so the major concern is simplicity.

var _ = require('underscore');

function Context() {
  return this.initialize();
}

Context.prototype.initialize = function() {
  this.ip = 0;
  this.stack = [];

  // Don't want pop() method to be listed and printed in console.log()
  Object.defineProperty(this.stack, 'pop', {
	'value': function() {
	  if(this.length == 0) {
		throw new Error("Stack underflow at " + this.ip);
	  }
	  return Array.prototype.pop.call(this);
	}
  });
  return this;
}



function VM() {
  return this.initialize();
}

VM.prototype.initialize = function() {
  this.code = [];
  this.runnable_contexts = [];
  this.suspended_contexts = [];
  this.finished_contexts = [];
  this.context = null;
  this.globals = {};
  this.methods = {}; // TODO: make it hash of arrays, not hash of scalars
  this.register_method('Array', function() {
	this.context.stack.push(new Array());
  });

  // stack: ... -> ... Array
  this.register_method('Array', function() {
	this.context.stack.push(new Array());
  });

  // stack: ... array value -> ...
  this.register_method('push', function(a, v) {
	a.push(v);
  });

  // stack: ... array1 array2 -> ... arrayConcat
  this.register_method('concat', function(a1, a2) {
	this.context.stack.push(a1.concat(a2));
  });

  // stack: ... value varname -> ...
  this.register_method('setvar', function(val, name) {
	this.globals[name] = val;
  });

  // stack: ... varname -> ... value
  this.register_method('getvar', function(name) {
	this.context.stack.push(this.globals[name]);
  });
  
  // stack: ... args -> ... job
  this.register_method('getvar', function(name) {
	this.context.stack.push(this.globals[name]);
  });

  // stack: ... v1 v2 -> ... v
  this.register_method('add', function(v1, v2) {
	var st = this.context.stack;
	// TODO: when multi-method is implemented, move to another method
	if(_.isArray(v1) && _.isArray(v2)) {
      st.push(v1.concat(v2));
	  return;
    }
	st.push(v1+v2);
  });

  // stack: ... v1 v2 -> ... v
  this.register_method('sub', function(v1, v2) {
	var st = this.context.stack;
	st.push(v1-v2);
  });

  // stack: ... v
  this.register_method('echo', function(v) {
	console.log('ECHO', v);
  });

  return this;
}

VM.prototype.useCode = function(c) {
  // TODO: assert that there are no cotext pointing to existing this.code
  //       (which is being replaced)
  this.code = c;
  return this;
}

VM.prototype.start = function(finished_callback) {
  var c = new Context();
  this.runnable_contexts.push(c);
  this.finished_callback = finished_callback;
  this.main_loop();
  
}

VM.prototype.main_loop = function() {
  while(this.runnable_contexts.length) {
	this.context = this.runnable_contexts[0];
	var op = this.code[this.context.ip];
	if(!op) {
	  this.finished_contexts.push(this.runnable_contexts.shift());
	  continue;
	}
	this.context.ip++;
	console.log('ST', this.context.stack);
	console.log('OP', op);
	console.log('');
	if(op[0] !== 'comment') {
	  this.opcodes[op[0]].call(this, op[1]);
	}
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
	this.main_loop();
  }.bind(this), 0);
}

VM.prototype.register_method = function(name, f) {
  // TODO: types
  this.methods[name] = f;
}

VM.prototype.invoke_method = function(m, args) {
  if(this.methods[m]) {
	this.methods[m].apply(this, args);
  } else {
	throw new Error('Failed to invoke_method ' + m);
  }
};

VM.prototype.opcodes = {
  
  // stack: ... -> ... value
  'push': function(v) {
	// ideally v is a scalar
	this.context.stack.push(v);
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

  // stack: ... method -> ...
  'invoke_method0': function() {
	var st = this.context.stack;
	var m = st.pop();
	this.invoke_method(m, []);
  },

  // stack: ... obj method -> ...
  'invoke_method1': function() {
	var st = this.context.stack;
	var m = st.pop();
	var obj = st.pop();
	this.invoke_method(m, [obj]);
  },

  // stack: ... obj arg method -> ...
  'invoke_method2': function() {
	var st = this.context.stack;
	var m = st.pop();
	var arg2 = st.pop();
	var arg1 = st.pop();
	this.invoke_method(m, [arg1, arg2]);
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
