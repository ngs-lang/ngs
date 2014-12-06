'use strict';
// apt-get install node-pegjs # 0.7.0
// pegjs syntax.pegs

var _ = require('underscore');

var parser = require('./syntax');

var uid = 1;
function uniq_id(pfx) {
  var ret = pfx + uid.toString();
  uid++;
  return ret;
}

function CodeChunk(node, main, pre, post) {
  this.node = node;
  this.pre = pre || '';
  this.main = main || '';
  this.post = post || '';
};

CodeChunk.prototype.toString = function() {
  return this.pre + this.main + this.post;
};

CodeChunk.prototype.use = function(other) {
  this.pre += other.pre;
  this.post += other.post;
  return this;
}

function dup_if_needed(code, leave_value_in_stack) {
  if(leave_value_in_stack) {
	return code.concat([
	  ['comment', 'leave_value_in_stack', true],
	  ['dup']
	]);
  }
  return code;
}

function pop_if_needed(code, leave_value_in_stack) {
  if(!leave_value_in_stack) {
	return code.concat([
	  ['comment', 'leave_value_in_stack', false],
	  ['pop']
	]);
  }
  return code;
}

function compile_tree(node, leave_value_in_stack) {
  if(leave_value_in_stack === undefined) {
	leave_value_in_stack = 1;
  }
  console.log('node', node, leave_value_in_stack);
  if(node.is('assignment')) {
    if(node[0].is('varname')) {
      var rhs = dup_if_needed(compile_tree(node[1]), leave_value_in_stack);
      return rhs.concat([
		['push', node[0].data],
		['push', '__set_var'],
		['invoke_method2'],
	  ]);
    }
    throw new Error("Assignment to type " + node[0] + " is not implemented");
  }
  if(node.is('commands') || node.is('top_level_expressions')) {
    var ret = [];
    for(var i=0; i<node.length; i++) {
	  var lvis;
	  if(i<node.length-1) {
		lvis = false;
	  } else {
		lvis = leave_value_in_stack;
	  }
      var t = compile_tree(node[i], lvis);
	  ret = ret.concat(t);
    }
    return ret;
  }
  if(node.is('if')) {
    var ret = compile_tree(node[0], true); // condition
    var t = compile_tree(node[1], leave_value_in_stack);
	var f;
	if(node[2]) {
	  f = compile_tree(node[2], leave_value_in_stack);
	} else {
	  f = [];
	}
	ret = ret.concat([
	  ['jump_if_false', t.length+1]
	], t, [
	  ['jump', f.length]
	], f);
    return ret;
  }
  if(node.is('number')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push', node.data]];
  }
  if(node.is('string')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push', node.data]];
  }
  if(node.is('varname')) {
	var ret = [
	  ['push', node.data],
	  ['push', '__get_var'],
	  ['invoke_method1']
	];
	if(!leave_value_in_stack) {
	  ret = ret.concat([
		['pop']
	  ]);
	}
	return ret;
  }
  if(node.is('exec')) {
    // TODO: real word expansion
	var ret = compile_tree(node[0]);
	ret = ret.concat([
	  ['push', 'exec'],
	  ['invoke_method1'],
	]);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('splice')) {
	return compile_tree(node[0], true);
  }
  if(node.is('array') || node.is('expressions')) {
	// TODO: implement 'expressions' here, for now it only tested with 'array'
    var ret = [
	  ['comment', 'start', node.node_type],
	  ['push', 'Array'],
	  ['invoke_method0']
	];
	var m;
    for(var i=0; i<node.length; i++) {
      var t = compile_tree(node[i]);
	  if(!node[i].is('splice')) {
		// because concat() leaves new array in stack but push() does not
		ret = ret.concat([
		  ['dup'],
		]);
	  }
	  ret = ret.concat(t);
	  if(node[i].is('splice')) {
		m = 'concat'
	  } else {
		m = 'push'
	  }
	  ret = ret.concat([
		['push', m],
		['invoke_method2'],
	  ]);
    }
    ret = pop_if_needed(ret, leave_value_in_stack);
	ret = ret.concat([['comment', 'end', node.node_type]]);
	return ret;
  }
  if(node.is('binop')) {
	// node.data -- operation name
    var ret = [].concat(
	  compile_tree(node[0], true),
      compile_tree(node[1], true),
	  [
		['push', '__' + node.data],
		['invoke_method2']
	  ]
	)
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('deftype')) {
    var ret = [
	  ['comment', 'start', node.node_type],
	  ['push', node.data],
	  ['push', 'Array'],
	  ['invoke_method0'],
	];
	for(var i=0; i<node.length; i++) {
	  ret = ret.concat([
		['comment', 'start', node.node_type, 'field'],
	  ])
	  ret = ret.concat([
		['dup'],
		['push', 'Array'],
		['invoke_method0'],
		['dup'],
		['push', node[i].data[0]],
		['push', 'push'],
		['invoke_method2'],
		['dup'],
		['push', node[i].data[1]],
		['push', 'push'],
		['invoke_method2'],
		['push', 'push'],
		['invoke_method2'],
		['comment', 'end', node.node_type, 'field'],
	  ]);
	}
	ret = ret.concat([
	  ['push', '__deftype'],
	  ['invoke_method2'],
	]);
	return ret;
  }
  /*
  if(node.is('func')) {
    return new CodeChunk(
      node,
      '(function() {\n' +
        compile_tree(node[1], pfx + INDENT).toString() +
        '})'
    );
  }
  if(node.is('ret')) {
    var e = compile_tree(node[0], pfx);
    if(e.post) {
      throw new Error('Compling "return" with post effects in expression is not supported yet');
    }
    return new CodeChunk(node, pfx + "return " + e.main + ";\n").use(e);
  }
  if(node.is('call')) {
    // TODO: fix splice - it doesn't work yet
    var args_array = [];
    var f = compile_tree(node[0], pfx);
    var ret = new CodeChunk(node, f.main).use(f);

    if(node[1]) {
      var args_array = compile_tree(node[1], pfx);
      ret.use(args_array);
    }
    ret.main += '(' + args_array.main + ')';

    return ret;

  }
  */
  if(node.node_type) {
    throw "Don't know how to compile type '" + node.node_type + "'";
  }
  throw "Don't know how to compile '" + node + "'";
}

// console.log(tree.commands[0].rhs);

if(require.main === module) {
  var BUFFER_SIZE = 1024*1024;
  var fs = require('fs');
  var program = fs.readSync(process.stdin.fd, BUFFER_SIZE);
  var tree = parser.parse(program[0]);
  var code = compile_tree(tree);
  console.log(code);
  var vm = require('./vm');
  var v = new vm.VM();
  v.useCode(code);
  v.start();
  console.log('stack', v.context.stack, 'globals', v.globals);
}

function compile(code, options) {
  var o = {
	leave_value_in_stack: false
  }
  _.extend(o, options || {});
  var tree = parser.parse(code);
  var compiled = compile_tree(tree, o.leave_value_in_stack);
  return {'compiled_code': compiled};
}

exports.compile = compile;
