'use strict';
// apt-get install node-pegjs # 0.7.0
// pegjs syntax.pegs

var _ = require('underscore');

var parser = require('./syntax');

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

function compile_invoke(method_name, positional_args, named_args) {
  return [
	['push', positional_args || []],
	['push', named_args || {}],
	['push', method_name],
	['get_var'],
	['invoke'],
  ];
}

function compile_push() {
  return [
	['push', 'push'],
	['get_var'],
	['invoke2'],
  ];
}

function transform_args(node) {
  if(!node.is('parameters')) {
	throw new Error('transform_args() must handle "parameters" nodes only');
  }
  return node.map(function(n) { return [n.data, n.node_type]; });
}

function compile_tree(node, leave_value_in_stack) {
  if(leave_value_in_stack === undefined) {
	leave_value_in_stack = 1;
  }
  console.log('node', node, leave_value_in_stack);
  if(node.is('assignment')) {
    if(node[0].is('varname')) {
      var rhs = dup_if_needed(compile_tree(node[1], true), leave_value_in_stack);
      return rhs.concat([
		['push', node[0].data],
		['set_var'],
	  ]);
    }
    throw new Error("Assignment to type " + node[0] + " is not implemented");
  }
  if(node.is('commands') || node.is('top_level_expressions')) {
    var ret = [];
    for(var i=0; i<node.length; i++) {
	  var lvis = (i == node.length-1) && leave_value_in_stack;
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
	  ['get_var'],
	];
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('exec')) {
    // TODO: real word expansion
	var ret = compile_tree(node[0]);
	ret = ret.concat([
	  ['push', {}],
	  ['push', 'exec'],
	  ['get_var'],
	  ['invoke'],
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
	];
	ret = ret.concat(compile_invoke('Array', [], {}));
	var m;
    for(var i=0; i<node.length; i++) {
      var t = compile_tree(node[i]);
	  ret = ret.concat(t);
	  if(node[i].is('splice')) {
		m = 'concat'
	  } else {
		m = 'push'
	  }
	  ret = ret.concat([
		['push', m],
		['get_var'],
		['invoke2'],
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
		['get_var'],
		['invoke2'],
	  ]
	)
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('deftype')) {
	// TODO: new call convention
	var mk_array = compile_invoke('Array', [], {});
    var ret = [].concat(
	  [
		['comment', 'start', node.node_type],
		['push', node.data],
	  ],
	  mk_array
	);
	for(var i=0; i<node.length; i++) {
	  ret = ret.concat([
		['comment', 'start', node.node_type, 'field'],
	  ]);
	  ret = ret.concat(
		mk_array,
		[
		  ['push', node[i].data[0]],
		  ['push', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['push', node[i].data[1]],
		  ['push', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['push', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['comment', 'end', node.node_type, 'field'],
		]
	  );
	}
	ret = ret.concat([
	  ['push', '__deftype'],
	  ['get_var'],
	  ['invoke2'],
	]);
	return ret;
  }
  if(node.is('defun')) {
	var ret = compile_tree(node[0], true);
	ret = ret.concat([
	  ['push', node.data],
	  ['push', '__register_method'],
	  ['get_var'],
	  ['invoke2'],
	]);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('lambda')) {
	// XXX
	var code = compile_tree(node[1], true);
	// code = code.concat(
	//   [
	// 	['push', null],
	// 	['return']
	//   ]
	// );

	console.log('FUNC CODE', code, leave_value_in_stack);
	var ret = [].concat(
	  compile_invoke('Array'),
	  // Lexical scopes
	  compile_invoke('__get_lexical_scopes'),
	  compile_push(),
	  [
		['push', transform_args(node[0])],
	  ],
	  compile_push(),
	  [
		// IP
		['push_ip'],
		['jump', code.length],
	  ],
	  code,
	  [
		['push', 1],
		['push', '__add'],
		['get_var'],
		['invoke2'],
	  ],
	  compile_push(),
	  [
		['push', {}],
		['push', '__lambda'],
		['get_var'],
		['invoke'],
	  ]);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('call')) {
	var methods = compile_tree(node[0]);
	var positional_args = compile_tree(node[1]);
	var ret = [].concat(
	  positional_args,
	  [
		['push', {}],
	  ],
	  methods,
	  [
		['invoke']
	  ]
	);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('ret')) {
	var ret = compile_tree(node[0], true);
	ret = ret.concat([
	  ['ret'],
	]);
	return ret;

  }
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
  console.log('stack', v.context.stack, 'lexical_scopes', v.lexical_scopes);
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
