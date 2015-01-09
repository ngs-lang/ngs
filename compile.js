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

function compile_invoke_no_args(method_name) {
  return [
	['push_arr'],
	['push_hsh'],
	['push_str', method_name],
	['get_var'],
	['invoke'],
  ];
}

function compile_invoke_pos_args_in_stack(method_name) {
  return [
	['push_hsh'],
	['push_str', method_name],
	['get_var'],
	['invoke'],
  ];
}

function compile_push() {
  return [
	['push_str', 'push'],
	['get_var'],
	['invoke2'],
  ];
}

function transform_args(node) {
  if(!node.is('parameters')) {
	throw new Error('transform_args() must handle "parameters" nodes only');
  }
  var ret = [['push_arr']];
  node.forEach(function(n) {
	if(n[0]) {
	  var param_type =
	  [
		['push_str', n[0].data]
	  ]
	} else {
	  var param_type =
	  [
		['push_nul']
	  ]
	};
	// console.log('XXX', n, param_type);
	ret = ret.concat(
	  [
		['push_arr'],
		['push_str', n.data],
	  ],
	  compile_push(),
	  [
		['push_str', n.node_type], // mode: arg_pos, arg_rest_pos, arg_rest_kw
	  ],
	  compile_push(),
	  param_type,
	  compile_push(),
	  compile_push()
	)
  })
  return ret;
}

function compile_tree(node, leave_value_in_stack) {
  if(leave_value_in_stack === undefined) {
	leave_value_in_stack = 1;
  }
  // console.log('node', node, leave_value_in_stack);
  if(node.is('assignment')) {
    if(node[0].is('varname')) {
      var rhs = dup_if_needed(compile_tree(node[1], true), leave_value_in_stack);
      return rhs.concat([
		['push_str', node[0].data],
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
    var ret = [];
	ret = ret.concat([
	  ['push_arr'],
	]);
	ret = ret.concat(compile_tree(node[0], true)); // condition
	ret = ret.concat(
	  compile_push(),
	  compile_invoke_pos_args_in_stack('Bool')
	);
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
  if(node.is('while')) {
    var ret = [];
	// ret = ret.concat(compile_pos_args(
	var inverse = node.data;
	var jump_cmd = inverse ? 'jump_if_true': 'jump_if_false';

	ret = ret.concat([
	  ['push_arr'],
	]);
	ret = ret.concat(compile_tree(node[0], true)); // condition
	ret = ret.concat(
	  compile_push(),
	  compile_invoke_pos_args_in_stack('Bool')
	);
    var body = compile_tree(node[1], false);
	ret = ret.concat(
	  [
		[jump_cmd, body.length + 1] // +1 for the jump instruction itself
	  ],
	  body
	);
	var jump_up = ret.length + 1; // +1 for the jump instruction itself
	ret = ret.concat([
	  ['jump', -jump_up],
	  ['push_nul'],
	]);
    return pop_if_needed(ret, leave_value_in_stack);

  }
  // TODO - refactor - start
  if(node.is('number')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push_num', node.data]];
  }
  if(node.is('string')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push_str', node.data]];
  }
  if(node.is('bool')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push_boo', node.data]];
  }
  if(node.is('null')) {
	if(!leave_value_in_stack) {
	  return [];
	}
	return [['push_nul']];
  }
  // TODO - refactor - end
  if(node.is('varname')) {
	var ret = [
	  ['push_str', node.data],
	  ['get_var'],
	];
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('exec')) {
    // TODO: real word expansion
	var ret = compile_tree(node[0]);
	ret = ret.concat(compile_invoke_pos_args_in_stack('exec'));
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
	ret = ret.concat(compile_invoke_no_args('Array'));
	var m;
    for(var i=0; i<node.length; i++) {
      var t = compile_tree(node[i]);
	  ret = ret.concat(t);
	  if(node[i].is('splice')) {
		m = '__add'
	  } else {
		m = 'push'
	  }
	  ret = ret.concat([
		['push_str', m],
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
		['push_str', '__' + node.data],
		['get_var'],
		['invoke2'],
	  ]
	)
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('deftype')) {
	// TODO: new call convention
	var mk_array = compile_invoke_no_args('Array');
    var ret = [].concat(
	  [
		['comment', 'start', node.node_type],
		['push_str', node.data],
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
		  ['push_str', node[i].data[0]],
		  ['push_str', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['push_str', node[i].data[1]],
		  ['push_str', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['push_str', 'push'],
		  ['get_var'],
		  ['invoke2'],
		  ['comment', 'end', node.node_type, 'field'],
		]
	  );
	}
	ret = ret.concat([
	  ['push_str', '__deftype'],
	  ['get_var'],
	  ['invoke2'],
	]);
	return ret;
  }
  if(node.is('defun')) {
	var ret = compile_tree(node[0], true);
	ret = ret.concat([
	  ['push_str', node.data],
	  ['push_str', '__register_method'],
	  ['get_var'],
	  ['invoke2'],
	]);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('lambda')) {
	var code = compile_tree(node[1], true);
	var ret = [].concat(
	  compile_invoke_no_args('Array'),
	  // Lexical scopes
	  compile_invoke_no_args('__get_lexical_scopes'),
	  compile_push(),
	  transform_args(node[0]),
	  compile_push(),
	  [
		// IP
		['push_ip'],
		['jump', code.length],
	  ],
	  code,
	  [
		['push_num', 1],
		['push_str', '__add'],
		['get_var'],
		['invoke2'],
	  ],
	  compile_push(),
	  compile_invoke_pos_args_in_stack('__lambda')
	);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('call')) {
	var methods = compile_tree(node[0]);
	var positional_args = compile_tree(node[1]);
	var ret = [].concat(
	  positional_args,
	  [
		['push_hsh'],
	  ],
	  methods,
	  [
		['invoke']
	  ]
	);
	return pop_if_needed(ret, leave_value_in_stack);
  }
  if(node.is('ret') || node.is('guard')) {
	var ret = compile_tree(node[0], true);
	ret = ret.concat([
	  [node.node_type],
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
