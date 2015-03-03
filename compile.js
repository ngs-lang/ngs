'use strict';
// apt-get install node-pegjs # 0.7.0
// pegjs syntax.pegs

var util = require('util');

var _ = require('underscore');

var parser = require('./syntax');
var N = require('./ast_node').ASTNode;

var PUSH_NODES = {
	'number': true,
	'string': true,
	'bool': true,
	'null': true
};

var CALL_NODES = {
	'get_item': true,
	'set_item': true,
	'in': true,
	'not_in': true,
}

function fix_binops_node(node) {
	var ret = node;
	while(ret.length > 1) {
		// find the most binding op and turn it into binop node
		var op_idx = null;
		var op_precedence = -1;
		for(var i=1; i<ret.length; i+=2) {
			if(ret[i].precedence > op_precedence) {
				op_idx = i;
				op_precedence = ret[i].precedence;
			}
		}
		ret.splice(op_idx - 1, 3, N('binop', [ret[op_idx-1], ret[op_idx+1]], ret[op_idx].data, -1));
	}
	if(ret[0].is('binops')) {
		ret[0] = fix_binops_node(ret[0]);
	}
	return ret[0];
}

function fix_binops(tree) {
	var ret = tree;
	if(ret.is('binops')) {
		ret = fix_binops_node(ret);
	}
	return ret.map(fix_binops);
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

function null_if_needed(code, leave_value_in_stack) {
	if(leave_value_in_stack) {
		return code.concat([
			['comment', 'leave_value_in_stack', true],
			['push_nul']
		]);
	}
	return code;
}

function pop_if_needed(code, leave_value_in_stack) {
	if(leave_value_in_stack === undefined) {
		throw new Error("pop_if_needed requires two arguments");
	}
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

function process_break(code, delta) {
	for(var i=0; i<code.length; i++) {
		if(code[i] === '$BREAK') {
			code[i] = ['jump', code.length - i - 1 /* from next instruction */ + delta];
		}
	}
}

function process_continue(code, delta) {
	for(var i=0; i<code.length; i++) {
		if(code[i] === '$CONTINUE') {
			code[i] = ['jump', - i - 1 /* from next instruction */ + delta];
		}
	}
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
	var ret = [];
	if(leave_value_in_stack === undefined) {
		leave_value_in_stack = 1;
	}
	function concat(a) {
		ret = ret.concat(a);
	}
	function cmd() {
		concat([Array.prototype.slice.call(arguments)]);
	}
	function concat_tree(i, lvs) {
		concat(compile_tree(node[i], lvs));
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
		for(var i=0; i<node.length; i++) {
			var lvis = (i == node.length-1) && leave_value_in_stack;
			var t = compile_tree(node[i], lvis);
			// console.log('IN', node[i], 'OUT', t);
			ret = ret.concat(t);
		}
		return ret;
	}
	if(node.is('if')) {
		cmd('push_arr');
		concat_tree(0, true); // condition
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('Bool'))
		var t = compile_tree(node[1], leave_value_in_stack);
		var f;
		if(node[2]) {
			f = compile_tree(node[2], leave_value_in_stack);
		} else {
			f = null_if_needed([], leave_value_in_stack);
		}
		cmd('jump_if_false', t.length+1);
		concat(t);
		cmd('jump', f.length);
		concat(f);
		return ret;
	}
	if(node.is('while')) {
		var inverse = node.data;
		var jump_cmd = inverse ? 'jump_if_true': 'jump_if_false';

		var body = compile_tree(node[1], false);
		process_break(body, 1); // + 1 for jump up instruction
		process_continue(body, 0);

		cmd('push_arr');
		concat(compile_tree(node[0], true)); // condition
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('Bool'));
		cmd(jump_cmd, body.length + 1); // +1 for the jump
		concat(body);
		var jump_up = ret.length + 1; // +1 for the jump instruction itself
		cmd('jump', -jump_up);
		return null_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('for')) {
		// 0: init, 1:cond, 2:incr, 3:body
		var init = compile_tree(node[0], false);
		var cond = [].concat(
			[
				['push_arr'],
			],
			compile_tree(node[1], true),
			compile_push(),
			compile_invoke_pos_args_in_stack('Bool')
		);
		var incr = compile_tree(node[2], false);
		var body = compile_tree(node[3], false);
		var jump_up =
			cond.length +
			1 /* jump_if_false */ +
			body.length +
			incr.length +
			1 /* jump */;
		process_break(body, incr.length + 1); // + 1 for jump up instruction
		// typically continue jumps to start of the body
		// but we need it at the end, the "incr" part
		process_continue(body, body.length);

		concat(init);
		concat(cond);
		cmd('jump_if_false', body.length + incr.length + 1);
		concat(body);
		concat(incr);
		cmd('jump', -jump_up);
		return null_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('break')) {
		return ['$BREAK'];
	}
	if(node.is('continue')) {
		return ['$CONTINUE'];
	}
	if(PUSH_NODES[node.node_type]) {
		if(!leave_value_in_stack) {
			return [];
		}
		return [['push_' + node.node_type.slice(0, 3), node.data]];
	}
	if(CALL_NODES[node.node_type]) {
		cmd('push_arr');
		for(var i=0; i<node.length; i++) {
			concat(compile_tree(node[i], true))
			concat(compile_push());
		}
		cmd('push_hsh');
		cmd('push_str', '__' + node.node_type);
		cmd('get_var');
		cmd('invoke');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('varname')) {
		cmd('push_str', node.data);
		cmd('get_var');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('spawn')) {
		// TODO: real word expansion
		concat_tree(0, true);
		concat(compile_invoke_pos_args_in_stack('spawn'));
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('splice')) {
		return compile_tree(node[0], true);
	}
	if(node.is('array') || node.is('expressions')) {
		// TODO: implement 'expressions' here, for now it only tested with 'array'
		cmd('comment', 'start', node.node_type);
		concat(compile_invoke_no_args('Array'));
		var m;
		for(var i=0; i<node.length; i++) {
			concat_tree(i, true);
			if(node[i].is('splice')) {
				m = '__add'
			} else {
				m = 'push'
			}
			cmd('push_str', m);
			cmd('get_var');
			cmd('invoke2');
		}
		ret = pop_if_needed(ret, leave_value_in_stack);
		cmd('comment', 'end', node.node_type);
		return ret;
	}
	if(node.is('hash')) {
		concat(compile_invoke_no_args('Hash'));
		for(var i=0; i<node.length; i++) {
			concat(compile_tree(node[i][0]), true);
			concat(compile_tree(node[i][1]), true);
			cmd('push_str', '__set_attr');
			cmd('get_var');
			cmd('invoke3');
		}
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('binop') && ((node.data == 'and') || (node.data == 'or'))) {
		concat_tree(0, true);
		cmd('dup');
		cmd('push_arr');
		cmd('xchg');
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('Bool'));
		var l = ret.length;
		var jump_cmd = (node.data == 'and' ? 'jump_if_false' : 'jump_if_true')
		cmd(jump_cmd, 0); // unknown yet
		cmd('pop')
		concat_tree(1, true)
		ret[l][1] = ret.length - l - 1; // minus jump_if_false instruction
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('binop')) {
		// node.data -- operation name
		concat_tree(0, true);
		concat_tree(1, true);
		cmd('push_str', '__' + node.data);
		cmd('get_var');
		cmd('invoke2');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('deftype')) {
		// TODO: new call convention
		var mk_array = compile_invoke_no_args('Array');
		cmd('comment', 'start', node.node_type);
		cmd('push_str', node.data);
		concat(mk_array)
		for(var i=0; i<node.length; i++) {
			cmd('comment', 'start', node.node_type, 'field');
			concat(mk_array);
			cmd('push_str', node[i].data[0]);
			cmd('push_str', 'push');
			cmd('get_var');
			cmd('invoke2');
			cmd('push_str', node[i].data[1]);
			cmd('push_str', 'push');
			cmd('get_var');
			cmd('invoke2');
			cmd('push_str', 'push');
			cmd('get_var');
			cmd('invoke2');
			cmd('comment', 'end', node.node_type, 'field');
		}
		cmd('push_str', '__deftype');
		cmd('get_var');
		cmd('invoke2');
		return ret;
	}
	if(node.is('defun')) {
		concat_tree(0, true);
		cmd('push_str', node.data);
		cmd('push_str', '__register_method');
		cmd('get_var');
		cmd('invoke2');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('lambda')) {
		var code = compile_tree(node[1], true);
		code = code.concat([['ret']]);
		concat(compile_invoke_no_args('Array'));
		// Lexical scopes
		concat(compile_invoke_no_args('__get_lexical_scopes'));
		concat(compile_push());
		concat(transform_args(node[0]));
		concat(compile_push());
		// IP
		cmd('push_ip');
		cmd('jump', code.length);
		concat(code);
		cmd('push_num', 1);
		cmd('push_str', '__add');
		cmd('get_var');
		cmd('invoke2');
		concat(compile_push());
		cmd('push_str', node.data ? node.data : '');
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('__lambda'));
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('call')) {
		concat_tree(1, true); // positional_args
		cmd('push_hsh');
		concat_tree(0, true); // methods
		var c = node.data ? 'invoke_catch' : 'invoke';
		cmd(c);
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('ret')) {
		concat_tree(0, true);
		cmd('ret');
		return ret;
	}
	if(node.is('throw')) {
		concat_tree(0, true);
		cmd('thr');
		return ret;
	}
	if(node.is('guard')) {
		cmd('push_arr');
		concat_tree(0, true);
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('Bool'));
		cmd('guard');
		return ret;
	}
	if(node.is('match')) {
		var cases = node[1];

		// --- preapre args as in 'call' - start ---
		// positional_args for cases
		concat_tree(0, true);
		// named_args for cases
		cmd('push_hsh');
		// --- preapre args as in 'call' - end ---

		concat(compile_invoke_no_args('Array'));
		for(var i=cases.length-1; i>=0; i--) {
			cmd('comment', 'catch clause ' + i + ' start');
			concat(compile_tree(cases[i], true));
			cmd('comment', 'catch clause ' + i + ' push result');
			concat(compile_push());
			cmd('comment', 'catch clause ' + i + ' end');
		} // for cases

		cmd('invoke');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('comment')) {
		cmd('comment', 'user: ' + node.data);
		return ret;
	}
	if(node.is('get_attr')) {
		cmd('push_arr');
		concat_tree(0, true);
		concat(compile_push());
		cmd('push_str', node.data);
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('__get_attr'));
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.node_type) {
		throw "Don't know how to compile type '" + node.node_type + "'";
	}
	throw "Don't know how to compile '" + node + "'";
}

function compile(code, options) {
	var debug_ast = process.env.NGS_DEBUG_AST;
	var o = {
		start_rule: 'commands',
		leave_value_in_stack: false
	}
	_.extend(o, options || {});
	try {
		var tree = parser.parse(code, o.start_rule);
	} catch(e) {
		console.log(e);
		throw e;
	}
	if(debug_ast) {
		console.log('AST BEFORE fix_binops()\n', tree.toString());
	}
	tree = fix_binops(tree);
	if(debug_ast) {
		console.log('AST AFTER fix_binops()\n', tree.toString());
	}
	var compiled = compile_tree(tree, o.leave_value_in_stack);
	if(process.env.NGS_DEBUG_COMPILED) {
		console.log('COMPILED');
		for(var i=0; i<compiled.length; i++) {
			console.log(i, compiled[i]);
		}
	}
	return {'compiled_code': compiled};
}

exports.compile = compile;
