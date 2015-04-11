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
	// console.log('fix_binops_node', node);
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
		// TODO: fix offset later
		ret.splice(op_idx - 1, 3, new N('binop', ret[op_idx].offset, [ret[op_idx-1], ret[op_idx+1]], ret[op_idx].data, -1));
	}
	if(ret[0].is('binops')) {
		ret[0] = fix_binops_node(ret[0]);
	}

	// e1 @ e2 -> map(e1, F(X=null, Y=null) { e2 })
	if(ret[0].is('binop') && (ret[0].data.substring(0, 7)=='inline_')) {
		var o = ret[0].offset;
		var p1 = new N('arg_nam', o, [new N('string', o, [new N('null', o, [], false)], 'X'), new N('null', o, [], false)]);
		var p2 = new N('arg_nam', o, [new N('string', o, [new N('null', o, [], false)], 'Y'), new N('null', o, [], false)]);
		var f = new N(
			'lambda',
			o,
			[
				new N('string', o, [], ret[0].data + '_' + o),
				new N('parameters', o, [p1, p2]),
				ret[0][1]
			]
		);
		return new N(
			'call',
			o,
			[
				new N('varname', o, [], ret[0].data.substring(7)),
				new N('expressions', o, [ret[0][0], f])
			],
			false
		);
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
		if(code[i][1] === '$BREAK') {
			code[i] = [code[i][0], 'jump', code.length - i - 1 /* from next instruction */ + delta];
		}
	}
}

function process_continue(code, delta) {
	for(var i=0; i<code.length; i++) {
		if(code[i][1] === '$CONTINUE') {
			code[i] = [code[i][0], 'jump', - i - 1 /* from next instruction */ + delta];
		}
	}
}

function transform_args(node) {
	if(!node.is('parameters')) {
		throw new Error('transform_args() must handle "parameters" nodes only');
	}
	var ret = [['push_arr']];
	node.forEach(function(n) {
		ret = ret.concat(
			[
				['push_arr'],
			],
			compile_tree(n[0], true), // name
			compile_push(),
			[
				['push_str', n.node_type], // mode: arg_pos, arg_rest_pos, arg_rest_kw
			],
			compile_push()
		);
		// type and optional default value
		for(var i=1;i<n.length;i++) {
			ret = ret.concat(
				compile_tree(n[i], true),
				compile_push()
			);
		}
		ret = ret.concat(compile_push());
	})
	return ret;
}

function compile_tree(node, leave_value_in_stack) {
	var ret = compile_tree_kern(node, leave_value_in_stack);
	ret = ret.map(function(op) {
		if(typeof(op[0]) == 'string') {
			return [node.offset].concat(op);
		} else {
			return op;
		}
	});
	return ret;
}

function compile_tree_kern(node, leave_value_in_stack) {
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
				[node.data.global ? 'set_glo_var': 'set_loc_var'],
			]);
		}
		throw new Error("Assignment to type " + node[0] + " is not implemented");
	}
	if(node.is('commands') || node.is('top_level_expressions')) {
		var last_non_comment = node.length-1;
		while((last_non_comment>=0) && (node[last_non_comment].is('comment'))) {
			last_non_comment--;
		}
		var j = last_non_comment+1;
		for(var i=0; i<j; i++) {
			var lvis = (i == last_non_comment) && leave_value_in_stack;
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
		var body = compile_tree(node[1], false);
		process_break(body, 1); // + 1 for jump up instruction
		process_continue(body, 0);

		cmd('push_arr');
		concat(compile_tree(node[0], true)); // condition
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack('Bool'));
		cmd('jump_if_false', body.length + 1); // +1 for the jump
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
		return [['$BREAK']];
	}
	if(node.is('continue')) {
		return [['$CONTINUE']];
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
	if(node.is('defun')) {
		concat_tree(1, true); // Lambda
		concat_tree(0, true); // Name
		cmd('push_boo', node.data.global);
		cmd('push_str', '__register_method');
		cmd('get_var');
		cmd('invoke3');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('lambda')) {
		var code = compile_tree(node[2], true);
		code = code.concat([['ret']]);
		concat(compile_invoke_no_args('Array'));
		// Lexical scopes
		concat(compile_invoke_no_args('__get_lexical_scopes'));
		concat(compile_push());
		concat(transform_args(node[1]));
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
		concat_tree(0, true);
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
	if(node.is('string_container')) {
		cmd('push_str', '');
		var last_is_string = ret.length;
		// concat_idx - after compile_tree() the index of the immediate string is 2, because of prepended offset
		var concat_idx = 1;
		for(var i=0; i<node.length; i++) {
			var elt = node[i];
			if(elt.is('string') && last_is_string) {
				ret[last_is_string-1][concat_idx] += elt.data;
				continue;
			}
			cmd('comment', 'string_container tree start')
			if(!elt.is('string')) {
				// Only invoke String() for non-immediate strings
				cmd('push_arr');
			}
			concat(compile_tree(elt, true));
			if(elt.is('string')) {
				last_is_string = ret.length;
				concat_idx = 2;
			} else {
				last_is_string = 0;
				// Only invoke String() for non-immediate strings
				concat(compile_push());
				concat(compile_invoke_pos_args_in_stack('String'));
			}
			cmd('push_str', '__add');
			cmd('get_var');
			cmd('invoke2');
			cmd('comment', 'string_container tree end')
		}
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('regexp')) {
		concat_tree(0, true);
		concat_tree(1, true);
		cmd('push_str', 'Regexp');
		cmd('get_var');
		cmd('invoke2');
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('object_literal')) {
		cmd('push_arr');
		concat_tree(1, true); // Literal string
		concat(compile_push());
		cmd('push_hsh');
		cmd('push_str', '__literal_')
		concat_tree(0, true); // Name
		cmd('push_str', '__add');
		cmd('get_var');
		cmd('invoke2');
		cmd('get_var');
		cmd('invoke');
		return pop_if_needed(ret, leave_value_in_stack);
	}

	function concat_capture(f) {
		cmd('push_arr');
		concat_tree(0, true);
		concat(compile_push());
		concat(compile_invoke_pos_args_in_stack(f));
	}
	if(node.is('capture')) {
		concat_capture('__capture')
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.is('capture_parse')) {
		concat_capture('__parse')
		return pop_if_needed(ret, leave_value_in_stack);
	}
	if(node.node_type) {
		throw "Don't know how to compile type '" + node.node_type + "'";
	}
	throw "Don't know how to compile '" + node + "'";
}

function get_newlines_positions(code) {
	var ret = [];
	for(var i=0; i<code.length; i++) {
		if(code[i] == '\n') {
			ret.push(i);
		}
	}
	return ret;
}
/*
defg get_newlines_positions(code:String) {
	code.len().map(@if code[X] == '\n' {X}).filter()
}
*/

var offset_to_line_and_col = (function() {
	var prev_positions = null;
	var prev_offset = null;
	var prev_result = null;
	var hits = 0;
	var misses = 0;
	function offset_to_line_and_col(positions, offset) {
		// TODO: binary search
		var ret = null;
		if((positions === prev_positions) && (offset === prev_offset)) {
			hits++;
			return prev_result;
		}
		for(var i=0; i<positions.length; i++) {
			if(positions[i] > offset) {
				ret = [i+1, positions[i] - offset + 1];
				break;
			}
		}
		if(!ret) {
			ret = [positions.length, positions[positions.length] - offset + 1]
		}
		prev_positions = positions;
		prev_offset = offset;
		prev_result = ret;
		misses++;
		return ret;
	}
	offset_to_line_and_col.stats = function() {
		console.log('offset_to_line_and_col() hits and misses:', hits, misses);
	}
	return offset_to_line_and_col;
})();

function compile(code, fname, options) {
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

	// debug data processing - start
	var newlines_positions = get_newlines_positions(code);
	compiled.forEach(function(op, i) {
		if(op[0] === null) {
			return;
		}
		op[0] = [i+1].concat(offset_to_line_and_col(newlines_positions, op[0]));
	});
	compiled = [[null, 'src_file', fname]].concat(compiled);
	// debug data processing - end

	if(process.env.NGS_DEBUG_COMPILED) {
		console.log('COMPILED');
		for(var i=0; i<compiled.length; i++) {
			console.log(compiled[i]);
		}
		offset_to_line_and_col.stats();
	}
	return {'compiled_code': compiled};
}

exports.compile = compile;
