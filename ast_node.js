'use strict';

// Big TODO: Refactor to use real OO:
//   1. use subclasses of ASTNode for different types instead of ASTNode.node_type
//   2. move any ASTNode.node_type specific code to these subclasses
//   3. ASTNode.precedence should only be in relevant subclasses
// What's going on here is classic non-OO programming but using classes :)
// PS. Forgive me for using "classes" in classless language :)

var _ = require('underscore');

function ASTNode(node_type, offset, sub_nodes, data, precedence) {
	if(!this) {
		return new ASTNode(node_type, offset, sub_nodes, data, precedence);
	}
	this.initialize(node_type, offset, sub_nodes, data, precedence);
}

ASTNode.prototype = Object.create(Array.prototype);

ASTNode.prototype.initialize = function(node_type, offset, sub_nodes, data, precedence) {
	this.node_type = node_type;
	this.offset = offset;
	if(data === undefined) {
		this.data = null;
	} else {
		this.data = data;
	}
	this.precedence = precedence || null;
	if(sub_nodes) {
		for(var i=0; i<sub_nodes.length; i++) {
			this[i] = sub_nodes[i];
		}
		this.length = sub_nodes.length;
	}
	return this;
}

ASTNode.prototype.push = function(sub_node) {
	if(!sub_node) {
		return this;
	}
	Array.prototype.push.call(this, sub_node);
	return this;
}

ASTNode.prototype.is = function(node_type) {
	return this.node_type === node_type;
}

ASTNode.prototype.same_precedence_binops_p = function(other) {
	if((this.node_type !== 'binop') || (other.node_type !== 'binop')) {
		return false;
	}
	// Lower number is more binding, not that it matters here.
	return this.precedence === other.precedence;
}

ASTNode.prototype.toString = function(depth) {
	depth = depth || 0;
	var d = this.data !== null ? ' ' + this.data : '';
	var p = this.precedence !== null ? ' precedence=' + this.precedence : '';
	var ret = _repr_depth(depth) + '+ ' + this.node_type + ' @' + this.offset + d + p + '\n';
	for(var i=0; i<this.length; i++) {
		ret += this[i].toString(depth+1);
	}
	return ret;
}

// Array.prototype.concat returns Array object, not ASTNode object
ASTNode.prototype.concat = function(other) {
	var ret = new ASTNode(this.node_type, this.offset, [], this.data);
	for(var i=0; i<this.length; i++) {
		ret[i] = this[i];
	}
	for(var i=this.length, j=0; j<other.length; i++, j++) {
		ret[i] = other[j];
	}
	ret.length = this.length + other.length;
	return ret;
}

ASTNode.prototype.map = function(f) {
	var ret = new ASTNode(this.node_type, this.offset, [], this.data, this.precedence);
	for(var i=0; i<this.length; i++) {
		ret[i] = f(this[i]);
	}
	ret.length = this.length;
	return ret;
}

function _repr_depth(depth) {
	var ret = '';
	for(var i=0;i<depth;i++) {
		ret = ret + '  ';
	}
	return ret;
}

exports.ASTNode = ASTNode;
