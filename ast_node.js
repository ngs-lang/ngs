'use strict';

function ASTNode(node_type, sub_nodes, data) {
  if(!this) {
	return new ASTNode(node_type, sub_nodes, data);
  }
  this.initialize(node_type, sub_nodes, data);
}

ASTNode.prototype = Object.create(Array.prototype);

ASTNode.prototype.initialize = function(node_type, sub_nodes, data) {
  this.node_type = node_type;
  this.data = data || null;
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

ASTNode.prototype.toString = function() {
  return '<ASTNode type=' + this.node_type + ' children=[' + Array.prototype.toString.call(this)+ ']>';
}

// Array.prototype.concat returns Array object, not ASTNode object
ASTNode.prototype.concat = function(other) {
  var ret = new ASTNode(this.node_type, [], this.data);
  for(var i=0; i<this.length; i++) {
	ret[i] = this[i];
  }
  for(var i=this.length, j=0; j<other.length; i++, j++) {
	ret[i] = other[j];
  }
  ret.length = this.length + other.length;
  return ret;
}

exports.ASTNode = ASTNode;

// var N = ASTNode('t', [7]);
// var N2 = ASTNode('tt', [8]).concat(N);
// console.log(N2.toString());
