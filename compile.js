'use strict';
// apt-get install node-pegjs # 0.7.0
// pegjs syntax.pegs

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


var INDENT = '\t';
function compile_tree(node, pfx) {
  // console.log('node', node);
  if(node.is('assignment')) {
    if(node[0].is('varname')) {
      var rhs = compile_tree(node[1], pfx);
      // console.log('XXX', rhs, rhs['main']);
      return new CodeChunk(node, pfx+"set_var('" + node[0].data + "', " + rhs.main + ");\n", rhs.pre, rhs.post);
    }
    throw new Error("Assignment to type " + node[0] + " is not implemented");
  }
  if(node.is('commands')) {
    var uid = uniq_id('cmds$');
    var ret = new CodeChunk(node, uid, pfx + 'var ' + uid + ';\n');
    for(var i=0; i<node.length; i++) {
      var t = compile_tree(node[i], pfx);
      ret.use(t);
      ret.pre += pfx + uid + ' = ' + t.main + ';\n';
    }
    return ret;
  }
  if(node.is('if')) {
    var f = null;
    var c = compile_tree(node[0], pfx);
    var t = compile_tree(node[1], pfx + INDENT);
    if(node[2]) {
      f = compile_tree(node[2], pfx + INDENT);
    }
    var uid = uniq_id('tmp_if$');
    var ret = new CodeChunk(node, uid);
    ret.pre += c.pre;
    ret.pre += pfx + 'var '+uid+'=null; /* temp if result */\n';
    ret.pre += pfx + 'if(' + c.main + ') {\n';
    ret.pre += t.pre;
    ret.pre += pfx + INDENT + uid + '=' + t.main + ';\n';
    ret.pre += t.post;
    ret.pre += pfx + '}';
    if(f) {
      ret.pre += ' else {\n'
      ret.pre += f.pre;
      ret.pre += pfx + INDENT + uid + '=' + f.main + ';\n';
      ret.pre += f.post;
      ret.pre += pfx + '}';
    }
    ret.pre += ';\n'
    return ret;
  }
  if(node.is('number')) {
    return new CodeChunk(node, node.data.toString());
  }
  if(node.is('string')) {
    // TODO: more escape: quotes, backslashes, etc
    var s = node.data.replace(/\r?\n/g, "\\n")
    return new CodeChunk(node, "'" + s + "'");
  }
  if(node.is('varname')) {
    // pfx not used - we are probably not a top level statement
    return new CodeChunk(node, "get_var('" + node.data + "')");
  }
  if(node.is('exec')) {
    // TODO: real word expansion
    var words_array = compile_tree(node[0], pfx);
    var ret = new CodeChunk(node).use(words_array);
    var uid = uniq_id('exec$');
    ret.pre += pfx + 'var ' + uid + ' = ' + words_array.main + ';\n';
    ret.main += pfx + 'get_var("exec")(' + uid + '[0], ' + uid + '.slice(1))';
    return ret;
  }
  if(node.is('array') || node.is('expressions')) {
    // TODO: decide about pfx. Are we expression or a top-level statement?
    var ret = new CodeChunk(node);
    var ret_elts = [];
    var have_splice = node.some(function(elt) { return elt.is('splice'); });

    if(have_splice) {
      var uid = uniq_id('splice$');
      ret.pre += pfx + 'var ' + uid + ' = []; // array accumulator\n';
    }

    node.forEach(function(e) {
      var e_tree = compile_tree(e, pfx);
      ret.use(e_tree);
      if(have_splice) {
	    if(e.is('splice')) {
          ret.pre += pfx + uid + ' = ' + uid + '.concat(' + e_tree.main + ');\n';
	    } else {
	      ret.pre += pfx + uid + '.push(' + e_tree.main + ');\n';
	    }
      } else {
	    ret_elts.push(e_tree.main);
      }
    });
    if(have_splice) {
      ret.main += uid;
    } else {
      ret.main += '[' + ret_elts.join(', ') + ']';
      if(node.is('expressions')) {
        ret.main = ret_elts.join(', ');
      }
    }
	// console.log('have_splice', have_splice, ret.toString());
    return ret;
  }
  if(node.is('top_level_expressions')) {
	var ret = new CodeChunk();
    node.forEach(function(e) {
      var e_tree = compile_tree(e, pfx + INDENT);
      ret.use(e_tree);
      ret.pre += pfx + e_tree.main + ';\n';
    });
	return ret;
  }
  if(node.is('splice')) {
    return compile_tree(node[0], pfx);
  }
  if(node.is('binop')) {
	// node.data -- operation name
    var e1 = compile_tree(node[0], pfx);
    var e2 = compile_tree(node[1], pfx);
    return new CodeChunk(node, node.data + '(' + e1.main + ', ' + e2.main + ')').use(e1).use(e2);
  }
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
  var js = compile_tree(tree, '');
  console.log(js.toString());

}

function compile(code) {
  var tree = parser.parse(code);
  // tree.setCallback('ngs_runtime_script_finish_callback');
  console.log('isSync', tree);
  var compiled = compile_tree(tree, '');
  return {'compiled_code': compiled.toString()};
}

exports.compile = compile;
