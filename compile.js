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

function CodeChunk(main, pre, post) {
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
  if(node['type'] == 'assignment') {
    if(node['lhs']['type'] == 'var') {
      var rhs = compile_tree(node['rhs'], pfx);
      // console.log('XXX', rhs, rhs['main']);
      return new CodeChunk(pfx+"set_var('" + node['lhs']['name'] + "', " + rhs.main + ");\n", rhs.pre, rhs.post);
    }
    throw new Error("Assignment to type " + lhs['type'] + " is not implemented");
  }
  if(node['type'] == 'commands') {
    var cs = node['commands'];
    return new CodeChunk(cs.map(function(c) {return compile_tree(c, pfx);}).join('\n'));
  }
  if(node['type'] == 'if') {
    var f = null;
    var c = compile_tree(node['cond'], pfx);
    var t = compile_tree(node['true'], pfx + INDENT);
    if(node['false']) {
      f = compile_tree(node['false'], pfx + INDENT);
    }
    var uid = uniq_id('tmp_if$');
    var ret = new CodeChunk(uid);
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
  if(node['type'] == 'number') {
    return new CodeChunk(node['val'].toString());
  }
  if(node['type'] == 'string') {
    // TODO: more escape: quotes, backslashes, etc
    var s = node['val'].replace(/\r?\n/g, "\\n")
    return new CodeChunk("'" + s + "'");
  }
  if(node['type'] == 'var') {
    // pfx not used - we are probably not a top level statement
    return new CodeChunk("get_var('" + node['name'] + "')");
  }
  if(node['type'] == 'exec') {
    // TODO: real word expansion
    var w = node['words'];
    // console.log('WORDS', w);
    var words_array = compile_tree(w, pfx);
    var ret = new CodeChunk();
    ret.pre = words_array.pre;
    ret.post = words_array.post;
    var uid = uniq_id('cmd$');
    ret.main = pfx + 'var ' + uid + ' = ' + words_array.main + ';\n';
    ret.main += pfx + 'exec(' + uid + '[0], ' + uid + '.slice(1));\n';
    return ret;
  }
  if(node['type'] == 'array') {
    // TODO: decide about pfx. Are we expression or a top-level statement?
    var elements = node['elements'];
    // console.log('ARRAY', elements);
    var ret = new CodeChunk();
    var ret_elts = [];
    var have_splice = elements.some(function(elt) { return elt['type'] == 'splice'; });

    if(have_splice) {
      var uid = uniq_id('splice$');
      ret.pre += pfx + 'var ' + uid + ' = []; // array accumulator\n';
    }

    elements.forEach(function(e) {
      var e_tree = compile_tree(e, pfx);
      ret.use(e_tree);
      if(have_splice) {
	if(e['type'] == 'splice') {
	  ret.pre += pfx + uid + ' = ' + uid + '.concat(' + e_tree.main + ');\n';
	} else {
	  ret.pre += pfx + uid + '.push(' + e_tree.main + ');\n';
	}
      } else {
	ret_elts.push(e_tree.main);
      }
    });
    if(have_splice) {
      ret.main = uid;
    } else {
      ret.main = '[' + ret_elts.join(', ') + ']';
    }
    return ret;
  }
  if(node['type'] == 'splice') {
    return compile_tree(node['expression'], pfx);
  }
  if(node['type'] == 'binop') {
    var e1 = compile_tree(node['e1'], pfx);
    var e2 = compile_tree(node['e2'], pfx);
    return new CodeChunk(node['op'] + '(' + e1.main + ', ' + e2.main + ')').use(e1).use(e2);
  }
  if(node['type']) {
    throw "Don't know how to compile type '" + node['type'] + "'";
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
  var compiled = compile_tree(tree, '');
  return {'compiled_code': compiled.toString()};
}

exports.compile = compile;
