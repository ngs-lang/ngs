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


var INDENT = '\t';
function compile(node, pfx) {
  if(node['type'] == 'assignment') {
    if(node['lhs']['type'] == 'var') {
      var rhs = compile(node['rhs'], pfx);
      // console.log('XXX', rhs, rhs['main']);
      return new CodeChunk(pfx+"set_var('" + node['lhs']['name'] + "', " + rhs.main + ");\n", rhs.pre, rhs.post);
    }
    throw new Error("Assignment to type " + lhs['type'] + " is not implemented");
  }
  if(node['type'] == 'commands') {
    var cs = node['commands'];
    return new CodeChunk(cs.map(function(c) {return compile(c, pfx);}).join('\n'));
  }
  if(node['type'] == 'if') {
    var f = null;
    var c = compile(node['cond'], pfx);
    var t = compile(node['true'], pfx + INDENT);
    if(node['false']) {
      f = compile(node['false'], pfx + INDENT);
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
  if(node['type'] == 'var') {
    return new CodeChunk(pfx + "ngs_get_var('" + node['name'] + "')");
  }
  if(node['type'] == 'exec') {
    console.log('WORDS', node['words']);
    return new CodeChunk(pfx + "ngs_exec('" + node['words'] + "')");
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
  var js = compile(tree, '');
  console.log(js.toString());

}
