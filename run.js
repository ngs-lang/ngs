'use strict';

var _ = require('underscore');

var objects = require('./objects');
var storage = require('./storage').storage;
var compile = require('./compile').compile;
var jobs = require('./plugins/jobs');

// WARNING: global vars
var vars={}; // TODO: sync with storage


function NgsMethod() {

}

function NgsMethods() {

}
NgsMethods.prototype = Object.create(Array.prototype);

// TODO: security checks
function run(job) {

  console.log('RUN START');

  // TODO: lexical scope, etc.
  function set_var(name, val) {
    console.log('run.set_var', name, val);
    vars[name] = val;
    return val;
  }

  // TODO: exception if var not found
  function get_var(name) {
    var ret = vars[name];
    console.log('run.get_var', name, '->', ret);
    return ret
  }

  // Built-in functions, will be rewritten for multi-dispatch
  // TODO: function define_function(args_types, code)
  set_var('echo', function ngs_runtime_echo() {
	console.log('ECHO()', Array.prototype.slice.call(arguments));
  });

  set_var('exec', function ngs_runtime_exec(cmd, args, cb) {
    console.log('run.exec', cmd);
    var subJob = new objects.ExecJob(null, {
      'cmd': cmd,
      'args': args || [],
      'parent_id': job.id
    });
    subJob.start();
    return subJob;
  });

  function add(e1, e2) {
    // Fix JS arrays addition which makes no sense.
    if(_.isArray(e1) && _.isArray(e2)) {
      return e1.concat(e2);
    }
    // TODO: _.extend for two hashes
    // TODO: errors when adding (and other binops) incompatible types
    return e1 + e2;
  }

  // TODO: update state
  var source_code = compile(job.cmd).compiled_code;
  console.log('SOURCE', source_code);
  // TODO: update state
  // TODO: update job with compiled code for debug purposes
  eval(source_code);
  // TODO: update state
  console.log('RUN END');
}

exports.run = run;
