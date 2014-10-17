'use strict';

var objects = require('./objects');
var storage = require('./storage').storage;
var compile = require('./compile').compile;
var jobs = require('./plugins/jobs');

// WARNING: global vars
var vars={}; // TODO: sync with storage

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
    console.log('run.get_var', name);
    return vars[name];
  }

  function exec(cmd, args) {
    console.log('run.exec', cmd);
    var subJob = new objects.ExecJob(null, {
      'cmd': cmd,
      'args': args || [],
      'parent_id': job.id
    });
    subJob.start();
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
