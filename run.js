'use strict';
var storage = require('./storage').storage;
var compile = require('./compile').compile;
var jobs = require('./plugins/jobs');

// TODO: security checks
function run(job) {

  console.log('RUN START');
  var vars={};

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
    jobs.startJob(cmd, args, {'mode': 'external', 'parent_id': job.id});
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
