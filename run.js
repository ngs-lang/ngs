'use strict';

var util = require('util');

var _ = require('underscore');

var objects = require('./objects');
var storage = require('./storage').storage;
var compile = require('./compile').compile;
var jobs = require('./plugins/jobs');

// TODO: security checks
function run(job) {

  // console.log('RUN START');


  function ngs_runtime_script_finish_callback() {

  }

  // TODO: update state
  var code = compile(job.cmd).compiled_code;
  // TODO: update state
  // TODO: update job with compiled code for debug purposes
  var vm = require('./vm');
  var v = new vm.VM();
  var ctx = v.setupContext();

  ctx.registerMethod('exec', function ngs_runtime_exec(args) {
    var subJob = new objects.ExecJob(null, {
      'cmd': args[0],
      'args': args.slice(1),
      'parent_id': job.id
    });
    subJob.start(function ngs_runtime_exec_finish_callback(e, exit_code, signal) {
	  // ctx.stack.push([e, exit_code, signal]);
	  v.unsuspend_context(this);
	}.bind(this));
	v.suspend_context();
	return subJob;
  });

  v.useCode(code);
  console.log(code);
  v.start(function ngs_runtime_script_finish_callback() {
	console.log('finished_contexts', util.inspect(v.finished_contexts, {'depth': 10}));
  });
  // TODO: update state
  console.log('RUN END');
}

exports.run = run;
