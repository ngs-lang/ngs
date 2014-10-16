'use strict';

var child_process = require('child_process');

var _ = require('underscore');

var storage = require('./storage').storage;

var run = require('./run').run;


var jobs = {};

var STORAGE_CWD_PATH = '/vars/CWD';
var STORAGE_LAST_JOB_ID_PATH = '/vars/LAST_JOB_ID';

function get_next_job_id() {
  var last_job_id = storage.get(STORAGE_LAST_JOB_ID_PATH) || 0;
  var ret = last_job_id + 1;
  storage.set(STORAGE_LAST_JOB_ID_PATH, ret);
  return ret;
}

function Job(id, job_info) {
  this.initialize(id, job_info);
}

Job.getById = function(id) {
  // console.log('getById', id);
  if(!id) {
    return null;
  }
  // console.log(jobs[id] ? 'Y': 'N');
  return jobs[id] || null;
}

Job.prototype.initialize = function(id, job_info) {
  // console.log('Job.initialize', arguments, _.keys(jobs));
  if(id) {
    _.extend(this, storage.get(this.getStoragePath()));
  } else {
    this.id = get_next_job_id();
    this.parent_id = null;
    _.extend(this, job_info || {});
    jobs[this.id] = this;
    this.setState('starting');
  }
}

Job.prototype.getStoragePath = function() {
  return '/jobs/' + this.id;
}

Job.prototype.appendOutput = function(output_channel_name, data) {
  // TODO: implement and use Storage.append() or Storage.appendBuffer() or alike
  var path = this.getStoragePath() + '/' + output_channel_name;
  storage.set(path, storage.get(path) + data);
  var parentJob = this.getParentJob();
  if(parentJob) {
    // console.log('parentJob', parentJob.serialize());
    parentJob.appendOutput(output_channel_name, data);
  }
}


Job.prototype.setState = function(s) {
  this.state = s;
}

Job.prototype.getParentJob = function() {
  return Job.getById(this.parent_id);
}

// serialize is not a very appropriate name. maybe toSimpleStruct() ?
Job.prototype.serialize = function() {
  var o = {};
  var job = this;
  ['id', 'cmd', 'args', 'parent_id', 'type', 'state'].forEach(function(k) {
    o[k] = job[k];
  });
  return o;
}

Job.prototype.save = function() {
  storage.set(this.getStoragePath(), this.serialize());
}

function ScriptJob() {
  var ret = Job.apply(this, arguments);
  return ret;
}

ScriptJob.prototype = Object.create(Job.prototype);

ScriptJob.prototype.type = 'script';

ScriptJob.prototype.start = function(){
  // console.log(this.serialize());

  // TODO - dedup - start
  var job = this;
  (['stdout', 'stderr']).forEach(function(output_channel_name) {
    var path = job.getStoragePath() + '/' + output_channel_name;
    storage.set(path, new Buffer(0));
  });
  // TODO - dedup - end
  setTimeout(run.bind(null, this), 0);
}

function ExecJob() {
  var ret = Job.apply(this, arguments);
  return ret;
}

ExecJob.prototype = Object.create(Job.prototype);

ExecJob.prototype.type = 'exec';

ExecJob.prototype.start = function(){
  var job = this;
  console.log('start_external()', job.serialize());
  // TODO: process working directory should be
  //       inherited from parent process.
  var p = child_process.spawn(job.cmd, job.args, {
    cwd: storage.get(STORAGE_CWD_PATH) || process.cwd()
  });
  // TODO: maybe store output with it's timestamp?
  (['stdout', 'stderr']).forEach(function(output_channel_name) {
    var path = job.getStoragePath() + '/' + output_channel_name;
    storage.set(path, new Buffer(0));
    p[output_channel_name].on('data', function capture_process_output(data) {
      // console.log('DATA', output_channel_name, data, path);
      job.appendOutput(output_channel_name, data);
    });
  });
  p.on('error', function(e) {
    job.setState('error');
    // TODO // storage.setProperty(job_status_storage_path, 'error', e.toString());
  });
  p.on('exit', function(exit_code, _signal) {
    job.setState('done');
    // TODO // storage.setProperty(job_status_storage_path, 'exit_code', exit_code);
  });
}


exports.Job = Job;
exports.ScriptJob = ScriptJob;
exports.ExecJob = ExecJob;
