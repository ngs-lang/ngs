var STORAGE_LAST_JOB_ID_PATH = '/vars/LAST_JOB_ID';
var STORAGE_CWD_PATH = '/vars/CWD';
var URL_SEE_OUTPUT = new RegExp('^/jobs/\\d+/std(out|err)$');
// var STORAGE_ENV_PATH = '/vars/ZE_ENV';

var _ = require('underscore');

var child_process = require('child_process');
var run = require('../run').run;
var storage = require('../storage').storage;

function job_storage_path_by_id(id) {
  return '/jobs/' + id;
}

function start_script(job, job_storage_path, job_status_storage_path) {
  console.log('start_script()', job);
  setTimeout(run.bind(null, job), 0);
}

function start_external(job, job_storage_path, job_status_storage_path) {
  // TODO: something more sensible regarding environment
  // http://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options
  console.log('start_external()', job);
  var p = child_process.spawn(job.cmd, job.args, {
    cwd: storage.get(STORAGE_CWD_PATH) || process.cwd()
  });
  // TODO: maybe store output with it's timestamp?
  (['stdout', 'stderr']).forEach(function(what) {
    var path = job_storage_path + '/' + what
    storage.set(path, new Buffer(0));
    p[what].on('data', function capture_process_output(data) {
      // console.log('DATA', what, data, path);
      // TODO: implement and use Storage.append() or Storage.appendBuffer() or alike
      storage.set(path, storage.get(path) + data);
    });
  });
  p.on('error', function(e) {
    storage.setProperty(job_status_storage_path, 'state', 'error');
    storage.setProperty(job_status_storage_path, 'error', e.toString());
  });
  p.on('exit', function(exit_code, _signal) {
    storage.setProperty(job_status_storage_path, 'state', 'done');
    storage.setProperty(job_status_storage_path, 'exit_code', exit_code);
  });

}

var job_starter = {
  'script': start_script,
  'external': start_external,
}

function start_job(cmd, args, options) {
  // TODO: Saner: return "Created" after spawn(), return failure if can't spawn()
  // TODO: Allow input feeding via stdin
  var o = {
    'mode': 'script'
  };
  _.extend(o, options || {});
  var last_job_id = storage.get(STORAGE_LAST_JOB_ID_PATH) || 0;
  var new_job_id = last_job_id + 1;
  storage.set(STORAGE_LAST_JOB_ID_PATH, new_job_id);

  var job = {
    'id': new_job_id,
    'cmd': cmd,
    'args': args,
    'options': o,
  }

  var job_storage_path = job_storage_path_by_id(job.id);
  var job_status_storage_path = job_storage_path + '/status';
  storage.set(job_storage_path, job);
  storage.set(job_status_storage_path, {state: 'starting'});

  job_starter[o.mode](job, job_storage_path, job_status_storage_path);

  return {'id': new_job_id, 'storage_path': job_storage_path_by_id(new_job_id)};
}

function handleRequest(req, res, ctx) {
  if(req.method === 'POST' && req.url === '/jobs') {
    var job_info = start_job(req.param('cmd'), req.param('args', []));
    var ret = {};
    ret[STORAGE_LAST_JOB_ID_PATH] = job_info['id'];
    res.status(201); // Created
    res.set('Location', job_info['storage_path']);
    res.send(ret);
    ctx.done = true;
    return;
  }
  // WIP: 
  if(req.method === 'GET' && URL_SEE_OUTPUT.exec(req.url)) {
    res.send({result: storage.get(req.url).toString()}); // Buffer. Without .toString, empty buffer is returned as "<Buffer >".
    ctx.done = true;
    return;
  }
}

exports.startJob = start_job;
exports.handleRequest = handleRequest;
