var STORAGE_LAST_JOB_ID_PATH = '/vars/LAST_JOB_ID';
var STORAGE_CWD_PATH = '/vars/CWD';
var URL_SEE_OUTPUT = new RegExp('^/jobs/\\d+/std(out|err)$');
// var STORAGE_ENV_PATH = '/vars/ZE_ENV';

var storage = require('../storage').storage;
var child_process = require('child_process');

function do_job(job, storage_path_known) {
  // TODO: Saner: return "Created" after spawn(), return failure if can't spawn()
  // TODO: Allow input feeding via stdin
  var job_storage_path = '/jobs/' + job.id;
  var job_status_storage_path = job_storage_path + '/status';
  storage.set(job_storage_path, job);
  storage.set(job_status_storage_path, {state: 'starting'});
  storage_path_known(job_storage_path);
  // TODO: something more sensible regarding environment
  // http://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options
  console.log(job.cmd, job.args,storage.get(STORAGE_CWD_PATH) || process.cwd());
  var p = child_process.spawn(job.cmd, job.args, {
    cwd: storage.get(STORAGE_CWD_PATH) || process.cwd()
  });
  // TODO: maybe store output with it's timestamp?
  (['stdout', 'stderr']).forEach(function(what) {
    var path = job_storage_path + '/' + what
    storage.set(path, new Buffer(0));
    p[what].on('data', function capture_process_output(data) {
      console.log('DATA', what, data, path);
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

function handleRequest(req, res, ctx) {
  if(req.method === 'POST' && req.url === '/jobs') {
    var last_job_id = storage.get(STORAGE_LAST_JOB_ID_PATH) || 0;
    var new_job_id = last_job_id + 1;
    var job = {
      'id': new_job_id,
      'cmd': req.param('cmd'),
      'args': req.param('args', []),
    }
    console.log('JOB', job, req.body);
    storage.set(STORAGE_LAST_JOB_ID_PATH, new_job_id);
    do_job(job, function storage_path_known(path) {
      var ret = {};
      ret[STORAGE_LAST_JOB_ID_PATH] = new_job_id;
      res.status(201); // Created
      res.set('Location', path);
      res.send(ret);
      ctx.done = true;
      return;
    });
  }
  // WIP: 
  if(req.method === 'GET' && URL_SEE_OUTPUT.exec(req.url)) {
    res.send({result: storage.get(req.url).toString()}); // Buffer. Without .toString, empty buffer is returned as "<Buffer >".
    ctx.done = true;
    return;
  }
}

exports.handleRequest = handleRequest;
