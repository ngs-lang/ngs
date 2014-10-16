var URL_SEE_OUTPUT = new RegExp('^/jobs/\\d+/std(out|err)$');
// var STORAGE_ENV_PATH = '/vars/ZE_ENV';

var _ = require('underscore');

var objects = require('../objects');
var storage = require('../storage').storage;

function start_external(job, job_storage_path, job_status_storage_path) {
  // TODO: something more sensible regarding environment
  // http://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options

}

function handleRequest(req, res, ctx) {
  if(req.method === 'POST' && req.url === '/jobs') {
    var job = new objects.ScriptJob(null, {
      'cmd':req.param('cmd'),
      'args': req.param('args', [])
    });
    job.start();
    res.status(201); // Created
    res.set('Location', job.getStoragePath());
    res.send(job.serialize());
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

exports.handleRequest = handleRequest;
