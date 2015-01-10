var objects = require('./objects');

var BUFFER_SIZE = 1024*1024;
var fs = require('fs');
var f = fs.readSync(process.stdin.fd, BUFFER_SIZE);
// console.log(f);
var job = new objects.ScriptJob(null, {
	'cmd': f[0].toString(),
	'args': []
});
job.start();

