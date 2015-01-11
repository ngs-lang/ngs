var objects = require('./objects');

var BUFFER_SIZE = 1024*1024;
var fs = require('fs');
var f = fs.readSync(fs.openSync(process.argv[2], 'r'), BUFFER_SIZE);
var job = new objects.ScriptJob(null, {
	'cmd': f[0].toString(),
	'args': []
});
job.start();

