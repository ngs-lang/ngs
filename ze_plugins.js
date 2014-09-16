var fs = require('fs');
var _ = require('underscore');
var child_process = require('child_process')
var execFile = child_process.execFile;
var spawn = child_process.spawn;


var _plugins = {

}

// http://www.perlmonks.org/bare/?node_id=323977
// /usr/include/linux/stat.h -- #define S_IXUSR 00100
var S_IXUSR = 00100;

function isFileExecutable(stats) {
	// http://stackoverflow.com/questions/16258578/how-do-i-check-if-a-file-is-executable-in-node-js
	// http://stackoverflow.com/questions/11775884/nodejs-file-permissions
	return stats.mode & S_IXUSR;
}


function scanPluginsDir(callback) {
	var dir = __dirname + '/plugins';
	fs.readdir(dir, function(err, files) {
		if(err) {
			console.log('fs.readdir()', dir, err);
			return;
		}
		files.forEach(function(file) {
			var bin = dir + '/' + file;
			fs.stat(bin, function(err, stats) {
				if(err) {
					console.log('fs.stat()', bin, err);
					return;
				}
				if(isFileExecutable(stats)) {
					execFile(bin, ['meta'], {env: process.env}, function(err, stdout, stderr) {
						var data = null;
						if(err) {
							console.log('execFile()', bin, err);
							return;
						}
						try {
							data = JSON.parse(stdout);
						} catch(e) {
							console.log('JSON.parse() error for', bin, e);

						}
						_plugins[bin] = {'meta': {'data': data, 'stdout': stdout, 'stderr': stderr}};
					});
				}
			});
		});
	});
}

function handleRequest(req, res, next) {

	console.log('ze_plugins.js:handleRequest() - URL', req.url);

	if(req.url == '/plugins') {
		res.send(_plugins);
		return;
	}

	var env = _.extend({}, process.env);
	_.extend(env, {

	});
	var args = [req.method, req.path];
	// spawn() probably
	for(bin in _plugins) {
		if(!Object.prototype.hasOwnProperty.call(_plugins, bin)) {
			continue;
		}
		var child = spawn(bin, args, {env: env});
		console.log('handleRequest', 'child', child.pid);
	};
}

scanPluginsDir();
// setInterval(scanPluginsDir, 1000);

var env = _.extend({}, process.env);

exports.handleRequest = handleRequest;
