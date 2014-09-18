// TODO: allow external plugins (simple scripts)
//       For simplicity, allow only JS plugins for now.

var fs = require('fs');
var _ = require('underscore');


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
				if(/\.js$/.exec(file)) {
					_plugins[file] = require(dir + '/' + file);
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

	var ctx = {
		done: false,
	}
	// spawn() probably
	for(bin in _plugins) {
		_plugins[bin].handleRequest(req, res, ctx);
	};

	if(!ctx.done) {
		res.status(404);
		res.end();
	}
}

scanPluginsDir();
// setInterval(scanPluginsDir, 1000);

var env = _.extend({}, process.env);

exports.handleRequest = handleRequest;
