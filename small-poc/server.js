const https = require('https');
const fs = require('fs');
const node_static = require('node-static');
const fileServer = new node_static.Server('./www');

const PR_TIME = 8000; // seconds * 1000

const options = {
	key: fs.readFileSync('ssl/mysitename.key'),
	cert: fs.readFileSync('ssl/mysitename.crt')
};

const server = https.createServer(options, function (req, res) {
	fileServer.serve(req, res);
});

const io_server = require('socket.io').listen(server);

server.listen(8443);

const commands = {
	'pr': function (cmd, socket) {
		socket.emit('progress', {id: cmd.id, obj: {text: 'working'}});
		for (let i = 0; i < 20; i++) {
			setTimeout(function () {
				socket.emit('progress', {id: cmd.id, obj: {text: 'working', pct: i * 5}});
			}, i * PR_TIME / 20);
		}
		setTimeout(function () {
			socket.emit('progress', {id: cmd.id, obj: {text: 'done', pct: 100}});
		}, PR_TIME);
	},
	'fail': function (cmd, socket) {
		socket.emit('progress', {id: cmd.id, obj: {text: 'working'}});
		setTimeout(function () {
			socket.emit('progress', {id: cmd.id, obj: {text: 'error'}});
		}, 1000);
	},
	'sleep': function (cmd, socket) {
		socket.emit('progress', {id: cmd.id, obj: {text: 'working'}});
		setTimeout(function () {
			socket.emit('progress', {id: cmd.id, obj: {text: 'done'}});
		}, 5000);
	},
	'ls': function (cmd, socket) {
		socket.emit('progress', {id: cmd.id, obj: {text: 'working'}});
		let path = '.';
		if (cmd.args && cmd.args[0]) {
			path = cmd.args[0];
		}
		fs.readdir(path, function (err, files) {
			if (err) {
				socket.emit('error', {id: cmd.id, text: err.toString()});
				socket.emit('progress', {id: cmd.id, obj: {text: 'error'}});
				return;
			}
			let c = files.length;
			files.forEach(function (file, i) {
				fs.stat(path + '/' + file, function (err, stats) {
					if (err) {
						socket.emit('error', {id: cmd.id, text: 'Could not stat() ' + file});
						socket.emit('progress', {id: cmd.id, obj: {text: 'error'}});
						return;
					}
					let t = 'file';
					if (stats.isDirectory()) {
						t = 'directory';
					}
					socket.emit('output', {id: cmd.id, obj: {'type': t, 'text': file, 'path': path, 'stat': stats}});
					c--;
					if (c === 0) {
						socket.emit('progress', {id: cmd.id, obj: {text: 'done'}});
					}
				});
			});
		});
	}
};

io_server.on('connection', function (socket) {
	console.log('Socket.IO connection', socket.handshake.address);
	if (!['127.0.0.1', '::ffff:127.0.0.1'].includes(socket.handshake.address)) {
		console.error(`Closing connection from ${socket.handshake.address}`);
		socket.disconnect();
		return;
	}
	socket.on('command', function (cmd) {
		console.log('command!', cmd);
		if (!commands[cmd.cmd]) {
			socket.emit('progress', {id: cmd.id, obj: {text: 'error'}});
			return;
		}
		commands[cmd.cmd](cmd, socket);
	});
});
