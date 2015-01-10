'use strict';

// TODO: parametrize via environment variables (with sane defaults):
// * server endpoint
// * ca certificate file location

var _ = require('underscore');
var events = require('events');
var fs = require('fs');
var https = require('https');
var querystring = require('querystring');

var token = require('./auth').getRootToken();
var ca = fs.readFileSync('server-cert.pem');

// TODO: big responses will be line-oriented JSONs - handle them better than just
//		 accumulating the whole response.

function Client(options) {
	var o = options || {};
	this.https_options = {
		hostname: 'ngs',
		port: 8443,
		path: '/jobs',
		method: 'POST',
		ca: ca,
		agent: false,
		headers: {
			'Content-Type': 'application/x-www-form-urlencoded',
			'X-ngs-auth': token,
		}
	}
	_.extend(this.https_options, o.transport || {});
}

Client.prototype = new events.EventEmitter();



Client.prototype.send_command = function send_command(line) {
	// console.log('process_command()', line);
	if(!line) {
		return;
	}
	var https_options = this.https_options;
	var post_data = querystring.stringify({cmd: line});
	https_options.headers['Content-Length'] = post_data.length;

	var client = this;

	var response_data = '';
	var req = https.request(https_options, function(res, err) {
		// console.log(err);
		// console.log(res);
		res.setEncoding('utf8');
		res.on('data', function handle_response_chunk(chunk) {
			response_data += chunk;
		});
		res.on('end', function handle_response_end() {
			if(res.statusCode === 201) {
				// console.log();
				var response = JSON.parse(response_data);
				// console.log(('JOB CREATED: ' + response['/vars/LAST_JOB_ID']).green);
				client.emit('job-created', response.id);
			} else {
				console.error('[client.js] JOB CREATION FAILED', res.statusCode, res.headers, response_data);
				client.emit('job-creation-failed', res);
			}
		});
	});
	req.end(post_data);
}

exports.Client = Client;
