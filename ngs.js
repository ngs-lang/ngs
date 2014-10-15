'use strict';

// NGS CLI client

// TODO: parametrize via environment variables (with sane defaults):
// * server endpoint
// * ca certificate file location

var fs = require('fs');
var https = require('https');
var readline = require('readline');
var querystring = require('querystring');

var token = require('./auth').getRootToken();

var https_options = {
  hostname: 'ngs',
  port: 8443,
  path: '/jobs',
  method: 'POST',
  ca: fs.readFileSync('server-cert.pem'),
  agent: false,
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
    'X-ngs-auth': token,
  }
}

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.setPrompt('NGS so-pre-alpha > ');

rl.on('close', function handle_close() {
  console.log('NSG CLI exit on close');
  process.exit(0);
});

rl.prompt();
rl.on('line', function handle_line(line) {
  line.trim();
  console.log('NGS CLI got line: ['+ line +']');
  process_command(line);
  rl.prompt();
});

// TODO: instead of HTTPS, use internal API which
// could use either HTTPS or direct internal API to run
// the command, depending in which mode the shell runs (client/server or combined)
function process_command(line) {
  console.log('process_command()', line);
  if(!line) {
    return;
  }
  var post_data = querystring.stringify({cmd: line});
  https_options.headers['Content-Length'] = post_data.length;

  var req = https.request(https_options, function(res, err) {
    // console.log(err);
    // console.log(res);
    res.setEncoding('utf8');
    res.on('data', function (chunk) {
      console.log('Response: ' + chunk);
    });
  });
  req.end(post_data);
}
