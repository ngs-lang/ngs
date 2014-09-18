// Setup:
//   mkdir ~/.ze
//   pwgen 32 >~/.ze/token
//   openssl x509 -req -in server-csr.pem -signkey server-key.pem -out server-cert.pem

// Run server:
//   nodejs ze/ze_server.js

// Usage example 1:
//   curl -H "X-ze-auth: $(cat ~/.ze/token)" --cacert server-cert.pem https://ze:8443/auth/tokens/ | jq .

// Usage example 2:
//   curl -i -H "X-ze-auth: $(cat ~/.ze/token)" --cacert server-cert.pem -d 'cmd=ls' https://ze:8443/jobs
//   curl -i -H "X-ze-auth: $(cat ~/.ze/token)" --cacert server-cert.pem https://ze:8443/jobs/1/stdout

var https = require('https');
var fs = require('fs');

var express = require('express');
var bodyParser = require('body-parser');

var objects = require('./ze_objects');
var plugins = require('./ze_plugins');
var auth = require('./ze_auth');

var jobs = [];


// Express app setup
var app = express();
var options = {
  key: fs.readFileSync('server-key.pem'),
  cert: fs.readFileSync('server-cert.pem')
};

https.createServer(options, app).listen(process.env['ZE_PORT'] || 8443);
console.log('STARTED');

// simple logger
app.use(bodyParser());

app.use(function(req, res, next){
  console.log('%s %s', req.method, req.url);
  next();
});

app.use(auth.authenticationMiddleware);

app.get('/', function(req, res){
  // TODO: list of all URL patterns
  res.status(501);
  res.send({'error_message': 'Listing of all URL patterns is not implemented yet'});
});


app.route('*').all(plugins.handleRequest);
