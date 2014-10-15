// Setup:
//   apt-get install node-express node-body-parser node-finished
//   mkdir ~/.ngs
//   pwgen 32 >~/.ngs/token
//   # In directory above the checked out "ngs" directory:
//   openssl genrsa -out server-key.pem 4096
//   openssl req -new -key server-key.pem -out server-csr.pem
//   openssl x509 -req -in server-csr.pem -signkey server-key.pem -out server-cert.pem
//   # Optional: add "127.0.0.1 ngs" line to /etc/hosts

// Run server:
//   nodejs ngs/server.js

// Usage example 1:
//   curl -H "X-ngs-auth: $(cat ~/.ngs/token)" --cacert server-cert.pem https://ngs:8443/auth/tokens | jq .

// Usage example 2:
//   curl -i -H "X-ngs-auth: $(cat ~/.ngs/token)" --cacert server-cert.pem -d 'cmd=ls' https://ngs:8443/jobs
//   curl -i -H "X-ngs-auth: $(cat ~/.ngs/token)" --cacert server-cert.pem https://ngs:8443/all | jq .

var https = require('https');
var fs = require('fs');

var express = require('express');
var bodyParser = require('body-parser');

var auth = require('./auth');
var objects = require('./objects');
var plugins = require('./plugins');
var storage = require('./storage').storage;

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

app.use(function tiny_logger_middleware(req, res, next){
  console.log('%s %s', req.method, req.url);
  next();
});

app.use(auth.authenticationMiddleware);

app.get('/', function slash_request_handler(req, res){
  // TODO: list of all URL patterns
  res.status(501);
  res.send({'error_message': 'Listing of all URL patterns is not implemented yet'});
});

app.get('/ALL', function slash_all_request_handler(req, res){
  // For debug
  res.send(storage._data);
});


app.route('*').all(plugins.handleRequest);
