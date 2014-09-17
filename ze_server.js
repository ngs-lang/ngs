var https = require('https');
var fs = require('fs');

var express = require('express');

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
