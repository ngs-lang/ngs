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

// simple logger
app.use(function(req, res, next){
  console.log('%s %s', req.method, req.url);
  next();
});

// TODO: auth
app.use(auth.authenticationMiddleware);

app.get('/', function(req, res){
  res.send('hello world');
});


app.route('*').all(plugins.handleRequest);
