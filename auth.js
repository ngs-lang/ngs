'use strict';

var fs = require('fs');
var storage = require('./storage').storage;

// TODO: warn or refuse to work when permissions are too open
var token_file_path = process.env['ZE_TOKEN_PATH'] || (process.env['HOME'] + '/.ngs/token');

var all_perms_token_id = fs.readFileSync(token_file_path).toString().trim();

// TODO: validation and/or escaping of the tokens
storage.set('/auth/tokens/' + all_perms_token_id, {
  id: all_perms_token_id,
  name: 'Root token from the token file',
  rules: [
    {
      method: '.',
      url: '.',
      action: 'allow'
    }
  ],
  created: null,
  expires: null
});


function deny(res, x) {
  res.status(403);
    res.send(x);
}

function can_access(token_data, method, url) {
  for(var i=0; i<token_data.rules.length; i++) {
    var rule = token_data.rules[i];
    if(!method.match(rule.method)) {
      continue;
    }
    if(!url.match(rule.url)) {
      continue;
    }
    if(rule.action === 'allow') {
      return true;
    }
    if(rule.action === 'deny') {
      return true;
    }
    throw RuntimeError("Rule action is neither allow or deny: " + JSON.stringify([i, token_data]));
  }
  return false;
}

function handleRequest(req, res) {
  if(req.url == '/auth/tokens/') {
    res.redirect(301, '/auth/tokens');
    return;
  }
  if(req.url == '/auth/tokens') {
    res.send({'result': storage.getByPrefix('/auth/tokens/')});
  }
  // TODO /auth/tokens/ID
  res.status(404);
  res.end();
}

function authenticationMiddleware(req, res, next) {
  console.log('AUTH FOR %s %s', req.method, req.url);
  var auth_token = req.get('X-ngs-auth');
  if(!auth_token) {
    deny(res, {'error_message': 'Missing X-ngs-auth header in request'});
    return;
  }
  var token_data = storage.get('/auth/tokens/' + auth_token);
  if(!token_data) {
    deny(res, {'error_message': 'Authentication token specified by X-ngs-auth not found', 'token': auth_token});
    return;
  }
  var ok = can_access(token_data, req.method, req.url);
  if(!ok) {
    deny(res, {'error_message': 'Authentication token specified by X-ngs-auth does not have permissions for the operation', 'token': auth_token, 'method': req.method, 'url': req.url});
    return;
  }
  // Request is allowed
  if(/^\/auth\//.test(req.url)) {
    handleRequest(req, res);
  } else {
    next();
  }
}

exports.authenticationMiddleware = authenticationMiddleware;
