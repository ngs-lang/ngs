var fs = require('fs');

// pwgen 32 >/home/ilya/.ze/token

// TODO: warn or refuse to work w
var token_file_path = process.env['ZE_TOKEN_PATH'] || (process.env['HOME'] + '/.ze/token');

function Permissions() {
	this.initialize();
}

Permissions.prototype.initialize = function() {

}

function AllPermissions() {
}

AllPermissions.prototype.can = function(method, url) {
	return true;
};

// Cookie ID -> Permissions object
var _perms = {};

var all_perms_token_id = fs.readFileSync(token_file_path).toString().trim();
_perms[all_perms_token_id] = new AllPermissions();

function deny(res, x) {
	res.status(403);
  	res.send(x);
}

function authenticationMiddleware(req, res, next) {
  console.log('AUTH FOR %s %s', req.method, req.url);
  var auth_token = req.get('X-ze-auth');
  if(!auth_token) {
  	deny(res, {'error_message': 'Missing X-ze-auth header in request'});
	return;
  }
  if(!_perms[auth_token]) {
  	deny(res, {'error_message': 'Authentication token specified by X-ze-auth not found', 'token': auth_token});
	return;
  }
  var ok = _perms[auth_token].can(req.method, req.url);
  if(!ok) {
  	deny(res, {'error_message': 'Authentication token specified by X-ze-auth does not have permissions for the operation', 'token': auth_token, 'method': req.method, 'url': req.url});
	return;
  }
  next();
}

exports.authenticationMiddleware = authenticationMiddleware;
