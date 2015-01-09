"use strict";

function make_getter(type, getter_name) {
  var r = function(data) {
	if(Object.prototype.toString.call(data) !== '[object Array]') {
	  console.log('Got non-data', data);
	  throw new Error('Got non-data: ' + Object.toString(data));
	}
	if(data[0] !== type) {
	  console.log('Got data of unexpcted type. Expected', type, 'got', data[0]);
	  throw new Error('Got non-'+type+': ' + Object.toString(data));
	}
	return data[1];
  } 
  exports['get_' + getter_name] = r;
}

function get_type(data) {
  if(Object.prototype.toString.call(data) !== '[object Array]') {
	console.log('Got non-data', data);
	throw new Error('Got non-data: ' + Object.toString(data));
  }
  return data[0];
}

make_getter('Array',        'arr');
make_getter('Bool',         'boo');
make_getter('Hash',         'hsh');
make_getter('Lambda',       'lmb');
make_getter('NativeMethod', 'nm');
make_getter('Number',       'num');
make_getter('Scopes',       'scp');
make_getter('String',       'str');

exports.get_type = get_type;
