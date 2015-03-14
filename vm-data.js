"use strict";

var _ = require('underscore');

var tty = require('tty');

var value_id = 1;

function NgsValue(type, data, meta) {
	if(!this) {
		return new NgsValue(type, data, meta);
	}
	this.id = value_id++;
	this.type = type;
	this.data = data;
	this.meta = meta || {};
}

NgsValue.prototype.toString = function() {
	return '<' + this.type + ' #' + this.id + ' ' + this.data + '>';
}

NgsValue.prototype.eq = function(other) {
	// Only for scalars
	return (this.type == other.type) && (this.data == other.data);
}

function assert_data(data) {
	if(!(data instanceof NgsValue)) {
		console.log('Got non-data', data);
		throw new Error('Got non-data: ' + Object.toString(data));
	}
}

function make_getter(type, getter_name, assertor, processor) {
	// Ideally we should check whether type is anchestor of data.type
	// For simplicity and speed we don't.
	var r = function(data) {
		assert_data(data);
		var ret = data.data;
		if(assertor) {
			if(!assertor(ret)) {
				console.log('Got data of unexpected type. Expected', type, 'got', data.type, ' (', data, ')');
				throw new Error('Got non-'+type+': ' + Object.toString(data));
			}
		}
		if(processor) {
			ret = processor(ret);
		}
		return ret;
	}
	exports['get_' + getter_name] = r;
}

function get_id(data) {
	assert_data(data);
	return data.id;
}

function get_type(data) {
	assert_data(data);
	return data.type;
}

function get_meta(data) {
	assert_data(data);
	return data.meta;
}

make_getter('Array',		'arr', _.isArray);
make_getter('Bool',			'boo', _.isBoolean);
make_getter('Code',			'cod', _.isArray);
make_getter('Hash',			'hsh', _.isObject);
make_getter('Lambda',		'lmb', _.isObject);
make_getter('NativeMethod', 'nm' , _.isFunction);
make_getter('Null',			'nul', _.isNull);
make_getter('Number',		'num', _.isNumber);
make_getter('Process',		'prc', _.isObject);
make_getter('Readline',		'rl' , _.isObject);
make_getter('Regexp',		'rgx', _.isRegExp);
make_getter('Scopes',		'scp', _.isArray);
make_getter('String',		'str', _.isString);
make_getter('Thread',		'thr', _.isObject);

make_getter('Stream', 'stm', _.isString, function(s) {
	if(s != 'stdin' && s != 'stdout' && s != 'stderr') {
		throw new Error('Currently supported streams are only stdin, stdout and stderr');
	}
	return process[s];
});

exports.get_id = get_id;
exports.get_type = get_type;
exports.get_meta = get_meta;
exports.NgsValue = NgsValue;
