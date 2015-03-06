"use strict";

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

function make_getter(type, getter_name, processor) {
	var r = function(data) {
		assert_data(data);
		if(data.type !== type) {
			console.log('Got data of unexpected type. Expected', type, 'got', data.type);
			throw new Error('Got non-'+type+': ' + Object.toString(data));
		}
		var ret = data.data;
		if(processor) {
			ret = processor(ret);
		}
		return ret;
	}
	exports['get_' + getter_name] = r;
}

function get_type(data) {
	assert_data(data);
	return data.type;
}

function get_meta(data) {
	assert_data(data);
	return data.meta;
}

make_getter('Array',		'arr');
make_getter('Bool',			'boo');
make_getter('Code',			'cod');
make_getter('Hash',			'hsh');
make_getter('Lambda',		'lmb');
make_getter('NativeMethod', 'nm');
make_getter('Null',			'nul');
make_getter('Number',		'num');
make_getter('Process',		'prc');
make_getter('Readline',		'rl');
make_getter('Scopes',		'scp');
make_getter('String',		'str');
make_getter('Thread',		'thr');

make_getter('Stream', 'stm', function(s) {
	if(s != 'stdin' && s != 'stdout' && s != 'stderr') {
		throw new Error('Currently supported streams are only stdin, stdout and stderr');
	}
	return process[s];
});

exports.get_type = get_type;
exports.get_meta = get_meta;
exports.NgsValue = NgsValue;
