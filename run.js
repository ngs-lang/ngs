'use strict';

var BUFFER_SIZE = 1024*1024;

var fs = require('fs');
var util = require('util');

var _ = require('underscore');

var compile = require('./compile').compile;
var data = require('./vm-data');
var nm = require('./vm-native-methods');

for(var k in data) {
	global[k] = data[k];
}

var vm = require('./vm');
var v = new vm.VM();
var ctx = v.setupContext();

function load(fname) {
	var f = fs.readSync(fs.openSync(fname, 'r'), BUFFER_SIZE);
	var code = compile(f[0].toString()).compiled_code;
	code = [].concat([['comment', 'file: ' + fname]], code);
	v.useCode(code);
}

load('ngs/stdlib.ngs');
load(process.argv[2]);

v.start(function ngs_runtime_script_finish_callback() {
	if(process.env.NGS_DEBUG_FINISH) {
		console.log('finished_contexts', util.inspect(v.finished_contexts, {'depth': 10}));
		console.log('types', v.types);
	}
});
