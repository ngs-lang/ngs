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

var start_rule = 'commands';

function load(fname, start_rule) {
	var f = fs.readSync(fs.openSync(fname, 'r'), BUFFER_SIZE);
	var code = compile(f[0].toString(), {'start_rule': start_rule}).compiled_code;
	code = [].concat([['comment', 'file: ' + fname]], code);
	v.useCode(code);
}

// load('ngs/stdlib.ngs', start_rule);
// load(process.argv[2], start_rule);

load('ngs/bootstrap.ngs');

v.start(function ngs_runtime_script_finish_callback() {
	if(process.env.NGS_DEBUG_FINISH) {
		console.log('finished_contexts', util.inspect(v.finished_contexts, {'depth': 10}));
	}
});
