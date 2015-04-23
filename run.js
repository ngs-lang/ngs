'use strict';

var BUFFER_SIZE = 1024*1024;

var fs = require('fs');
var path = require('path');
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

var NGS_FOLDER = path.dirname(path.normalize(process.argv[1]));
ctx.frames[0].scopes[0]['NGS_FOLDER'] = nm.to_ngs_object(NGS_FOLDER);
ctx.frames[0].scopes[0]['ENV'] = nm.to_ngs_object(process.env);

function load(fname, start_rule) {
	var f = fs.readSync(fs.openSync(fname, 'r'), BUFFER_SIZE);
	var code = compile(f[0].toString(), fname, {'start_rule': start_rule}).compiled_code;
	v.useCode(code);
}

load(path.join(NGS_FOLDER, 'bootstrap.ngs'));

v.start(function ngs_runtime_script_finish_callback() {
	if(process.env.NGS_DEBUG_FINISH) {
		console.log('finished_contexts', util.inspect(v.finished_contexts, {'depth': 10}));
	}
	if(process.env.NGS_PROFILE) {
		console.log('OPCODES', v.opcodes_stats);
		console.log('MATCH_PARAMS', v.match_params_stats);
	}
});
