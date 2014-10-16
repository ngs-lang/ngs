'use strict';

// NGS CLI client

// TODO:
// * if invoked with an argument(s) - execute that command and exit
// * make sure pipe in/out work (for non-interactive use)
//   * no command specified - stdin is the script to execute
//   * command specified - pipe to/from command

var colors = require('colors');
var readline = require('readline');

var Client = require('./client').Client;
var client = new Client();

function console_async_out() {
  // Async in relation to readline. Will probably mess with it.
  readline.clearLine(process.stdin, 0); // clear whole line
  readline.cursorTo(process.stdin, 0);  // cursor to beginning of line
  console.log.apply(null, arguments);
  rl.prompt();
}

client.on('job-created', function job_created_handler(id) {
  console_async_out(('JOB CREATED: ' + id).green);
});

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.setPrompt('NGS so-pre-alpha > ');

rl.on('close', function handle_close() {
  console.log('NSG CLI exit on close');
  process.exit(0);
});

rl.prompt();
rl.on('line', function handle_line(line) {
  line.trim();
  // console.log('NGS CLI got line: ['+ line +']');
  client.send_command(line);
  rl.prompt();
});
