$(function() {
  "use strict";

	console.log('start');

	$('#command-input').focus();

  var issued_commands = {};

  function Command(cmd, args) {
    this.cmd = cmd;
    this.args = args || [];
    this.generateId();
  }

  Command.prototype.generateId = function() {
    this.id = Math.random().toString(36).substring(7);
  }

  Command.prototype.show = function() {
		var c = $('<div class="cmd cmd-'+this.id+'" title="Command '+this.id+'"></div>');
		c.text(this.toString());
    this.cmdElt = c;

    var progressElt = $('<div class="progress-indicator">?</div>');
    this.progressElt = progressElt;
    progressElt.prependTo(c);

    this.pctElt = null;

    c.data('command', this);
    $('#output').append(c);

    var o = $('<div class="out out-'+this.id+'" title="Command '+this.id+' output"></div>');
    o.data('command', this);
    $('#output').append(o);
	}

  Command.prototype.updateProgress = function(progressObj) {
    this.progressElt.attr('title', progressObj.text);
    var iconsByStatus = {
      'working': 'spinner',
      'done': 'thumbs-up',
      'error': 'thumbs-down'
    }
    var e = $('<i class="icon-' + iconsByStatus[progressObj.text] + '"></i>');
    this.progressElt.empty().append(e);

    if(progressObj.hasOwnProperty('pct')) {
      // TODO: change title to "X% - CMD"
      if(!this.pctElt) {
        this.pctElt = $('<div class="pct"><div class="pct-inner"></div></div>');
        this.pctElt.insertAfter(this.cmdElt);
      }
      this.pctElt.find('div').css('width', progressObj.pct + '%');
      this.pctElt.attr('title', progressObj.pct + '%');
    }

    if(progressObj.text != 'working' && this.pctElt) {
      this.pctElt.addClass('done');
    }
  }

  Command.prototype.run = function() {
    issued_commands[this.id] = this;
    socket_io.emit('command', {id: this.id, cmd: this.cmd, args: this.args});
  }

  Command.prototype.toString = function() {
    var t = this.cmd;
    if(this.args) {
      t = t + ' ' + this.args.join(' ');
    }
    return t;
  }

  Command.getById = function(id) {
    return issued_commands[id];
  }

  var Objects = {
    'file': {
      'repr': function(obj) {
        var e = $('<div><i class="icon-file"></i> <span></span></div>');
        e.find('span').text(obj.text);
        return e;
      }
    },
    'directory': {
      'repr': function(obj) {
        var c = new Command('ls', [obj.path + '/' + obj.text]);
        console.log('repr');
        var e = $('<div><i class="icon-folder-close"></i> <a href="#dir"></a></div>');
        e.attr('title', c.toString());
        e.find('a').text(obj.text);
        e.find('a').click(function(ev) {
          ev.preventDefault();
          ev.stopPropagation();

          c.generateId();
          c.show();
          c.run();
        });
        console.log(e);
        return e;
      }
    }
  };

	$('#command-input').keyup(function(ev) {
		// console.log(Object.getOwnPropertyNames(ev));
		if(ev.keyCode == 13) {
			var cmd_text = $(this).val();
      if(cmd_text === '') { return; }
			$(this).val('');
      var c = new Command(cmd_text);
      c.show();
			c.run();
		}
	});
  var socket_io = io.connect('', {
    transports: ['xhr-polling'],
    'sync disconnect on unload': true
  });
  socket_io.on('error', function(err) {
    console.log(err);
    var e = $('<div class="out-err"></div>');
    e.text(err.text);
    $('.out-' + err.id).append(e);
  });
  socket_io.on('output', function(out) {
    var dst = $('.out-' + out.id);
    var e = $('<div class="out-out"></div>');
    if(!Objects[out.obj.type]) {
      dst.append($('<div class="out-cant-repr">[object of type '+out.obj.type+']</div>'));
      return;
    }
    e.append(Objects[out.obj.type].repr(out.obj, Command.getById(out.id)));
    dst.append(e);
  });
  socket_io.on('progress', function(out) {
    Command.getById(out.id).updateProgress(out.obj);
    // console.log(out);
    // var dst = $('.out-' + out.id);
    // var e = $('<div class="out-progress">'+out.obj.text+'</div>');
    // e.appendTo(dst);
  });
});
