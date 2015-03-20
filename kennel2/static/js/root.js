/* main() */

function post_code(compiler, result_container) {
  $('#compile').hide();
  $('#compiling').show();

  var editor = new Editor('#editor', '#editor-settings');
  var code = editor.getValue();
  var stdin = new Stdin('#stdin');

  result_container.post_code(compiler, code, stdin.get_stdin());
}

function update_compile_command(compiler) {
  var command = compiler.get_selected_compiler_element().attr('data-command');
  var compiler_name = compiler.get_selected_compiler_element().text();
  var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).attr('data-flags'); });
  var compiler_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_compiler_option_raw());
  var runtime_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_runtime_option_raw());

  $('#compiler_name').text(compiler_name);

  var compile_command = '$ ' + command + ' ' + compile_options.get().join(' ') + ' ' + compiler_options_arguments.join(' ') + runtime_options_arguments.join(' ');
  $('#compile_command').html($('<code>').text(compile_command));
}

function save(key, value) {
  $.cookie(key, value, { expires: 365, path: '/wandbox' });
}

$(function() {
  $.cookie.json = true;

  var result_container = new ResultContainer('#result-container', '#result-container-settings')
  result_container.onfinish = function() {
    $('#compile').show();
    $('#compiling').hide();
  };
  result_container.result_changed = function() {
    if (!USING_PERMLINK)
      save('wandbox.result_container', result_container.serialize());
  }

  $('#compile').click(function(event) {
    event.preventDefault();
    post_code(compiler, result_container);
  });
  $('#compiling').hide();

  var editor = new Editor('#editor', '#editor-settings');
  editor.onrun = function() {
    post_code(compiler, result_container);
  };
  editor.editor_changed = function() {
    if (!USING_PERMLINK)
      save('wandbox.editor', editor.serialize());
    if (editor.is_legacy()) {
      $('#compile').text('Run');
    } else {
      $('#compile').text('Run (or Ctrl+Enter)');
    }
  };

  // create compiler
  var compiler = new Compiler('#compiler', '#compile-options');
  compiler.compiler_changed = function() {
    if (!USING_PERMLINK)
      save('wandbox.compiler.current', compiler.serialize_current());

    update_compile_command(compiler);

    var lang = compiler.get_selected_compiler_element().attr('data-language');
    editor.setLanguage(lang);
  };
  compiler.compile_option_changed = function() {
    if (!USING_PERMLINK)
      save('wandbox.compiler.current', compiler.serialize_current());

    update_compile_command(compiler);
  };
  compiler.raw_keyup = function() {
    update_compile_command(compiler);
  };
  compiler.raw_changed = function() {
    if (!USING_PERMLINK)
      save('wandbox.compiler.current', compiler.serialize_current());

    update_compile_command(compiler);
  };

  var stdin = new Stdin('#stdin');

  // deserialize if settings is stored in the cookie.
  var compiler_settings = $.cookie('wandbox.compiler.current');
  if (compiler_settings) {
    compiler.deserialize(compiler_settings);
    var editor_settings = $.cookie('wandbox.editor');
    if (editor_settings)
      editor.deserialize(editor_settings);
    var result_container_settings = $.cookie('wandbox.result_container');
    if (result_container_settings)
      result_container.deserialize(result_container_settings);
  } else {
    // select default compiler
    compiler.set_compiler(DEFAULT_COMPILER);
  }

  // expand window if using permlink
  if (USING_PERMLINK) {
    editor.expand(true).change();
    result_container.expand(true).change();
  }

  // deserialize if code exists.
  var code = JSON_CODE;
  if (code != null) {
    editor.setValue(code.code);
    stdin.set_stdin(code.stdin);

    compiler.deserialize({
      compiler: code.compiler,
      compile_options: code.options,
      compiler_option_raw: code['compiler-option-raw'],
      runtime_option_raw: code['runtime-option-raw'],
    });

    result_container.set_code(compiler, code.code, code.stdin, code.outputs);
  }

  update_compile_command(compiler);

  editor.focus();
});

/* compiler() */

function to_id(id) {
  return id.replace(/\./g, "\\.");
}

function Compiler(compiler_id, compile_options_id) {
  var self = this;

  this.compiler_id = compiler_id;
  this.compile_options_id = compile_options_id;

  $(this.compiler_id).change(function() {
    $(self.compile_options_id).children().removeClass('selected');
    self._compile_options().addClass('selected');

    if (self.compiler_changed)
      self.compiler_changed();
  });
  $(this.compile_options_id + ' input').change(function() {
    if (self.compile_option_changed)
      self.compile_option_changed();
  });
  $(this.compile_options_id + ' select').change(function() {
    if (self.compile_option_changed)
      self.compile_option_changed();
  });

  var init_editor = function(es) {
    es.map(function(n,e) {
      if ($(e).children().size() != 0)
        return;

       var editor = CodeMirror(e, {
         viewportMargin: Infinity,
         smartIndent: false,
       });
       $(e).data('editor', editor);
    });
  };
  init_editor($(this.compile_options_id).find('.compiler-option-raw'));
  init_editor($(this.compile_options_id).find('.runtime-option-raw'));

  $(this.compile_options_id).find('.option-raw').keyup(function() {
    if (self.raw_keyup)
      self.raw_keyup();
  });
  $(this.compile_options_id).find('.option-raw').change(function() {
    if (self.raw_changed)
      self.raw_changed();
  });

}

Compiler.prototype._compiler = function() {
  return $(this.compiler_id);
}

Compiler.prototype.get_selected_compiler = function() {
  return this.get_selected_compiler_element().val();
}

Compiler.prototype.get_selected_compiler_element = function() {
  return $(this.compiler_id + ' > :enabled:eq(' + $(this.compiler_id)[0].selectedIndex + ')');
}

Compiler.prototype.set_compiler = function(compiler) {
  this._compiler().val(compiler);
  this._compiler().change();
}

Compiler.prototype._compile_options = function() {
  var compiler = this.get_selected_compiler();
  return $(this.compile_options_id + ' div[data-compiler=' + to_id(compiler) + ']');
}

Compiler.prototype.get_checked_compile_options = function() {
  var options = this._compile_options();
  return options.find(':checked,:selected');
}

Compiler.prototype._selected_option_raw = function(cls, value) {
  var options = this._compile_options();
  var e = options.find(cls);
  if (e.size() == 0)
    return undefined;
  return value ? e.data('editor').setValue(value) : e.data('editor').getValue();
}

Compiler.prototype.get_selected_compiler_option_raw = function() {
  return this._selected_option_raw('.compiler-option-raw');
}
Compiler.prototype.set_selected_compiler_option_raw = function(value) {
  this._selected_option_raw('.compiler-option-raw', value);
}

Compiler.prototype.get_selected_runtime_option_raw = function() {
  return this._selected_option_raw('.runtime-option-raw');
}
Compiler.prototype.set_selected_runtime_option_raw = function(value) {
  this._selected_option_raw('.runtime-option-raw', value);
}

Compiler.prototype.raw_to_arguments = function(raw) {
  if (!raw)
    return [];

  // split by line-break
  var xs = raw.split(/\r\n|\r|\n/);
  // remove last line-break
  if (xs.length != 0 && xs[xs.length - 1].length == 0)
    xs.pop();
  // escape double-quotes and backslash
  xs = $.map(xs, function(x) { return x.replace(/\\|"/g, '\\$&'); });
  // enclose double-quotes
  xs = $.map(xs, function(x) { return '"' + x + '"'; });

  return xs;
}

Compiler.prototype.serialize_current = function() {
  return {
    compiler: this.get_selected_compiler(),
    compile_options: this.get_checked_compile_options().map(function(n,e) { return $(e).val(); }).get().join(','),
    compiler_option_raw: this.get_selected_compiler_option_raw(),
    runtime_option_raw: this.get_selected_runtime_option_raw(),
  };
}

Compiler.prototype.deserialize = function(settings) {
  this.set_compiler(settings.compiler);

  var options = settings.compile_options.split(',');
  var options_map = {};
  for (var i = 0; i < options.length; i++) {
    options_map[options[i]] = '';
  }
  this._compile_options().find('input').each(function() {
    var elem = $(this);
    var val = elem.val();
    var v = val in options_map;
    elem.prop('checked', v);
  });
  this._compile_options().find('select > option').each(function() {
    var elem = $(this);
    var val = elem.val();
    var v = val in options_map;
    if (v)
      elem.parent().val(val);
  });
  this.set_selected_compiler_option_raw(settings.compiler_option_raw);
  this.set_selected_runtime_option_raw(settings.runtime_option_raw);
}

$(function() {
  jQuery.fn.toggleOption = function(show) {
    jQuery(this).toggle(show);
    if (show) {
      if (jQuery(this).parent('span.toggleOption').length != 0)
        jQuery(this).unwrap();
    } else {
      if (jQuery(this).parent('span.toggleOption').length == 0)
        jQuery(this).wrap('<span class="toggleOption" style="display: none;" />');
    }
  };

  $.cookie.json = true;

  var matchedOption = function(option, searchText) {
    var matched = function(str) {
      return str.toLowerCase().indexOf(searchText.toLowerCase()) >= 0;
    };
    if (matched(option.attr('value'))) return true;
    if (matched(option.attr('data-language'))) return true;
    if (matched(option.attr('data-display-name'))) return true;
    return false;
  };
  var changeCandidateList = function(searchText, excepted) {
    var count = 0;
    $('#compiler option').each(function() {
      var option = $(this);
      if (matchedOption(option, searchText) ||
          excepted && option.attr('value') == excepted) {
        option.toggleOption(true);
        count += 1;
      } else {
        option.toggleOption(false);
      }
    });
    $('#compiler').attr('size', count > 5 ? count : 5);
    $('#compiler').change();

    $.cookie('wandbox.compiler.search', { text: searchText }, { expires: 365, path: '/wandbox' });
  };
  $('#typeahead-search').keyup(function() {
    changeCandidateList($(this).val());
  });
  $('#typeahead-search').change(function() {
    changeCandidateList($(this).val());
  });

  var search = $.cookie('wandbox.compiler.search');
  if (search) {
    $('#typeahead-search').val(search.text);
    changeCandidateList($('#typeahead-search').val(), JSON_CODE && JSON_CODE.compiler);
  }

  $('#typeahead-clear').click(function() {
    $('#typeahead-search').val('');
    $('#typeahead-search').change();
  });
});

/* editor() */

function Editor(id, settings_id) {
  this.id = id;
  this.settings_id = settings_id;

  // not initialized
  if ($(this.id).children().size() == 0) {
    this._initialize();
  }
}

Editor.prototype._initialize = function() {
  var self = this;

  $(this.id).append('<div class="smart-editor"></div>');
  $(this.id).append('<textarea class="span12 legacy-editor"></textarea>');

  var codemirror = CodeMirror($(this.id + ' .smart-editor')[0], {
    lineNumbers: true,
    theme: 'user',
    indentUnit: 4,
    extraKeys: {
      'Ctrl-Enter': function() {
        if (self.onrun)
          self.onrun(editor);
      },
      Tab: function(cm) {
        var cursor = cm.getCursor()['ch'];
        var indentUnit = cm.getOption("indentUnit");
        var newCursor = Math.floor((cursor + indentUnit) / indentUnit) * indentUnit
        var indentNum = newCursor - cursor;
        var spaces = Array(indentNum + 1).join(" ");
        cm.replaceSelection(spaces, "end", "+input");
      },
    },
  });
  $(this.id).find('.smart-editor').data('editor', codemirror);

  var editor = this.smart_editor()

  $(this.settings_id).find('select').change(function() {
    var value = $(this).val();
    editor.setOption('keyMap', value);
    if (self.editor_changed)
      self.editor_changed();
  }).change();
  $(this.settings_id).find('input.use-legacy-editor').change(function() {
    var val = self.getValue();
    var checked = $(this).prop('checked');
    if (checked) {
      $(this.id + ' .smart-editor').hide();
      $(this.id + ' .legacy-editor').show();
    } else {
      $(this.id + ' .smart-editor').show();
      $(this.id + ' .legacy-editor').hide();
    }
    self.setValue(val);
    self.focus();
    if (checked) {
      $(self.settings_id).find('select').attr('disabled', 'disabled');
    } else {
      $(self.settings_id).find('select').removeAttr('disabled');
    }
    if (self.editor_changed)
      self.editor_changed();
  });
  $(this.id + ' .legacy-editor').hide();

  $(this.settings_id).find('input.expand-editor').change(function(e) {
    if ($(e.target).prop('checked')) {
      $(this.id + ' .smart-editor').addClass('expand');
    } else {
      $(this.id + ' .smart-editor').removeClass('expand');
    }
    if (self.editor_changed)
      self.editor_changed();
  });

  $(this.settings_id).find('input.no-auto-indent').change(function(e) {
    self.smart_editor().setOption('smartIndent', !($(e.target).prop('checked')));
    if (self.editor_changed)
      self.editor_changed();
  });
}

Editor.prototype.is_legacy = function() {
  return $(this.id + ' .smart-editor').css('display') == 'none';
}
Editor.prototype.smart_editor = function() {
  return $(this.id).find('.smart-editor').data('editor');
}
Editor.prototype.legacy_editor = function() {
  return $(this.id + ' .legacy-editor');
}
Editor.prototype.editor = function() {
  return this.is_legacy() ? this.legacy_editor() : this.smart_editor();
}

Editor.prototype.focus = function() {
  this.editor().focus();
}

Editor.prototype.getValue = function() {
  return this.is_legacy() ? this.legacy_editor().val() : this.smart_editor().getValue();
}

Editor.prototype.setValue = function(value) {
  if (this.is_legacy()) {
    this.legacy_editor().val(value);
  } else {
    this.smart_editor().setValue(value);
  }
}

function _get_editor_mode(lang) {
  return EDITOR_MODE_MAPPING[lang];
}

Editor.prototype.setLanguage = function(lang) {
  this.smart_editor().setOption('mode', _get_editor_mode(lang));
}

Editor.prototype.expand = function(value) {
  var eexpand = $(this.settings_id).find('input.expand-editor');
  eexpand.prop('checked', value);
  return eexpand;
}

Editor.prototype.serialize = function() {
  return {
    keybinding: $(this.settings_id).find('select').val(),
    use_smart_editor: $(this.settings_id).find('input.use-legacy-editor').prop('checked'),
    no_auto_indent: $(this.settings_id).find('input.no-auto-indent').prop('checked'),
    expand_editor: $(this.settings_id).find('input.expand-editor').prop('checked'),
  };
}

Editor.prototype.deserialize = function(settings) {
  var ekey = $(this.settings_id).find('select');
  ekey.val(settings.keybinding);

  var esmart = $(this.settings_id).find('input.use-legacy-editor');
  esmart.prop('checked', settings.use_smart_editor);

  var eauto = $(this.settings_id).find('input.no-auto-indent');
  eauto.prop('checked', settings.no_auto_indent);

  var eexpand = this.expand(settings.expand_editor);

  ekey.change();
  esmart.change();
  eauto.change();
  eexpand.change();
}

/* result_container */

function ResultContainer(id, settings_id) {
  var self = this;

  this.id = id;
  this.settings_id = settings_id;
  this.name = 1;
  this.running = false;

  $(this.settings_id).find('input.nowrap-output-window').change(function(e) {
    var content = $(self.id + ' .tab-content');
    $(e.target).prop('checked') ? content.addClass('nowrap') : content.removeClass('nowrap')
    if (self.result_changed)
        self.result_changed();
  });
  $(this.settings_id).find('input.expand-output-window').change(function(e) {
    var content = $(self.id + ' .tab-content');
    $(e.target).prop('checked') ? content.addClass('expand') : content.removeClass('expand')
    if (self.result_changed)
        self.result_changed();
  });
}

ResultContainer.prototype._nav_tabs = function() {
  return $(this.id + ' .tabbable ul');
}

ResultContainer.prototype._tab_content = function() {
  return $(this.id + ' .tab-content');
}

ResultContainer.prototype._next_name = function() {
  var name = this.name + '';
  this.name += 1;
  return name;
}

ResultContainer.prototype._post_init = function(compiler, code, name) {
  var self = this;

  var TAB_PREFIX = 'result-container-tab-';

  var id_name = TAB_PREFIX + name;
  var tab = $('<li><button type="button" class="close" href="#">&times;</button><a data-toggle="tab" href="#' + id_name + '">#' + name + '</a></li>');
  var content = $('<div class="tab-pane active result-window" id="' + id_name + '">');

  var close = tab.find('.close');
  close.click(function() {
    if (tab.hasClass('active')) {
      tab.prev().find('a').tab('show');
      tab.next().find('a').tab('show');
    }
    tab.remove();
    content.remove();
  });

  this._nav_tabs().prepend(tab);
  this._tab_content().prepend(content);

  tab.find('a').tab('show');

  var result_window = new ResultWindow('#' + id_name);
  result_window.onfinish = function() {
    self.running = false;
    if (self.onfinish)
      self.onfinish();
  };
  return result_window;
}

ResultContainer.prototype.post_code = function(compiler, code, stdin) {
  if (this.running) {
    return;
  }
  this.running = true;

  var result_window = this._post_init(compiler, code, this._next_name());
  result_window.post_code(compiler, code, stdin);
}

ResultContainer.prototype.set_code = function(compiler, code, stdin, outputs) {
  var result_window = this._post_init(compiler, code, 'permlink');
  result_window.set_code(compiler, code, stdin, outputs);
}

ResultContainer.prototype.expand = function(value) {
  var eexpand = $(this.settings_id).find('input.expand-output-window');
  eexpand.prop('checked', value);
  return eexpand;
}

ResultContainer.prototype.serialize = function() {
  return {
    nowrap: $(this.settings_id).find('input.nowrap-output-window').prop('checked'),
    expand: $(this.settings_id).find('input.expand-output-window').prop('checked'),
  };
}

ResultContainer.prototype.deserialize = function(settings) {
  var enowrap = $(this.settings_id).find('input.nowrap-output-window');
  enowrap.prop('checked', settings.nowrap);

  var eexpand = this.expand(settings.expand);

  enowrap.change();
  eexpand.change();
}

/* result_window() */

function parse(str) {
  var index = str.indexOf(':');
  return {
    type: str.substring(0, index),
    message: str.substring(index + 1),
  };
}

function random_string(n) {
    var a = 'abcdefghijklmnopqrstuvwxyz'
    var s = '';
    for (var i = 0; i < n; i++) {
        s += a[Math.floor(Math.random() * a.length)];
    }
    return s;
}

var BASE_SOURCE_URL = '@{EmptySourceR}'
var BASE_COMPILE_URL = '@{EmptyCompileR}'

function ResultWindow(id) {
  this.id = id;
  $(this.id).empty();
  var permlink = $('<div class="permlink"></div>');
  var code = $('<div class="code-window"></div>');
  var output = $('<div class="output-window"></div>');
  $(this.id).append(permlink);
  $(this.id).append(code);
  $(this.id).append(output);
}

ResultWindow.prototype._permlink = function() {
  return $(this.id).find('.permlink');
}
ResultWindow.prototype._code_window = function() {
  return $(this.id).find('.code-window');
}
ResultWindow.prototype._output_window = function() {
  return $(this.id).find('.output-window');
}

ResultWindow.prototype.permlink = function(compiler_info, code, stdin, outputs) {
  var self = this;

  var a = $('<a href="#" class="btn btn-default">Share This Code</a>')
    .appendTo(this._permlink());
  a.click(function(event) {
    event.preventDefault();
    self.post_permlink(compiler_info, code, stdin, outputs);
  });
}

ResultWindow.prototype.post_permlink = function(compiler_info, code, stdin, outputs) {
  var self = this;

  var pm = this._permlink().find('a');
  if (pm.hasClass('disable')) return;
  pm.addClass('disable');

  outputs = $.map(outputs, function(e) {
    return { type: e.type, output: e.output };
  });

  var data = {
    compiler: compiler_info.selected_compiler,
    code: code,
    stdin: stdin,
    options: compiler_info.compile_options,
    'compiler-option-raw': compiler_info.compiler_option_raw,
    'runtime-option-raw': compiler_info.runtime_option_raw,
    outputs: outputs,
  };

  $.post(URL_PERMLINK, JSON.stringify(data),
    function(json) {
      if (!json.success) {
        pm.removeClass('disable');
        return;
      }

      pm.remove();

      var url = URL_PERMLINK + '/' + json.link;
      $('<a href="' + url + '" target="_blank" id="permlink">URL</a>')
        .appendTo(self._permlink());

      var xs = document.URL.split('/');
      var abs_url =  xs[0] + "//" + xs[2] + url;
      var div = $('<div></div>').appendTo(self._permlink());
      twttr.widgets.createShareButton(
        abs_url,
        div[0],
        function() { },
        {
          count: 'none',
          //text: 'wandbox'
        });

      window.history.pushState(null, null, url);
    });
}

ResultWindow.prototype.code_window = function(compiler_info, code, stdin) {
  var show_code = $('<div><code><a href="#">Show Code</a></code></div>');
  var hide_code = $('<div><code><a href="#">Hide Code</a></code></div>');
  var pre = $('<div><pre></pre><pre></pre></div>');
  pre.find('pre').eq(0).text(code);
  pre.find('pre').eq(1).text(stdin);

  var compiler_data = compiler_info.compiler_data;
  var code = compiler_info.compile_command_code;

  hide_code.toggle(false);
  compiler_data.toggle(false);
  pre.toggle(false);
  code.toggle(false);

  var toggle_func = function(event) {
    event.preventDefault();
    show_code.toggle();
    hide_code.toggle();
    compiler_data.toggle();
    pre.toggle();
    code.toggle();
  };
  show_code.click(toggle_func);
  hide_code.click(toggle_func);

  this._code_window().append(show_code);
  this._code_window().append(hide_code);
  this._code_window().append(compiler_data);
  this._code_window().append(code);
  this._code_window().append(pre);
}

ResultWindow.prototype.to_compiler_info = function(compiler) {
  var selected_compiler = compiler.get_selected_compiler();
  var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).val(); }).get().join(',');
  var compiler_option_raw = compiler.get_selected_compiler_option_raw();
  var runtime_option_raw = compiler.get_selected_runtime_option_raw();

  var compiler_data = (function(compiler) {
    var data = compiler.get_selected_compiler_element().html();
    return $('<div><p>' + data + '</p></div>');
  })(compiler);

  var compile_command_code = (function(compiler) {
    var command = compiler.get_selected_compiler_element().html();
    var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).attr('data-flags'); });
    var compiler_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_compiler_option_raw());
    var runtime_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_runtime_option_raw());

    var compile_command = '$ ' + command + ' ' + compile_options.get().join(' ') + ' ' + compiler_options_arguments.join(' ') + runtime_options_arguments.join(' ');
    return $('<code>' + compile_command + '</code>');
  })(compiler);

  var compiler_info = {
    selected_compiler: selected_compiler,
    compile_options: compile_options,
    compiler_option_raw: compiler_option_raw,
    runtime_option_raw: runtime_option_raw,
    compiler_data: compiler_data,
    compile_command_code: compile_command_code,
  };

  return compiler_info;
}

ResultWindow.prototype.post_code = function(compiler, code, stdin) {
  var self = this;

  var compiler_info = this.to_compiler_info(compiler);

  this.code_window(compiler_info, code, stdin);

  src = new PostEventSource(URL_COMPILE, {
      compiler: compiler_info.selected_compiler,
      code: code,
      stdin: stdin,
      options: compiler_info.compile_options,
      'compiler-option-raw': compiler_info.compiler_option_raw,
      'runtime-option-raw': compiler_info.runtime_option_raw,
  });

  var finalize = function() {
    src.close();

    var outputs = self._output_window().find('p').map(function(n,e) {
        return { 'type': $(e).attr('data-type'), 'output': $(e).text() };
    });
    self.permlink(compiler_info, code, stdin, outputs);

    if (self.onfinish)
      self.onfinish();
  };

  var preview_paragraph = null;
  src.onmessage = function(msg) {
    var output = self._output_window()

    var data = parse(msg.data);
    var is_message = function(type) {
      return data.type == "CompilerMessageS" ||
             data.type == "CompilerMessageE" ||
             data.type == "StdOut" ||
             data.type == "StdErr";
    };
    if (is_message(data.type) &&
        preview_paragraph &&
        preview_paragraph.hasClass(data.type)) {
      var p = preview_paragraph;
      p.text(p.text() + data.message)
    } else {
      var p = $('<pre>').addClass(data.type)
                        .attr('data-type', data.type)
                        .text(data.message)
                        .appendTo(output);
      preview_paragraph = p;
    }
    output[0].scrollTop = output[0].scrollHeight;

    if (data.type == 'Control' && data.message == 'Finish') {
        finalize();
    }
  };

  src.onerror = function() {
    finalize();
  }
}

ResultWindow.prototype.set_code = function(compiler, code, stdin, outputs) {
  var self = this;
  var compiler_info = self.to_compiler_info(compiler);
  this.code_window(compiler_info, code, stdin);
  $.each(outputs, function(n,e) {
    var type = e.type;
    var output = e.output;
    $('<p>').addClass(type)
            .attr('data-type', type)
            .text(output)
            .appendTo(self._output_window());
  });
}

/* Stdin */

function Stdin(stdin_id) {
  this.stdin_id = stdin_id;

  var init_editor = function(es) {
    es.map(function(n,e) {
      if ($(e).children().size() != 0)
        return;

       var editor = CodeMirror(e, {
         viewportMargin: Infinity,
         smartIndent: false,
       });
       $(e).data('editor', editor);
    });
  };
  init_editor($(this.stdin_id));
}
Stdin.prototype._stdin = function(value) {
  var e = $(this.stdin_id);
  return value ? e.data('editor').setValue(value) : e.data('editor').getValue();
}
Stdin.prototype.get_stdin = function() {
  return this._stdin();
}
Stdin.prototype.set_stdin = function(value) {
  this._stdin(value);
}
