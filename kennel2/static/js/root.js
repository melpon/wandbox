/* main() */

function get_codes(editor) {
  return $('#wandbox-editor-header > li > a.wandbox-editor-name-tab').map(function() {
    var file = '';
    if ($(this).find('.wandbox-renamable').length != 0) {
      file = $(this).find('.wandbox-renamable').text();
    } else {
      file = $(this).find('.wandbox-renaming > input').attr('data-original-value');
    }
    var code_ = editor.getValue($($(this).attr('href')));
    return { 'file': file, 'code': code_ };
  }).get();
}
function post_code(compiler, result_container) {
  $('#wandbox-compile').hide();
  $('#wandbox-compiling').show();

  var editor = new Editor('#wandbox-editor-settings');
  var code = editor.getValue($('#wandbox-editor-default'));
  var codes = get_codes(editor);
  var stdin = new Stdin('#stdin');

  result_container.post_code(compiler, code, codes, stdin.get_stdin());
}
function insert_template(template_name) {
  var editor = new Editor('#wandbox-editor-settings');
  $.getJSON(URL_API_TEMPLATE + template_name, function(json) {
    editor.setValue($('#wandbox-editor-default'), json.code);
  });
}

function update_compile_command(compiler) {
  var command = compiler.get_selected_compiler_element().attr('data-command');
  var compiler_name = compiler.get_selected_compiler_element().attr('data-compiler');
  var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).attr('data-flags'); });
  var compiler_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_compiler_option_raw());
  var runtime_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_runtime_option_raw());
  var options_arguments = compiler.get_selected_compiler_element().attr('data-options-raw') == 'compiler' ? compiler_options_arguments : runtime_options_arguments;

  $('#compiler_name').text(compiler_name);

  var compile_command = '$ ' + command + ' ' + compile_options.get().join(' ') + ' ' + options_arguments.join(' ');
  $('#compile_command').html($('<code>').text(compile_command));
}

function set_author(author) {
  html = '';

  var created_at = new Date();
  var created_at_html = '<p class="time-ago"' +
                        '   datetime="' + author.created_at * 1000 + '"' +
                        '   data-toggle="tooltip"' +
                        '   data-placement="bottom"' +
                        '   title="' + new Date(author.created_at * 1000).toString() + '"></p>';
  if (author.username === undefined) {
    html +=
      '<div>' +
      '  <p class="wandbox-author-createdby">created by</p>' +
      '  <i>anonymous</i>' +
      '  ' + created_at_html +
      '</div>' +
      '<div>' +
      '  <div class="wandbox-medium-avatar"></div>' +
      '</div>' +
      '';
  } else {
    html +=
      '<div>' +
      '  <p class="wandbox-author-createdby">created by</p>' +
      '  <a href="' + author.username_url + '">' + author.username + '</a>' +
      '  ' + created_at_html +
      '</div>' +
      '<div>' +
      '  <img class="wandbox-medium-avatar" src="' + author.avatar_url + '" />' +
      '</div>' +
      '';
  }

  $('.wandbox-author').empty();
  $(html).appendTo($('.wandbox-author'));

  $('[data-toggle="tooltip"]').tooltip();
}

function save(key, value) {
  // $.cookie(key, value, { expires: 365, path: $('body').attr('data-webroot') });
  localStorage.setItem(key, JSON.stringify(value));
}
function load(key) {
  // return $.cookie(key);
  return JSON.parse(localStorage.getItem(key));
}

$(function() {
  // $.cookie.json = true;
  $('[data-toggle="tooltip"]').tooltip();

  var result_container = new ResultContainer('#result-container', '#result-container-settings')
  result_container.onfinish = function() {
    $('#wandbox-compile').show();
    $('#wandbox-compiling').hide();
  };
  result_container.result_changed = function() {
  }


  $('#wandbox-compile').click(function(event) {
    event.preventDefault();
    post_code(compiler, result_container);
  });
  $('#wandbox-compiling').hide();

  var editor = new Editor('#wandbox-editor-settings');
  editor.onrun = function() {
    post_code(compiler, result_container);
  };
  editor.editor_changed = function() {
    $('#wandbox-compile').text('Run (or Ctrl+Enter)');
  };

  // create compiler
  var compiler = new Compiler('#wandbox-compiler', '#wandbox-compile-options', function() {
    post_code(compiler, result_container);
  });
  var initialized = false;
  var update_template_code = function() {
    if (editor.getValue($('#wandbox-editor-default')).length == 0) {
      var template_name = compiler.get_selected_compiler_element().attr('data-default-template');
      if (template_name.length != 0) {
        insert_template(template_name);
      }
    }
  };
  compiler.compiler_changed = function() {
    if (initialized) {
      update_template_code();
    }

    update_compile_command(compiler);

    var lang = compiler.get_selected_compiler_element().attr('data-language');
    editor.setLanguage(lang);
  };
  compiler.compile_option_changed = function() {
    update_compile_command(compiler);
  };
  compiler.raw_keyup = function() {
    update_compile_command(compiler);
  };
  compiler.raw_changed = function() {
    update_compile_command(compiler);
  };

  var stdin = new Stdin('#stdin', function() {
    post_code(compiler, result_container);
  });

  // deserialize if settings is stored in the local storage.
  var compiler_settings = load('wandbox.compiler.current');
  if (compiler_settings) {
    compiler.deserialize(compiler_settings);
    var editor_settings = load('wandbox.editor');
    if (editor_settings)
      editor.deserialize(editor_settings);
    var result_container_settings = load('wandbox.result_container');
    if (result_container_settings)
      result_container.deserialize(result_container_settings);
  } else {
    // select default compiler
    compiler.set_compiler(DEFAULT_COMPILER);
  }
  // restore editing code
  if (!USING_PERMLINK) {
    var editor_code = load('wandbox.editor.code');
    if (editor_code) {
      editor.setValue($('#wandbox-editor-default'), editor_code);
    }
    var editor_codes = load('wandbox.editor.codes');
    for (var i = 0; i < (editor_codes || []).length; i++) {
      editor.addEditor(editor_codes[i]['file'], editor_codes[i]['code']);
    }
  }

  // deserialize if code exists.
  var code = JSON_CODE;
  if (code != null) {
    editor.setValue($('#wandbox-editor-default'), code.code);
    for (var i = 0; i < (code.codes || []).length; i++) {
      editor.addEditor(code.codes[i]['file'], code.codes[i]['code']);
    }
    stdin.set_stdin(code.stdin);

    compiler.deserialize({
      compiler: code.compiler,
      compile_options: code.options,
      compiler_option_raw: code['compiler-option-raw'],
      runtime_option_raw: code['runtime-option-raw'],
    });

    result_container.set_code(compiler, code.code, code.codes || [], code.stdin, code.results);

    set_author(code.author);
  }

  update_compile_command(compiler);

  $(window).unload(function() {
    if (!USING_PERMLINK) {
      save('wandbox.result_container', result_container.serialize());
      save('wandbox.compiler.current', compiler.serialize_current());
      save('wandbox.editor', editor.serialize());
      save('wandbox.editor.code', editor.getValue($('#wandbox-editor-default')));
      save('wandbox.editor.codes', get_codes(editor));
    }
  });

  $(document).on('click', '.wandbox-shown-runtime-option-raw', function (e) {
    $(this).hide();
    $(this).parent().find('.wandbox-hidden-runtime-option-raw').show();
  });

  if (!USING_PERMLINK) {
    update_template_code();
  }
  initialized = true;
});

/* compiler() */

function to_id(id) {
  return id.replace(/\./g, "\\.");
}

function Compiler(compiler_id, compile_options_id, ctrl_enter) {
  var self = this;
  $('.wandbox-dropdown-area').click(function (e) {
    if (!$(e.target).hasClass('wandbox-dropdown-listitem')) {
      e.stopPropagation();
    }
  });
  $('a.wandbox-dropdown-listitem').hover(function (e) {
    $('.wandbox-dropdown-version').text($(e.target).attr('data-display-name'));
    $('.wandbox-dropdown-version').attr('title', $(e.target).attr('data-display-name'));
  });
  $('a.wandbox-dropdown-listitem').on('shown.bs.tab', function (e) {
    // hide all listitem tabs without this tab
    $(e.target).closest('.tab-content').find('.tab-pane:not(.active) ul > li').removeClass('active');

    $('.wandbox-current-compiler-text').text($(e.target).attr('data-display-name'));
    $('.wandbox-current-compiler-text').attr('title', $(e.target).attr('data-display-name'));

    // active language
    var lang = $($(e.target).attr('href')).attr('data-language');
    var tab_id = LANGUAGE_NAME_TO_TAB_ID[lang];
    var $lang = $('.wandbox-dropdown-lang-area a[href="#' + tab_id + '"]');
    $lang.tab('show');

    $('.wandbox-current-compiler-language').text(lang);

    if (self.compiler_changed)
      self.compiler_changed();
  });
  $('.wandbox-dropdown-lang-area a').click(function (e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).tab('show');
  });

  $('.wandbox-dropdown-lang-area a').click(function (e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).tab('show');
  });

  this.compiler_id = compiler_id;
  this.compile_options_id = compile_options_id;

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
         extraKeys: {
           'Ctrl-Enter': ctrl_enter,
           'Cmd-Enter': ctrl_enter,
         },
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

  $(document).on('click', '.wandbox-insert-template > a', function (e) {
    var template_name = self.get_selected_compiler_element().attr('data-default-template');
    insert_template(template_name);
  });
}

Compiler.prototype._compiler = function() {
  return $(this.compiler_id);
}

Compiler.prototype.get_selected_compiler = function() {
  return this.get_selected_compiler_element().attr('data-compiler');
}

Compiler.prototype.get_selected_compiler_element = function() {
  return $(this.compile_options_id + ' .active');
}

Compiler.prototype.set_compiler = function(compiler) {
  var anchor = this._compiler().find('a[href="#' + COMPILER_NAME_TO_TAB_ID[compiler] + '"]');
  if (anchor.length == 0)
    anchor = this._compiler().find('a[href="#' + DEFAULT_COMPILER + '"]');
  anchor.tab('show');
}

Compiler.prototype._compile_options = function() {
  return this.get_selected_compiler_element();
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
  return value !== undefined ? e.data('editor').setValue(value) : e.data('editor').getValue();
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

  if (settings.runtime_option_raw) {
    this._compile_options().find('.wandbox-shown-runtime-option-raw').hide();
    this._compile_options().find('.wandbox-hidden-runtime-option-raw').show();
  }
  this.set_selected_runtime_option_raw(settings.runtime_option_raw);
}

/* editor() */

function Editor(settings_id) {
  this.settings_id = settings_id;

  // not initialized
  if ($('#wandbox-editor-default').children().length == 0) {
    this._initialize();
  }
}

function insert_tab_space(cm) {
  var cursor = cm.getCursor()['ch'];
  var indentUnit = cm.getOption("indentUnit");
  var newCursor = Math.floor((cursor + indentUnit) / indentUnit) * indentUnit
  var indentNum = newCursor - cursor;
  var spaces = Array(indentNum + 1).join(" ");
  cm.replaceSelection(spaces, "end", "+input");
}

Editor.prototype._to_editor = function(elem) {
  var self = this;
  var codemirror = CodeMirror(elem[0], {
    lineNumbers: true,
    theme: 'user',
    indentUnit: 4,
    smartIndent: false,
    extraKeys: {
      'Ctrl-Enter': function() {
        if (self.onrun)
          self.onrun();
      },
      'Cmd-Enter': function() {
        if (self.onrun)
          self.onrun();
      },
      Tab: insert_tab_space,
    },
  });
  elem.data('editor', codemirror);
}

Editor.prototype._add_editor = function(elem) {
  var smart = $('<div class="wandbox-smart-editor"></div>').appendTo(elem);
  this._to_editor(smart);
}

function _normalize_path(path) {
  var parts = path.split('/');
  var result = [];
  for (var i = 0; i < parts.length; i++) {
    var part = parts[i];
    if (part == '') continue;
    if (part == '.') continue;
    if (part == '..') {
      if (result.length != 0) {
        result.pop();
      }
      continue;
    }
    result.push(part);
  }
  return result.join('/');
}

Editor.prototype._initialize = function() {
  var self = this;

  this._add_editor($('#wandbox-editor-default'));

  $(this.settings_id).find('select.wandbox-select').change(function() {
    var value = $(this).val() || 'default';
    self.contents().each(function(i) {
      self.editor($(this)).setOption('keyMap', value);
    });
    if (self.editor_changed)
      self.editor_changed();
  }).change();

  $(this.settings_id).find('select.wandbox-spaces-or-tab').change(function(e) {
    var name = $(this).val() || 'spaces-4';
    $(this).val(name);
    // name: 'spaces-N' or 'tab'
    var xs = name.split('-');
    var useTab = xs[0] == 'tab';
    var indentUnit = xs.length >= 2 ? parseInt(xs[1], 10) : 0;
    self.contents().each(function(i) {
      var extraKeys = self.editor($(this)).getOption('extraKeys');
      if (useTab) {
        extraKeys.Tab = undefined;
        self.editor($(this)).setOption('indentWithTabs', true);
      } else {
        extraKeys.Tab = insert_tab_space;
        self.editor($(this)).setOption('indentUnit', indentUnit);
        self.editor($(this)).setOption('indentWithTabs', false);
      }
    });
    if (self.editor_changed)
      self.editor_changed();
  });
  $(this.settings_id).find('select.wandbox-tab-width').change(function(e) {
    var name = $(this).val() || 'tab-4';
    $(this).val(name);
    // name: 'tab-N'
    var xs = name.split('-');
    var tabSize = parseInt(xs[1], 10);
    self.contents().each(function(i) {
      self.editor($(this)).setOption('tabSize', tabSize);
    });
    if (self.editor_changed)
      self.editor_changed();
  });

  $(this.settings_id).find('input.smart-indent').change(function(e) {
    var smartIndent = $(e.target).prop('checked');
    self.contents().each(function(i) {
      self.editor($(this)).setOption('smartIndent', smartIndent);
    });
    if (self.editor_changed)
      self.editor_changed();
  });

  this.name_counter = 1;
  $('#wandbox-editor-add-tab').click(function(e) {
    // check to already exists
    var file_names = $('li .wandbox-editor-name-tab > .wandbox-renamable').map(function() {
      return $(this).text();
    }).get();
    var file_name;
    while (true) {
      file_name = 'noname-' + self.name_counter;
      if (file_names.indexOf(file_name) == -1) break;
      self.name_counter += 1;
    }

    self.addEditor(file_name, '');
  });
  $(document).on('click', '.wandbox-editor-name-tab > button', function(e) {
    var li = $(this).closest('li');
    if (li.hasClass('active')) {
      var next = li.next();
      var a = next.find('a');
      if (a.hasClass('wandbox-editor-name-tab')) {
        a.tab('show');
      } else {
        $('#wandbox-editor-default-tab').tab('show');
      }
    }
    $($(this).closest('a').attr('href')).remove();
    li.remove();
  });
  $(document).on('click', 'li.active .wandbox-editor-name-tab > .wandbox-renamable', function(e) {
    var input = $('<input type="text">');
    input.attr('value', $(this).text());
    input.attr('data-original-value', $(this).text());
    $(this).empty();
    $(this).append(input);
    $(this).removeClass('wandbox-renamable');
    $(this).addClass('wandbox-renaming');
    input.select();
    input.focus();
  });
  $(document).on('dragstart', 'li a.wandbox-editor-name-tab', function(e) {
    if ($(this).find('.wandbox-renaming').length != 0) {
      // renaming
      e.preventDefault();
    }
  });
  $(document).on('shown.bs.tab', 'li a.wandbox-editor-name-tab', function (e) {
    self.editor($($(this).attr('href'))).refresh();
    self.focus($($(this).attr('href')));
  });
  $(document).on('shown.bs.tab', 'li a#wandbox-editor-default-tab', function (e) {
    self.editor($($(this).attr('href'))).refresh();
    self.focus($($(this).attr('href')));
  });
  $(document).on('focusout keypress', 'li .wandbox-editor-name-tab > .wandbox-renaming > input', function(e) {
    if (e.type == 'focusout' || e.which == 13) {
      if ($(this).hasClass('removed')) return;

      var text = $(this).val();
      text = _normalize_path(text);
      if (text == null || text.length == 0) {
        text = $(this).attr('data-original-value');
      }

      // check to already exists
      var foundIndex = $('li .wandbox-editor-name-tab > .wandbox-renamable').map(function() {
        return $(this).text();
      }).get().indexOf(text);
      if (foundIndex != -1) {
        text = $(this).attr('data-original-value');
      }

      var parent = $(this).parent();
      $(this).addClass('removed');
      $(this).remove();
      parent.removeClass('wandbox-renaming');
      parent.addClass('wandbox-renamable');
      parent.text(text);
    }
  });
}

Editor.prototype.addEditor = function(file_name, code) {
  var content_id = 'wandbox-editor-content-' + this.name_counter;
  var li = $(
    '<li class="">' +
    '  <a class="wandbox-editor-name-tab" href="#' + content_id + '" role="tab" data-toggle="tab">' +
    '    <i class="glyphicon glyphicon-file"></i>' +
    '    <span class="wandbox-renamable">' + file_name + '</span>' +
    '    <button class="glyphicon glyphicon-remove close"></button>' +
    '  </a>' +
    '</li>' +
  '');
  var content = $(
    '<div' +
    '  id="' + content_id + '"' +
    '  role="tabpanel"' +
    '  class="tab-pane"' +
    '>' +
    '</div>' +
  '');
  this.name_counter += 1;

  this._add_editor(content);
  this.setValue(content, code || '');
  $('#wandbox-editor-content').append(content);

  $('#wandbox-editor-add-tab').closest('li').before(li);

  this.setLanguage(this.getLanguage());
}

Editor.prototype.contents = function() {
  return $('#wandbox-editor-content').children();
}

Editor.prototype.editor = function(elem) {
  return elem.find('.wandbox-smart-editor').data('editor');
}

Editor.prototype.focus = function(elem) {
  this.editor(elem).focus();
}
Editor.prototype.focus_default = function() {
  this.editor($('#wandbox-editor-default')).focus();
}

Editor.prototype.getValue = function(elem) {
  return this.editor(elem).getValue();
}

Editor.prototype.setValue = function(elem, value) {
  this.editor(elem).setValue(value);
}

Editor.prototype.insertValue = function(elem, value) {
  this.editor(elem).replaceSelection(value);
}

function _get_editor_mode(lang) {
  return EDITOR_MODE_MAPPING[lang];
}

Editor.prototype.getLanguage = function() {
  return $('#wandbox-editor-content').attr('data-language');
}

Editor.prototype.setLanguage = function(lang) {
  $('#wandbox-editor-content').attr('data-language', lang);
  var self = this;
  this.contents().each(function(i) {
    self.editor($(this)).setOption('mode', _get_editor_mode(lang));
  });
}

Editor.prototype.serialize = function() {
  return {
    keybinding: $(this.settings_id).find('select.wandbox-select').val(),
    spaces_or_tab: $(this.settings_id).find('select.wandbox-spaces-or-tab').val(),
    tab_width: $(this.settings_id).find('select.wandbox-tab-width').val(),
    smart_indent: $(this.settings_id).find('input.smart-indent').prop('checked'),
  };
}

Editor.prototype.deserialize = function(settings) {
  var ekey = $(this.settings_id).find('select.wandbox-select');
  ekey.val(settings.keybinding);

  var esot = $(this.settings_id).find('select.wandbox-spaces-or-tab');
  esot.val(settings.spaces_or_tab);

  var etabw = $(this.settings_id).find('select.wandbox-tab-width');
  etabw.val(settings.tab_width);

  var esmart = $(this.settings_id).find('input.smart-indent');
  esmart.prop('checked', settings.smart_indent);

  ekey.change();
  esot.change();
  etabw.change();
  esmart.change();
}

/* result_container */

function ResultContainer(id, settings_id) {
  var self = this;

  this.id = id;
  this.settings_id = settings_id;
  this.name = 1;
  this.running = false;

  $(this.settings_id).find('input.nowrap-output-window').change(function(e) {
    var content = $(self.id + ' > .tab-content');
    $(e.target).prop('checked') ? content.addClass('nowrap') : content.removeClass('nowrap')
    if (self.result_changed)
        self.result_changed();
  });
}

ResultContainer.prototype._nav_tabs = function() {
  return $(this.id + ' .tabbable ul');
}

ResultContainer.prototype._tab_content = function() {
  return $(this.id + ' > .tab-content');
}

ResultContainer.prototype._next_name = function() {
  return this.name++;
}

ResultContainer.prototype._post_init = function(compiler, code, codes, name) {
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

  var result_window = new ResultWindow('#' + id_name, name);
  result_window.onfinish = function() {
    self.running = false;
    if (self.onfinish)
      self.onfinish();
  };
  return result_window;
}

ResultContainer.prototype.post_code = function(compiler, code, codes, stdin) {
  if (this.running) {
    return;
  }
  this.running = true;

  var result_window = this._post_init(compiler, code, codes, this._next_name());
  result_window.post_code(compiler, code, codes, stdin);
}

ResultContainer.prototype.set_code = function(compiler, code, codes, stdin, outputs) {
  var result_window = this._post_init(compiler, code, codes, 'permlink');
  result_window.set_code(compiler, code, codes, stdin, outputs);
}

ResultContainer.prototype.serialize = function() {
  return {
    nowrap: $(this.settings_id).find('input.nowrap-output-window').prop('checked'),
  };
}

ResultContainer.prototype.deserialize = function(settings) {
  var enowrap = $(this.settings_id).find('input.nowrap-output-window');
  enowrap.prop('checked', settings.nowrap);

  enowrap.change();
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

function ResultWindow(id, name_counter) {
  this.id = id;
  this.name_counter = name_counter
  $(this.id).empty();
  var permlink = $('<div class="permlink"></div>');
  var code = $('<div class="wandbox-code-window"></div>');
  var output = $('<div class="output-window"></div>');
  $(this.id).append(permlink);
  $(this.id).append(code);
  $(this.id).append(output);
}

ResultWindow.prototype._permlink = function() {
  return $(this.id).find('.permlink');
}
ResultWindow.prototype._code_window = function() {
  return $(this.id).find('.wandbox-code-window');
}
ResultWindow.prototype._output_window = function() {
  return $(this.id).find('.output-window');
}

ResultWindow.prototype.permlink = function(compiler_info, code, codes, stdin, outputs) {
  var self = this;

  var a;
  if (LOGIN_AVATAR_URL == null) {
    a = $('<a href="#" class="btn btn-default"><span class="glyphicon glyphicon-share"></span> Share</a>');
  } else {
    a = $('<a href="#" class="btn btn-default"><span class="glyphicon glyphicon-share"></span> Share by <img class="wandbox-small-avatar" src="' + LOGIN_AVATAR_URL + '" alt="' + LOGIN_NAME + '"></img></a>');
  }
  a.appendTo(this._permlink());
  a.click(function(event) {
    event.preventDefault();
    self.post_permlink(compiler_info, code, codes, stdin, outputs);
  });
}

ResultWindow.prototype.post_permlink = function(compiler_info, code, codes, stdin, outputs) {
  var self = this;

  var pm = this._permlink().find('a');
  if (pm.hasClass('disable')) return;
  pm.addClass('disable');

  outputs = $.map(outputs, function(e) {
    return { type: e.type, data: e.output };
  });

  var data = {
    compiler: compiler_info.selected_compiler,
    code: code,
    codes: codes,
    stdin: stdin,
    options: compiler_info.compile_options,
    'compiler-option-raw': compiler_info.compiler_option_raw,
    'runtime-option-raw': compiler_info.runtime_option_raw,
    results: outputs,
    login: LOGIN_NAME != null,
  };

  $.post(URL_PERMLINK, JSON.stringify(data),
    function(json) {
      if (json.permlink === undefined) {
        pm.removeClass('disable');
        return;
      }

      pm.remove();

      var url = URL_PERMLINK + '/' + json.permlink;
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

ResultWindow.prototype.code_window = function(compiler_info, code, codes, stdin) {
  var self = this;
  var header_id = 'wandbox-resultwindow-code-header-' + this.name_counter;
  var body_id = 'wandbox-resultwindow-code-body-' + this.name_counter;
  var code_window = $(
    '<div class="wandbox-code-window-code panel panel-default">' +
    '  <a' +
    '    id="' + header_id + '"' +
    '    data-toggle="collapse" href="#' + body_id + '"' +
    '    aria-expanded="true" class=""' +
    '  >' +
    '    Code' +
    '  </a>' +
    '  <div' +
    '    id="' + body_id + '"' +
    '    class="panel-collapse collapse"' +
    '  >' +
    '    <div class="panel-body">' +
    '      <div class="wandbox-resultwindow-compiler"></div>' +
    '      <div class="wandbox-resultwindow-code">' +
    '        <ul class="nav nav-tabs" role="tablist"></ul>' +
    '        <div class="tab-content"></div>' +
    '      </div>' +
    '      <div class="wandbox-resultwindow-stdin"></div>' +
    '    </div>' +
    '  </div>' +
    '</div>' +
  '')
  code_window.find('a').click(function(e) {
    $($(this).attr('href')).collapse('toggle');
  });
  var make_code_header = function(n, active, file_name) {
    var body_id = 'wandbox-resultwindow-code-body-' + self.name_counter + '-' + n;
    var li = $(
      '<li class="' + (active ? 'active' : '') + '">' +
      '  <a class="" href="#' + body_id + '" role="tab" data-toggle="tab">' +
      '    <i class="glyphicon glyphicon-file"></i>' +
      '    <span></span>' +
      '  </a>' +
      '</li>' +
    '');
    li.find('a > span').text(file_name);
    return li;
  };
  var make_code_body = function(n, active, body) {
    var id = 'wandbox-resultwindow-code-body-' + self.name_counter + '-' + n;
    var content = $(
      '<div' +
      '  id="' + id + '"' +
      '  role="tabpanel"' +
      '  class="tab-pane ' + (active ? 'active' : '') + '"' +
      '>' +
      '</div>' +
    '');
    content.append($('<pre>').text(body));
    return content;
  };
  var compiler_data = compiler_info.compiler_data;

  code_window.find('.wandbox-resultwindow-code > ul').append(make_code_header(0, true, ''));
  code_window.find('.wandbox-resultwindow-code > div').append(make_code_body(0, true, code));
  for (var i = 0; i < codes.length; i++) {
    code_window.find('.wandbox-resultwindow-code > ul').append(make_code_header(i + 1, false, codes[i].file));
    code_window.find('.wandbox-resultwindow-code > div').append(make_code_body(i + 1, false, codes[i].code));
  }
  code_window.find('.wandbox-resultwindow-stdin').append($('<pre>').text(stdin));
  code_window.find('.wandbox-resultwindow-compiler').append(compiler_data);

  this._code_window().append(code_window);
}

ResultWindow.prototype.to_compiler_info = function(compiler) {
  var selected_compiler = compiler.get_selected_compiler();
  var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).val(); }).get().join(',');
  var compiler_option_raw = compiler.get_selected_compiler_option_raw();
  var runtime_option_raw = compiler.get_selected_runtime_option_raw();

  var compiler_data = (function(compiler) {
    var data = compiler.get_selected_compiler_element().attr('data-full-name');
    return $('<div><p>' + data + '</p></div>');
  })(compiler);

  var compile_command_code = (function(compiler) {
    var command = compiler.get_selected_compiler_element().attr('data-full-name');
    var compile_options = compiler.get_checked_compile_options().map(function(n,e) { return $(e).attr('data-flags'); });
    var compiler_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_compiler_option_raw());
    var runtime_options_arguments = Compiler.prototype.raw_to_arguments(compiler.get_selected_runtime_option_raw());
    var options_arguments = compiler.get_selected_compiler_element().attr('data-options-raw') == 'compiler' ? compiler_options_arguments : runtime_options_arguments;

    var compile_command = '$ ' + command + ' ' + compile_options.get().join(' ') + ' ' + options_arguments.join(' ');
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

ResultWindow.prototype.post_code = function(compiler, code, codes, stdin) {
  var self = this;

  var compiler_info = this.to_compiler_info(compiler);

  this.code_window(compiler_info, code, codes, stdin);

  src = new PostEventSource(URL_COMPILE, {
      compiler: compiler_info.selected_compiler,
      code: code,
      codes: codes,
      stdin: stdin,
      options: compiler_info.compile_options,
      'compiler-option-raw': compiler_info.compiler_option_raw,
      'runtime-option-raw': compiler_info.runtime_option_raw,
  });

  var finalize = function() {
    src.close();

    var outputs = self._output_window().find('pre').map(function(n,e) {
        return { 'type': $(e).attr('data-type'), 'output': $(e).attr('data-text') };
    });
    self.permlink(compiler_info, code, codes, stdin, outputs);

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
    var p =
        (is_message(data.type) &&
         preview_paragraph &&
         preview_paragraph.hasClass(data.type))
            ? preview_paragraph
            : $('<pre>').addClass(data.type).attr('data-type', data.type).attr('data-text', '').appendTo(output);
    p.attr('data-text', p.attr('data-text') + data.message);
    var ansi_up = new AnsiUp();
    p.html(ansi_up.ansi_to_html(p.attr('data-text')));
    preview_paragraph = p;

    if (data.type == 'Control' && data.message == 'Finish') {
        finalize();
    }
  };
  src.onreceived = function() {
    var output = self._output_window()

    output[0].scrollTop = output[0].scrollHeight;
  };


  src.onerror = function() {
    finalize();
  }
}

ResultWindow.prototype.set_code = function(compiler, code, codes, stdin, outputs) {
  var self = this;
  var compiler_info = self.to_compiler_info(compiler);
  this.code_window(compiler_info, code, codes, stdin);
  $.each(outputs, function(n,e) {
    var type = e.type;
    var output = e.data;
    var ansi_up = new AnsiUp();
    $('<pre>').addClass(type)
              .attr('data-type', type)
              .html(ansi_up.ansi_to_html(output))
              .appendTo(self._output_window());
  });
}

/* Stdin */

function Stdin(stdin_id, ctrl_enter) {
  this.stdin_id = stdin_id;

  var init_editor = function(es) {
    es.map(function(n,e) {
      if ($(e).children().size() != 0)
        return;

       var editor = CodeMirror(e, {
         viewportMargin: Infinity,
         smartIndent: false,
         extraKeys: {
           'Ctrl-Enter': ctrl_enter,
           'Cmd-Enter': ctrl_enter,
         },
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
  if (value.length != 0) {
    $('#wandbox-stdin-body').addClass('in');
  }
  this._stdin(value);
}
