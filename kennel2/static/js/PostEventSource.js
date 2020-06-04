;(function (global) {

var reTrim = /^(\s|\u00A0)+|(\s|\u00A0)+$/g;

var PostEventSource = function (url, obj) {
  var eventsource = this,
      lastEventId = null,
      cache = '',
      data = [];

  if (!url || typeof url != 'string') {
    throw new SyntaxError('Not enough arguments');
  }

  this.readyState = this.CONNECTING;
  this._xhr = null;

  function poll() {
    try { // force hiding of the error message... insane?
      if (eventsource.readyState == eventsource.CLOSED) return;

      // NOTE: IE7 and upwards support
      var xhr = new XMLHttpRequest();
      xhr.open('POST', url, true);
      xhr.setRequestHeader('Content-Type', 'application/json');
      xhr.setRequestHeader('Accept', 'text/event-stream');
      xhr.setRequestHeader('Cache-Control', 'no-cache');
      // we must make use of this on the server side if we're working with Android - because they don't trigger
      // readychange until the server connection is closed
      xhr.setRequestHeader('X-Requested-With', 'XMLHttpRequest');

      if (lastEventId != null) xhr.setRequestHeader('Last-Event-ID', lastEventId);
      cache = '';

      xhr.timeout = 50000;
      xhr.onreadystatechange = function () {
        if (this.readyState == 3 || (this.readyState == 4 && this.status == 200)) {
          // on success
          if (eventsource.readyState == eventsource.CONNECTING) {
            eventsource.readyState = eventsource.OPEN;
            eventsource.dispatchEvent('open', { type: 'open' });
          }

          var responseText = '';
          try {
            responseText = this.responseText || '';
          } catch (e) {}

          var index = responseText.lastIndexOf("\n")
          responseText = responseText.substr(0, index + 1);
          // process this.responseText
          var parts = responseText.substr(cache.length).split("\n"),
              eventType = 'message',
              i = 0,
              line = '';

          cache = responseText;

          // TODO handle 'event' (for buffer name), retry
          for (; i < parts.length - 1; i++) {
            line = parts[i];
            if (line.indexOf('event') == 0) {
              eventType = line.replace(/event:? ?/, '');
            } else if (line.indexOf('retry') == 0) {
              retry = parseInt(line.replace(/retry:? ?/, ''));
              if(!isNaN(retry)) { interval = retry; }
            } else if (line.indexOf('data') == 0) {
              data.push(line.replace(/data:? ?/, ''));
            } else if (line.indexOf('id:') == 0) {
              lastEventId = line.replace(/id:? ?/, '');
            } else if (line.indexOf('id') == 0) { // this resets the id
              lastEventId = null;
            } else if (line == '') {
              if (data.length) {
                var event = new MessageEvent(data.join('\n'), eventsource.url, lastEventId);
                eventsource.dispatchEvent(eventType, event);
                data = [];
                eventType = 'message';
              }
            }
          }
          eventsource.dispatchEvent('received', null);

          // don't need to poll again, because we're long-loading
        } else if (eventsource.readyState !== eventsource.CLOSED) {
          if (this.readyState == 4) { // and some other status
            // dispatch error
            eventsource.readyState = eventsource.ERROR;
            eventsource.dispatchEvent('error', { type: 'error' });
          } else if (this.readyState == 0) { // likely aborted
            eventsource.readyState = eventsource.ERROR;
          } else {
          }
        }
      };

      xhr.send(JSON.stringify(obj));

      setTimeout(function () {
        if (true || xhr.readyState == 3) xhr.abort();
      }, xhr.timeout);

      eventsource._xhr = xhr;

    } catch (e) { // in an attempt to silence the errors
      eventsource.dispatchEvent('error', { type: 'error', data: e.message }); // ???
    }
  };

  poll(); // init now
};

PostEventSource.prototype = {
  close: function () {
    // closes the connection - disabling the polling
    this.readyState = this.CLOSED;
    this._xhr.abort();
  },
  CONNECTING: 0,
  OPEN: 1,
  CLOSED: 2,
  ERROR: 3,
  dispatchEvent: function (type, event) {
    var handlers = this['_' + type + 'Handlers'];
    if (handlers) {
      for (var i = 0; i < handlers.length; i++) {
        handlers[i].call(this, event);
      }
    }

    if (this['on' + type]) {
      this['on' + type].call(this, event);
    }
  },
  addEventListener: function (type, handler) {
    if (!this['_' + type + 'Handlers']) {
      this['_' + type + 'Handlers'] = [];
    }

    this['_' + type + 'Handlers'].push(handler);
  },
  removeEventListener: function (type, handler) {
    var handlers = this['_' + type + 'Handlers'];
    if (!handlers) {
      return;
    }
    for (var i = handlers.length - 1; i >= 0; --i) {
      if (handlers[i] === handler) {
        handlers.splice(i, 1);
        break;
      }
    }
  },
  onerror: null,
  onmessage: null,
  onopen: null,
  readyState: 0,
};

var MessageEvent = function (data, origin, lastEventId) {
  this.data = data;
  this.origin = origin;
  this.lastEventId = lastEventId || '';
};

MessageEvent.prototype = {
  data: null,
  type: 'message',
  lastEventId: '',
  origin: ''
};

if ('module' in global) module.exports = PostEventSource;
global.PostEventSource = PostEventSource;

})(this);
