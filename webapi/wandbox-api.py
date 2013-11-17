import sys, socket, re, quopri, getopt
from flask import Flask, request, Response, json, redirect
from werkzeug.serving import BaseWSGIServer

app = Flask(__name__)
cattleshed = ('localhost', 2012)
listen = ('localhost', 2009)

@app.route("/")
def root():
    return redirect('http://melpon.org/wandbox')

def get_lines(sock):
    data = ''
    r = re.compile(r'^[\r\n]*(\w+) +(\d+):')
    while True:
        data += sock.recv(4096)
        m = r.match(data)
        if not m: break;
        l = m.end() + int(m.group(2))
        while len(data) < l:
            data += sock.recv(4096)
        yield (m.group(1),quopri.decodestring(data[m.end():l]))
        data = data[l+1:]

def put_line(sock, command, data):
    data = quopri.encodestring(data)
    data = data.replace('\n', '=0a=\n').replace('==0a=\n', '=\n')
    sock.sendall(command + ' ' + str(len(data)) + ':' + data + '\n')

@app.route("/list.json", methods=["GET"])
def list_compiliers():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(cattleshed)
    put_line(s, 'Version', '')
    for d in get_lines(s): return d[1]

@app.route("/compile.json", methods=["GET", "POST"])
def get_compile():
    print(request.__dict__)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(cattleshed)
    put_line(s, 'Control', 'compiler=' + request.values['compiler'])
    put_line(s, 'CompilerOption', request.values['options'])
    put_line(s, 'Source', request.values['source'])
    put_line(s, 'Control', 'run')
    def l(s):
        exitcode = None
        exitsignal = None
        for d in get_lines(s):
            cmd = ''
            data = {}
            if d[0] == 'Control' and d[1] == 'Finish':
                cmd = 'finish'
                if exitcode is not None: data['status'] = exitcode
                if exitsignal is not None: data['signal'] = exitsignal
            if d[0] == 'ExitCode':
                exitcode = int(d[1])
            if d[0] == 'Signal':
                exitsignal = d[1]
            if d[0] == 'CompilerMessageS':
                cmd = 'compiler_message'
                data['stdout'] = d[1]
            if d[0] == 'CompilerMessageE':
                cmd = 'compiler_message'
                data['stderr'] = d[1]
            if d[0] == 'StdOut':
                cmd = 'program_output'
                data['stdout'] = d[1]
            if d[0] == 'StdErr':
                cmd = 'program_output'
                data['stderr'] = d[1]
            data['command'] = cmd
            x = json.dumps(data)
            if cmd: yield hex(len(x))[2:] + '\r\n' + x + '\r\n'
        yield "0\r\n\r\n"
    return Response(l(s), headers={'Transfer-Encoding':'chunked'})

if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], 'c:dl:', ['cattleshed=', 'debug', 'listen='])
    for (o, a) in opts:
        if o in ('-c', '--cattleshed'):
            x = a.split(':')
            cattleshed = (x[0], int(x[1]))
        elif o in ('-d', '--debug'):
            app.debug = True
        elif o in ('-l', '--listen'):
            x = a.split(':')
            listen = (x[0], int(x[1]))
    f = BaseWSGIServer.serve_forever
    def xx(self):
        self.RequestHandlerClass.protocol_version = "HTTP/1.1"
        f(self)
    BaseWSGIServer.serve_forever = xx
    app.run(host=listen[0], port=listen[1])

