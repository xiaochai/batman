import socket,sys,time
def server():
    s = socket.socket()
    s.bind(("127.0.0.1", 8888))
    s.listen(5) # 这个参数是backlog
    while True:
        c, addr = s.accept()
        print('Got connection from ', addr)
        c.sendall(b'Thank you for connecting')
        c.close()

def client():
    s = socket.socket()
    s.connect(("127.0.0.1", 8888))
    print(s.recv(1024))

def iclient():
    s = socket.socket()
    s.connect(("127.0.0.1", 8888))
    while True:
        i = input("enter:")
        s.sendall( bytes(i, "utf8"))


from socketserver import *
class Handler(StreamRequestHandler):
    def handle(self):
        addr = self.request.getpeername()
        print('Got connection from', addr)
        time.sleep(5)
        self.wfile.write(b'Thank you for connecting')

# 使用socketserver来创建服务端
def sServer():
    server = TCPServer(('127.0.0.1', 8888), Handler)
    server.serve_forever()

# 通过每来一个请求就fork进程的方式来同时处理多个请求
# 在client请求时，可以使用ps -ef| grep python| grep server来查看新起的进程
def forkingServer():
    class Server(ForkingMixIn, TCPServer): pass
    server = Server(('127.0.0.1', 8888), Handler)
    server.serve_forever()

# 通过每来一个请求创建一个线程的方式来同时处理多个请求
def threadServer():
    class Server(ThreadingMixIn, TCPServer): pass
    server = Server(('127.0.0.1', 8888), Handler)
    server.serve_forever()

import select
# 这个例子中使用select来同时保持多个连接，并且可以随时响应任意连接的数据写入
def selectServer():
    s = socket.socket()
    s.bind(("127.0.0.1", 8888))
    s.listen(5) # 这个参数是backlog
    inputs = [s]
    while True:
        rs, ws, es = select.select(inputs, [], [])
        for r in rs:
            if r is s:
                c, addr = s.accept()
                print('Got connection from ', addr)
                inputs.append(c)
            else:
                try:
                    data = r.recv(1024)
                    disconnected = not data
                except socket.error:
                    disconnected = True
                if disconnected:
                    print(r.getpeername(), "disconnected")
                    inputs.remove(r)
                else:
                    print("from ", r.getpeername(), ":", data)


from twisted.internet import reactor
from twisted.internet.protocol import Protocol, Factory 

def twistedServer():
    class SimpleLogger(Protocol):
        def connectionMade(self):
            print('Got connection from', self.transport.client)
        def connectionLost(self, reason): 
            print(self.transport.client, 'disconnected')
        def dataReceived(self, data): 
            print("from ", self.transport.client, ":", data)
    factory = Factory()
    factory.protocol = SimpleLogger
    reactor.listenTCP(8888, factory)
    reactor.run()


# 一个简单地只能每一次处理一个请求的server和client示例
if len(sys.argv) >= 2:
    if sys.argv[1] == "server":
        server()
    elif sys.argv[1] == "sserver":
        sServer()
    elif sys.argv[1] == "forkingserver":
        forkingServer()
    elif sys.argv[1] == "threadserver":
        threadServer()
    elif sys.argv[1] == "selectserver":
        selectServer()
    elif sys.argv[1] == "twistedserver":
        twistedServer()

    elif sys.argv[1] == "iclient":
        iclient()
else:
    client()