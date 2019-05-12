# coding:Utf-8
import socketserver
import random

# 定义一个 类
class MyServer(socketserver.BaseRequestHandler):
    # 如果handle方法出现报错，则会跳过
    # setup和finish方法无论如何都会执行
    # 首先执行setup
    def setup(self):
        pass

    # 然后执行handle
    def handle(self):
        # 定义连接变量
        conn = self.request
        # 发送消息定义
        msg = "Hello World!"
        # 消息发送
        conn.send(msg.encode())
        # 进入循环，不断接收客户端消息
        while True:
            # 接收客户端消息
            data = conn.recv(1024)
            # 打印消息
            print(data.decode())
            # 循环退出条件
            if data == b'exit':
                break

            # 返回接收到的消息，同时发送随机数
            conn.send(data)
            conn.send(str(random.randint(1, 1000)).encode())
        conn.close()

    # 最后执行finish
    def finish(self):
        pass


if __name__ == '__main__':
    # 创建多线程实例
    server = socketserver.ThreadingTCPServer(('127.0.0.1', 8888), MyServer)
    # 开启异步多线程，等待连接
    server.serve_forever()
