# coding:utf-8
import socket

# 实例初始化
client = socket.socket()
# 访问的服务器的IP和端口
ip_port = ('127.0.0.1', 8888)
# 连接主机
client.connect(ip_port)
# 定义一个循环，不断向服务器端发送消息
while True:
    # 接收主机数据，每次接收缓冲区的数据为1024字节
    data = client.recv(1024)
    # 打印接收的数据，注：如果使用的是Python 3.x，则需要解码
    print(data.decode())

    # 输入发送的消息
    msg_input = input('请输入需要发送的消息：')
    # 消息发送
    client.send(msg_input.encode())
    if msg_input == 'exit':
        break
    # 接收主机数据，每次接收缓冲区的数据为1024字节
    data = client.recv(1024)
    # 打印接收的数据，注：如果使用的是Python 3.x，则需要解码
    print(data.decode())
