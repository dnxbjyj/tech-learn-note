# coding:utf-8
import socket
import random

#  创建实例
sk = socket.socket()
# 定义绑定的IP和端口
ip_port = ('127.0.0.1', 8888)
# 绑定监听
sk.bind(ip_port)
# 设置最大连接数
sk.listen(5)

# 不断循环，不断接收数据
while True:
    # 提示信息
    print('正在进行等待接收数据......')
    # 接收数据
    conn, address = sk.accept()
    # 定义信息
    msg = '连接成功！'
    # 返回信息，注意：在Python 3.x以上，网络数据的发送接收都是byte类型，如果发送的数据是str类型的，则需要编码
    conn.send(msg.encode())

    # 不断接收客户端发来的消息
    while True:
        # 接收客户端的消息，每次读取缓冲区1024字节的数据
        data = conn.recv(1024)
        # 打印数据
        print(data.decode())
        # 如果接收到退出指令，则退出
        if data == b'exit':
            break
        # 处理客户端的数据（把客户端数据原样返回）
        conn.send(data)
        # 发送一个随机数
        conn.send(str(random.randint(1,1000)).encode())
    # 主动关闭连接
    conn.close()
