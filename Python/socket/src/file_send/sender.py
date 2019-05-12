import socket

# 实例化门课
sk = socket.socket()
# 定义IP和端口
ip_port = ('127.0.0.1', 9999)
# 服务器连接
sk.connect(ip_port)

# 文件上传
# 打开文件
with open('data.txt', 'rb') as f:
    # 按每一段分割文件
    for d in f:
        # 数据上传
        sk.send(d)

# 给服务器端发送结束信号
sk.send('quit'.encode())
