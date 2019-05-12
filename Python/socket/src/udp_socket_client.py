# coding:utf-8
import socket

# 创建实例
sk = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
# 定义绑定的ip和端口
ip_port = ('127.0.0.1', 8888)
# 循环数据的输入
while True:
    # 手动输入需要发送的消息
    msg_input = input('请输入发送的消息：')
    # 退出循环的条件
    if msg_input == 'exit':
        break
    # 数据发送
    sk.sendto(msg_input.encode(), ip_port)

# 发送关闭信息
sk.close()
    
