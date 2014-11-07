rash_socks
==========

带强制超时功能的socks4代理服务器，内部业务使用。

运维指南
服务的启动

在安装好的ebin目录下运行rash_socks_start脚本即可以daemon方式运行。

进程的PID写在：（尚未完成）

    var/pid

 
配置文件说明：

配置文件是一个文本文件，内容是一个erlang的list。样例如下：

  [   {port, 10800},
      {addr, "0.0.0.0"},
      {admin_port, 8972},
      {shortlist_size, 256},
      {log_file, "var/log"},
      {log_level, log_info},
      {servers, [
            % {{"IP_Prefix", Port}, {Connect_timeout, Receive_timeout, Send_timeout}},
            % Timeouts are all in ms.
            {{"10.210.74.190", 80}, {1000, 1000, 1000}},
            {{"10.75.12.60",80}, {2000, 1000, 1000}},
            {{"0.0.0.0/0", '*'}, {2000, 0, 0}} % Default Values
      ]}
  ].

 

 
解释：

{port, 10800}：代理端口打开在10800端口
{addr, "0.0.0.0"}：代理端口绑定到地址0.0.0.0
{admin_port, 8972}：实时监控端口开在8972端口
{shortlist_size, 256}：用于记录最近连接状况的列表长度是256，超过这个数量之后，新记录会把最老的记录冲掉
{log_file, "var/log"}： 用于定义日志路径。如写相对路径，则基于config.mk里面定义的PREFIX计算绝对路径；如写绝对路径，则不进行处理
{log_level, log_info}：用于过滤低级别的日志，级别小于指定级别的日志不予记录。
      级别的定义是： log_debug < log_info < log_notice < log_warning < log_error < log_critical
{servers, [{某IP:Port的超时定义}, ...] }：用于定义不同IP:Port的超时值，格式如下：
  {{"地址前缀", 端口}, {连接超时, 接收超时, 发送超时}}
地址前缀：字符列表（不要忘了双引号！），使用常见的CIDR前缀格式定义，如：10.0.0.0/24，如果忽略前缀长度（如10.210.213.229），则前缀长度为32（精确匹配）。

端口：整数（不要加任何引号！）。

三个超时：整数（不要加任何引号！），单位都是毫秒，0代表不限制。注意：长连接场合的接受和发送超时一般设成0。

 
注意事项：

这个配置文件本质上是一段erlang代码，这段代码的值就是配置文件的内容。熟悉Erlang语言的人可以自行发挥，如使用comprehension等技巧。

如果不熟悉Erlang语言的话，只要參照记住下面几个要点：

    大小写敏感、空白不敏感
    最后的句点（.）不能省略
    IP地址和地址前缀外面的双引号（"）不能省略
    中括号和大括号不能混淆
    百分号（%）到行尾的内容为注解

 
日志： 

日志以文本文件的形式存放在

    var/log

 
实时监控接口： 

是一个http接口：

    curl http://server_ip:admin_port/status?server=*

即可查看当前所有连接的状况。
注意：回应是纯文本，不含http头部，请按照http 0.9标准处理。
