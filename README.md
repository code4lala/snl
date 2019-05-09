# snl
snl grammar analyzer

全部都在 [main.cpp](https://github.com/code4lala/snl/blob/master/main.cpp) 里面了。

所有的非终极符，终极符，列了出来。

所有的推导式，列了出来。

求解First集，Follow集，Predict集，几个函数。

采用LL(1)分析法分析的，也是个函数。

要是跑起来输出有error试试改一下main函数中的打开文件的文件路径，我在CLion中是这个相对路径，其他环境可能不是。

[examples文件夹](https://github.com/code4lala/snl/tree/master/examples)中的几个测试样例都能通过。
