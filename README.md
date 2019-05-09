# snl
snl grammar analyzer

编译原理课设

采用LL(1)分析法的snl词法语法分析器。

封装成了一个C++类SnlGrammarAnalyser，包含头文件之后就可以用了。

三个公开函数
- 一个`void init()`进行初始化，求解First、Follow和Predict集。
- 一个`int parse(const char *code)`解析给定的代码，返回值0为分析成功，非0为出错。
- 如果出错了可以调用`string getError()`查看出错信息。（一共只写了4种错误，详见[代码](https://github.com/code4lala/snl/blob/master/SnlGrammarAnalyser.h))

main函数中打开示例代码是用的相对路径，如果你运行时候发现路径不一样的话请自行修正。

snl示例代码来自[SNL-Compiler项目](https://github.com/YouthLin/SNL-Compiler)中的随书光盘。