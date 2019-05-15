# snl
>snl grammar analyzer  
>采用LL(1)分析法的snl词法语法分析器。

## 说明

封装成了一个C++类SnlGrammarAnalyser，包含头文件之后就可以用了。  
main函数中打开示例代码是用的相对路径，如果你运行时候发现路径不一样的话请自行修正。  

## 公开函数

```cpp
static void init()
```
进行初始化，求解First、Follow和Predict集。

---
```cpp
int parse(const char *code)
```
解析给定的代码，返回值0为分析成功，非0为出错。
>(解析前需进行初始化)

---
```cpp
string getError()
```
如果解析出错可以调用这个获取出错信息。
>(只写了4种错误，其中3种词法错误，一种语法错误)

---
```cpp
vector<TokenSymbol> getTokenList()
```
如果解析成功可以调用这个获取TokenList。
>(该TokenList中不包含注释，TokenSymbol为自定义的结构体，定义详情请参见代码)

---
```cpp
string getTree()
```
如果解析成功可以调用这个获取
[The DOT Language](https://graphviz.gitlab.io/_pages/doc/info/lang.html)
格式的语法树。
得到的语法树可以在类似
[Graphviz Online](https://dreampuf.github.io/GraphvizOnline/)
这样的网站上在线生成图片。

## 致谢

snl示例代码来自[SNL-Compiler项目](https://github.com/YouthLin/SNL-Compiler)中的随书光盘。

## 协议
[The GNU General Public License v3.0](https://github.com/code4lala/snl/blob/master/LICENSE)
