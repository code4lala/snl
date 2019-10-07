#ifndef SNL_SNLGRAMMARANALYSER_H
#define SNL_SNLGRAMMARANALYSER_H

/***
 * 关键字不区分大小写 ID区分大小写 snl不支持字符串 因此不解析字符串
 * 修正了推导式中的错误 采用LL(1)分析法 求得了First Follow和Predict集
 */

#include <string>
#include <iostream>
#include <iomanip>
#include <list>
#include <cstring>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <limits>
#include <sstream>
#include <queue>

namespace snl {

#if _MSC_VER >= 1910
#define strncpy(a,b,c) strncpy_s(a,b,c)
#endif

    using std::numeric_limits;
    using std::transform;
    using std::cout;
    using std::pair;
    using std::set;
    using std::map;
    using std::vector;
    using std::endl;
    using std::string;
    using std::ostream;
    using std::setw;
    using std::list;
    using std::cerr;
    using std::to_string;
    using std::ofstream;
    using std::stringstream;
    using std::queue;

    // 返回给上层的错误值
    enum {
        E_COMMENT_NOT_TERMINATED = 0x10,
        E_ONLY_COLON_BUT_NO_EQUAL,
        E_UNEXPECTED_CHAR,

        E_GRAMMAR,

        E_UNKNOWN
    };

    enum {
        ERROR_TWO_CHAR = -2,
        ERROR_COMMENT = -3,
        ERROR_UNEXPECTED_CHAR = -4
    };

    enum {
        // ID 或 关键字
                TYPE_IDENTIFIER = 1,
        // INTC
                TYPE_UNSIGNED_INT,
        // 单字符分界符
                TYPE_SINGLE_CHAR,
        // 双字符分界符中的 :=
                TYPE_EQUAL,
        // 注释
                TYPE_COMMENT,
        // 双字符分界符中的 ..
                TYPE_ARRAY,
        // 空白
                TYPE_WHITESPACE
    };

    extern int global_line_number;

    struct Word {
        int type;
        string v;
        int line;

        explicit Word(const string &s, int i) : type(i), v(s), line(global_line_number) {}

        static string getType(int i) {
            switch (i) {
                case TYPE_IDENTIFIER:
                    return "___ID___";
                case TYPE_UNSIGNED_INT:
                    return "UNSIGNED";
                case TYPE_SINGLE_CHAR:
                    return "ONE_CHAR";
                case TYPE_EQUAL:
                    return "__EQUAL_";
                case TYPE_COMMENT:
                    return "COMMENT_";
                case TYPE_ARRAY:
                    return "__ARRAY_";
                case TYPE_WHITESPACE:
                    return "__WHITE_";
                default:
                    return "UNKNOWN_";
            }
        }

        friend ostream &operator<<(ostream &os, const Word &word) {
            os << "[" << getType(word.type) << "]" << setw(4) << word.line << "{" << word.v << "}";
            return os;
        }
    };

    struct Symbol {
        string name;
        bool terminate;
        string v;

        explicit Symbol(const string &name, bool terminate) : name(name), terminate(terminate) { v = ""; }

        explicit Symbol(const string &name, bool terminate, const string &v) : name(name), terminate(terminate),
                                                                               v(v) {}

        friend ostream &operator<<(ostream &os, const Symbol &symbol) {
            os << setw(10) << symbol.name << " " << (symbol.terminate ? "T" : "N");
            if (symbol.v != symbol.name)
                os << " " << setw(10) << symbol.v;
            return os;
        }

        bool operator==(const Symbol &rhs) const {
            return name == rhs.name &&
                   terminate == rhs.terminate &&
                   v == rhs.v;
        }

        bool operator!=(const Symbol &rhs) const {
            return !(rhs == *this);
        }

        bool operator<(const Symbol &rhs) const {
            return name < rhs.name;
        }

        bool operator>(const Symbol &rhs) const {
            return rhs < *this;
        }

        bool operator<=(const Symbol &rhs) const {
            return !(rhs < *this);
        }

        bool operator>=(const Symbol &rhs) const {
            return !(*this < rhs);
        }
    };

    // 终极符 空串
    Symbol& Epsilon();

    // 终极符 单字符分界符
    Symbol& Plus();//加号 +
    Symbol& Sub();//减号 -
    Symbol& Mul();//乘号 *
    Symbol& Div();//除号 /
    Symbol& LessThan();//小于号 <
    Symbol& Equal();// 等于 =
    Symbol& LeftParenthesis(); // 左小括号 (
    Symbol& RightParenthesis(); // 右小括号 )
    Symbol& LeftBracket();//左中括号 [
    Symbol& RightBracket();//右中括号 ]
    Symbol& Dot();//单字符分界符 . 一个点 用于结构体
    Symbol& Semicolon();// 分号 ;
    Symbol& Comma();// 逗号 ,

    // 终极符 双字符分界符
    Symbol& TwoDots();//数组下标分隔符 两个点 ..
    Symbol& ColonEqual();// 赋值符号 :=

    // 终极符 #
    Symbol& Sharp();//不出现在First集中，出现在Follow集和Predict集中

    // 终极符 关键字
    Symbol& ID();
    Symbol& PROCEDURE();// keyword procedure
    Symbol& PROGRAM(); // keyword program
    Symbol& TYPE(); // keyword type
    Symbol& CHAR();// keyword char
    Symbol& INTEGER();// keyword integer
    Symbol& ARRAY();// keyword array
    Symbol& OF();// keyword of
    Symbol& INTC();// keyword intc
    Symbol& RECORD();// keyword record
    Symbol& END();// keyword end
    Symbol& VAR();// keyword var
    Symbol& BEGIN(); // keyword begin
    Symbol& IF();// keyword if
    Symbol& THEN();// keyword then
    Symbol& ELSE();// keyword else
    Symbol& FI();// keyword fi
    Symbol& WHILE();//keyword while
    Symbol& DO();// keyword do
    Symbol& ENDWH();//keyword endwh
    Symbol& READ();//keyword read
    Symbol& WRITE();//keyword write
    Symbol& RETURN();//keyword return

    // 非终极符
    Symbol& Program();
    Symbol& ProgramHead();
    Symbol& DeclarePart();
    Symbol& ProgramBody();
    Symbol& ProcDecPart();
    Symbol& TypeDec();
    Symbol& TypeDecList();
    Symbol& TypeId();
    Symbol& TypeName();
    Symbol& TypeDecMore();
    Symbol& BaseType();
    Symbol& StructureType();
    Symbol& ArrayType();
    Symbol& RecType();
    Symbol& FieldDecList();
    Symbol& IdList();
    Symbol& FieldDecMore();
    Symbol& IdMore();
    Symbol& VarDec();
    Symbol& VarDecList();
    Symbol& VarIdList();
    Symbol& VarDecMore();
    Symbol& VarIdMore();
    Symbol& ProcDec();
    Symbol& ParamList();
    Symbol& ProcBody();
    Symbol& ProcDeclaration();
    Symbol& ParamDecList();
    Symbol& Param();
    Symbol& ParamMore();
    Symbol& FormList();
    Symbol& FidMore();
    Symbol& StmList();
    Symbol& Stm();
    Symbol& StmMore();
    Symbol& ConditionalStm();
    Symbol& LoopStm();
    Symbol& InputStm();
    Symbol& OutputStm();
    Symbol& ReturnStm();
    Symbol& AssCall();
    Symbol& AssignmentRest();
    Symbol& CallStmRest();
    Symbol& VariMore();
    Symbol& Exp();
    Symbol& ActParamList();
    Symbol& ActParamMore();
    Symbol& OtherExp();
    Symbol& CmpOp();
    Symbol& Term();
    Symbol& OtherTerm();
    Symbol& AddOp();
    Symbol& Factor();
    Symbol& OtherFactor();
    Symbol& MultOp();
    Symbol& Variable();
    Symbol& FieldVar();
    Symbol& FieldVarMore();
    Symbol& TypeDeclaration();
    Symbol& VarDeclaration();
    Symbol& SimpleExp();

    extern char buf[4096];

    // 推导 的结构体 比如 F->(E)
    struct Derivation {
        Symbol head;
        vector<Symbol> symbolVector;

        explicit Derivation(const Symbol &head, const vector<Symbol> &symbolVector) : head(head),
                                                                                      symbolVector(symbolVector) {}

        bool operator==(const Derivation &rhs) const {
            return head == rhs.head &&
                   symbolVector == rhs.symbolVector;
        }

        bool operator!=(const Derivation &rhs) const {
            return !(rhs == *this);
        }

        friend ostream &operator<<(ostream &os, const Derivation &derivation) {
            os << "[" << derivation.head.name << "] -> [ ";
            for (const Symbol &s : derivation.symbolVector) {
                os << s.name << " ";
            }
            os << "]";
            return os;
        }

        bool operator<(const Derivation &rhs) const {
            if (head < rhs.head)
                return true;
            if (rhs.head < head)
                return false;
            return symbolVector < rhs.symbolVector;
        }

        bool operator>(const Derivation &rhs) const {
            return rhs < *this;
        }

        bool operator<=(const Derivation &rhs) const {
            return !(rhs < *this);
        }

        bool operator>=(const Derivation &rhs) const {
            return !(*this < rhs);
        }

    };

    // 所有推导的链表
    extern list<Derivation> allDerivations;
    // First集
    extern map<Symbol, set<Symbol>> FirstSet;
    // Follow集
    extern map<Symbol, set<Symbol>> FollowSet;
    // Predict集
    extern map<Derivation, set<Symbol>> PredictSet;

    void buildInit();

    int firstSize();

    /**
     * 构造First集
     */
    void buildFirst();

    /**
     * FollowSet中set<Symbol>的所有元素个数
     * @return FollowSet中set<Symbol>的所有元素个数
     */
    int followSize();

    /**
     * 构造Follow集
     */
    void buildFollow();

    set<Symbol> getFirstBySymbol(const Symbol &s);

    /**
     * 构造Predict集
     */
    void buildPredict();

    struct TokenSymbol {
        Symbol s;
        int line;

        explicit TokenSymbol(const Symbol &s, int line) : s(s), line(line) {}

        friend ostream &operator<<(ostream &os, const TokenSymbol &symbol) {
            os << setw(4) << symbol.line << symbol.s;
            return os;
        }
    };

    string up(const string &x);

    extern bool initialized;

    struct Node {
        Symbol curr;
        vector<Node *> children;
        Node *parent;
        int id;

        explicit Node(const Symbol &s, Node *parent, int id) : curr(s), parent(parent), id(id) {}
    };

    // parse之前先init一下就可以了，如果解析出错调用getError()查看出错原因
    class SnlGrammarAnalyser {
    public:
        static void init() {
            if (initialized)return;
            buildInit();
            buildFirst();
            buildFollow();
            buildPredict();
            initialized = true;
        }

        int parse(const char *code) {
            if (!initialized)return -1;
            // 清空之前的
            words.clear();
            tokenSymbols.clear();
            vTree.clear();
            errMsg = "";
            // 先进行词法分析
            global_line_number = 1;
            while (*code != '\0') {
                int r = analysisOneWord(code);
                if (r <= 0) {
                    errMsg = "[Lexical error] line " + to_string(global_line_number) + " Current parsing \'" + *code +
                             "\'";
                    // 出错
                    switch (r) {
                        case ERROR_COMMENT:
                            errMsg += "Missing a right brace causes the comment to not close";
                            return E_COMMENT_NOT_TERMINATED;
                        case ERROR_TWO_CHAR:
                            errMsg += "There should be an equal sign after the colon but no";
                            return E_ONLY_COLON_BUT_NO_EQUAL;
                        case ERROR_UNEXPECTED_CHAR:
                            errMsg += "Encountered symbols other than snl grammar";
                            return E_UNEXPECTED_CHAR;
                    }
                    return E_UNKNOWN;
                } else {
                    code += r;
                }
            }

            convertWordsToSymbols();

            // 然后进行语法分析
            return grammarAnalysis();
        }

        string getError() { return errMsg; }

        vector<TokenSymbol> getTokenList() {
            return tokenSymbols;
        }

        string getTree() {
            if (!initialized)return "";
            if (errMsg != "")return "";
            if (vTree.size() == 0)return "";

            ////DEBUG 输出所有已知顺序的推导式
            //cout << "DEBUG 所有推导式" << endl;
            //for (const Derivation& d : vTree) {
            //	cout << d << endl;
            //}
            //cout << "DEBUG 所有推导式" << endl;

            global_id = 1;

            root = new Node(vTree[0].head, nullptr, global_id++);
            for (const Symbol &s : vTree[0].symbolVector) {
                root->children.emplace_back(new Node(s, root, global_id++));
            }
            treeIndex = 1;
            global_token_index = 0;
            for (unsigned int i = 0; i < root->children.size(); i++) {
                if (root->children[i]->curr.terminate)continue;
                dfsBuildTree(root->children[i]);
            }

            stringstream ss;//把DOT Language表示的语法树的字符串放到这里边

            ss.clear();
            ss << "digraph GrammarTree {" << endl;
            queue<Node *> q;
            q.push(root);
            while (!q.empty()) {
                Node *c = q.front();
                q.pop();

                // 该节点的样式和内容
                if (c->curr.terminate) {
                    if (c->curr == Epsilon()) {
                        ss << "\"" << c->id << "\" [shape=square; style=filled; fillcolor=cornsilk; label=\""
                           << "ε" << "\"];" << endl;
                    } else if (c->curr.name == "ID") {
                        ss << "\"" << c->id << "\" [shape=square; style=filled; fillcolor=lightpink; label=\""
                           << c->curr.v << "\"];" << endl;
                    } else {
                        ss << "\"" << c->id << "\" [shape=square; style=filled; fillcolor=chartreuse1; label=\""
                           << c->curr.v << "\"];" << endl;
                    }
                } else {
                    ss << "\"" << c->id << "\" [style=filled; fillcolor=cyan; label=\""
                       << c->curr.name << "\"];" << endl;
                }

                if (c->children.size() == 0) {
                    // 叶子结点
                    continue;
                }

                // 跟其他节点的关系
                string children = "";
                for (unsigned int i = 0; i < c->children.size(); i++) {
                    children += "\"" + to_string(c->children[i]->id) + "\"; "; // "id; "
                }
                ss << "\"" << c->id << "\" -> {" << children << "}" << endl;
                ss << "{rank=same; " << children << "}" << endl;

                // 入栈
                for (Node *nd : c->children) {
                    q.push(nd);
                }
            }
            ss << "}" << endl;
            return ss.str();
        }

    private:
        string errMsg;
        list<Word> words;
        vector<TokenSymbol> tokenSymbols;
        vector<Derivation> vTree;// 以Vector表示的语法树
        unsigned int treeIndex;// 指示vTree的下标
        Node *root;// 以链表树表示的语法树
        int global_id;//DOT Language中每个节点的id
        unsigned int global_token_index;//用于将ID转化为变量名后者函数名等
        void dfsBuildTree(Node *&parent) {
            // 传过来的参数parent一定是非终极符
            if (parent->curr.terminate) {
                cerr << "[error] dfsBuildTree invalid param" << endl;
                exit(-1);
            }
            if (treeIndex >= vTree.size()) {
                cerr << "[error] dfsBuildTree treeIndex" << endl;
                exit(-1);
            }
            // 构造子节点
            for (unsigned int i = 0; i < vTree[treeIndex].symbolVector.size(); i++) {
                parent->children.emplace_back(new Node(vTree[treeIndex].symbolVector[i], parent, global_id++));
            }
            // 构造完推导式下标就加加
            treeIndex++;
            for (unsigned int i = 0; i < parent->children.size(); i++) {
                if (parent->children[i]->curr.terminate) {
                    // 将ID转化为实际值
                    if (parent->children[i]->curr == Epsilon()) {}
                    else {
                        //cout << tokenSymbols[global_token_index] <<"\t\ttokenlist"<< endl;
                        //cout << parent->children[i]->curr << endl;
                        //cout << endl;
                        parent->children[i]->curr.v = tokenSymbols[global_token_index].s.v;
                        global_token_index++;
                    }
                    continue;// 终极符是叶节点
                }
                dfsBuildTree(parent->children[i]);
            }
        }

        /**
         * 语法分析
         */
        int grammarAnalysis() {
            // 例 分析栈 #E   输入流 i+i*i#
            list<Symbol> s;//因为栈不能遍历输出所以用list链表代替
            s.emplace_back(Sharp()); // #
            s.emplace_back(allDerivations.front().head); // 文法开始符
            tokenSymbols.emplace_back(TokenSymbol(Sharp(), numeric_limits<int>::max()));
            int i = 0;//输入流索引
            while (!s.empty()) {
                Symbol t = s.back();//栈顶元素
                Symbol r = tokenSymbols[static_cast<unsigned int>(i)].s;//输入流当前扫描符号
                if (!t.terminate) {
                    //非终极符 寻找 t->啥 的Predict集 中有输入流当前索引符号
                    bool notFound = true;
                    for (const auto &a : PredictSet) {
                        if (a.first.head != t)continue;
                        set<Symbol>::iterator f;
                        if (r.name == "ID") {
                            f = a.second.find(ID());
                        } else {
                            f = a.second.find(r);
                        }
                        if (f == a.second.end())continue;
                        // 找到了 t->a.second的Predict集中有 r

                        // 绘制语法树部分
                        vTree.emplace_back(a.first);

                        notFound = false;
                        s.pop_back();//先弹栈
                        // 然后推导式倒着入栈
                        for (auto it = a.first.symbolVector.rbegin(); it != a.first.symbolVector.rend(); it++) {
                            s.emplace_back(*it);
                        }
                        break;
                    }
                    if (notFound) {
                        // 出错
                        errMsg = "[Grammatical error] line " +
                                 to_string(tokenSymbols[static_cast<unsigned int>(i)].line) + " Current parsing \'" +
                                 r.v + "\'";
                        return E_GRAMMAR;
                        //cerr << "[error] " << __LINE__ << " " << t << " can not predict " << r << endl;
                        //cerr << "at line " << tokenSymbols[i].line << endl;
                        //exit(-1);
                    } else {
                        continue;// 继续
                    }
                } else {
                    if (t.name == "ID") {
                        // 非关键字的标识符
                        if (r.name == "ID") {
                            // 匹配了!!!
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            errMsg = "[Grammatical error] line " +
                                     to_string(tokenSymbols[static_cast<unsigned int>(i)].line) +
                                     " Current parsing \'" + r.v + "\'";
                            return E_GRAMMAR;
                            // 出错
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }
                    } else if (t == Epsilon()) {
                        // 空串，直接弹栈继续
                        s.pop_back();
                        continue;
                    } else if (t.name == "INTC") {
                        // 无符号整数
                        if (r.name == "INTC") {
                            // 匹配了!!!
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            errMsg = "[Grammatical error] line " +
                                     to_string(tokenSymbols[static_cast<unsigned int>(i)].line) +
                                     " Current parsing \'" + r.v + "\'";
                            return E_GRAMMAR;
                            // 出错
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }

                    } else {
                        // 单独的
                        if (t == r) {
                            // 匹配了!!!
                            if (t == Sharp()) {
                                // 完成了!!!
                                break;
                            }
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            // 出错
                            errMsg = "[Grammatical error] line " +
                                     to_string(tokenSymbols[static_cast<unsigned int>(i)].line) +
                                     " Current parsing \'" + r.v + "\'";
                            return E_GRAMMAR;
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }
                    }
                }
            }
            //cout << endl << "Grammar analysis completed! 0 error!" << endl;
            return 0;
        }


        void pushWord(const char *s, int l, int type) {
            strncpy(buf, s, static_cast<unsigned int>(l));
            buf[l] = '\0';
            words.emplace_back(Word(buf, type));
        }

        /**
         * 标识符
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeIdentifier(const char *s) {
            int i = 1;
            while (s[i] != '\0') {
                if ((s[i] >= 'a' && s[i] <= 'z') ||
                    (s[i] >= 'A' && s[i] <= 'Z') ||
                    (s[i] >= '0' && s[i] <= '9')) {
                    i++;
                } else {
                    break;
                }
            }
            pushWord(s, i, TYPE_IDENTIFIER);
            return i;
        }

        /**
         * 简单数字
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeNum(const char *s) {
            int i = 1;
            while (s[i] != '\0') {
                if (s[i] >= '0' && s[i] <= '9') {
                    i++;
                } else {
                    break;
                }
            }
            pushWord(s, i, TYPE_UNSIGNED_INT);
            return i;
        }

        /**
         * 除了.以外的单字符分界符
         * @param s 待处理字符串
         * @return 1
         */
        int takeOneChar(const char *s) {
            pushWord(s, 1, TYPE_SINGLE_CHAR);
            return 1;
        }

        /**
         * 双字符分界符 :=
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 2 或 失败原因
         */
        int takeTwoChar(const char *s) {
            if (s[1] == '=') {
                pushWord(s, 2, TYPE_EQUAL);
                return 2;
            }
            return ERROR_TWO_CHAR;
        }

        /**
         * 注释 {comment}
         * @param s 待处理字符串
         * @return 处理成功的字符串长度(大于等于2) 或 失败原因
         */
        int takeComment(const char *s) {
            int i = 1;
            while (s[i] != '\0') {
                if (s[i] == '\n')global_line_number++;
                if (s[i] != '}')i++;
                else {
                    pushWord(s, i + 1, TYPE_COMMENT);
                    return i + 1;
                }
            }
            return ERROR_COMMENT;
        }

        /**
         * 一个点 . 或者两个点 ..
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 1 或 2
         */
        int takeDot(const char *s) {
            if (s[1] == '.') {
                pushWord(s, 2, TYPE_ARRAY);
                return 2;
            }
            pushWord(s, 1, TYPE_SINGLE_CHAR);
            return 1;
        }

        /**
         * 空白符
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeWhiteSpace(const char *s) {
            if (s[0] == '\n')global_line_number++;
            int i = 1;
            while (s[i] != '\0') {
                if (s[i] == '\n')global_line_number++;
                if (s[i] == ' ' || s[i] == '\t' || s[i] == '\n') {
                    i++;
                } else break;
            }
            pushWord(s, i, TYPE_WHITESPACE);
            return i;
        }

        int analysisOneWord(const char *s) {
            int c;
            if ((s[0] >= 'a' && s[0] <= 'z') || (s[0] >= 'A' && s[0] <= 'Z')) {
                // 字母打头			返回 大于等于1
                c = takeIdentifier(s);
            } else if (s[0] >= '0' && s[0] <= '9') {
                // 数字打头			返回 大于等于1
                c = takeNum(s);
            } else if (
                    s[0] == '+' || s[0] == '-' || s[0] == '*' || s[0] == '/' ||
                    s[0] == '(' || s[0] == ')' || s[0] == '[' || s[0] == ']' ||
                    s[0] == ';' || s[0] == '<' || s[0] == '=' || s[0] == ','
                    ) {
                // 单个字符			返回 大于等于1
                c = takeOneChar(s);
            } else if (s[0] == ':') {
                // 双字符 :=			返回 2 或 错误
                c = takeTwoChar(s);
            } else if (s[0] == '{') {
                // 注释				返回 大于等于2 或 错误
                c = takeComment(s);
            } else if (s[0] == '.') {
                // 可能是单字符中的 .	返回 1 或 2
                // 也可能是双字符中的数组下标界限符 ..
                c = takeDot(s);
            } else if (s[0] == ' ' || s[0] == '\t' || s[0] == '\n') {
                // 空白符			返回 大于等于1
                c = takeWhiteSpace(s);
            } else {
                return ERROR_UNEXPECTED_CHAR;
            }
            return c;
        }

        /**
         * 将词法分析解析出来的Word序列转换为Symbol序列
         * 去掉注释和空白
         */
        void convertWordsToSymbols() {
            for (const Word &w : words) {
                switch (w.type) {
                    // 忽略注释和空白符
                    case TYPE_COMMENT:
                    case TYPE_WHITESPACE:
                        continue;
                    case TYPE_ARRAY:
                        tokenSymbols.emplace_back(TokenSymbol(TwoDots(), w.line));
                        break;
                    case TYPE_SINGLE_CHAR:
                        tokenSymbols.emplace_back(TokenSymbol(Symbol(w.v, true, w.v), w.line));
                        break;
                    case TYPE_EQUAL:
                        tokenSymbols.emplace_back(TokenSymbol(ColonEqual(), w.line));
                        break;
                    case TYPE_UNSIGNED_INT:
                        tokenSymbols.emplace_back(TokenSymbol(Symbol("INTC", true, w.v), w.line));
                        break;
                    case TYPE_IDENTIFIER: {
                        string v = up(w.v);
                        int l = w.line;
                        auto b = [](Symbol s, int l, vector<TokenSymbol> &t) {
                            t.emplace_back(s, l);
                        };
                        if (v == PROCEDURE().v) {
                            b(PROCEDURE(), l, tokenSymbols);
                        } else if (v == PROGRAM().v) {
                            b(PROGRAM(), l, tokenSymbols);
                        } else if (v == TYPE().v) {
                            b(TYPE(), l, tokenSymbols);
                        } else if (v == CHAR().v) {
                            b(CHAR(), l, tokenSymbols);
                        } else if (v == INTEGER().v) {
                            b(INTEGER(), l, tokenSymbols);
                        } else if (v == ARRAY().v) {
                            b(ARRAY(), l, tokenSymbols);
                        } else if (v == OF().v) {
                            b(OF(), l, tokenSymbols);
                        } else if (v == RECORD().v) {
                            b(RECORD(), l, tokenSymbols);
                        } else if (v == END().v) {
                            b(END(), l, tokenSymbols);
                        } else if (v == VAR().v) {
                            b(VAR(), l, tokenSymbols);
                        } else if (v == BEGIN().v) {
                            b(BEGIN(), l, tokenSymbols);
                        } else if (v == IF().v) {
                            b(IF(), l, tokenSymbols);
                        } else if (v == THEN().v) {
                            b(THEN(), l, tokenSymbols);
                        } else if (v == ELSE().v) {
                            b(ELSE(), l, tokenSymbols);
                        } else if (v == FI().v) {
                            b(FI(), l, tokenSymbols);
                        } else if (v == WHILE().v) {
                            b(WHILE(), l, tokenSymbols);
                        } else if (v == DO().v) {
                            b(DO(), l, tokenSymbols);
                        } else if (v == ENDWH().v) {
                            b(ENDWH(), l, tokenSymbols);
                        } else if (v == READ().v) {
                            b(READ(), l, tokenSymbols);
                        } else if (v == WRITE().v) {
                            b(WRITE(), l, tokenSymbols);
                        } else if (v == RETURN().v) {
                            b(RETURN(), l, tokenSymbols);
                        } else {
                            // 不是关键字 不转换成大写
                            b(Symbol("ID", true, w.v), l, tokenSymbols);
                        }
                        break;
                    }
                }
            }
        }

    };

}

using snl::SnlGrammarAnalyser;

#endif //SNL_SNLGRAMMARANALYSER_H
