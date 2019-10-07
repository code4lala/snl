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
#include <utility>
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

        explicit Word(string s, int i);
        static string getType(int i);
        friend ostream &operator<<(ostream &os, const Word &word);
    };
    ostream &operator<<(ostream &, const Word &);

    struct Symbol {
        string name;
        bool terminate;
        string v;

        explicit Symbol(string name, bool terminate);
        explicit Symbol(string name, bool terminate, string v);
        friend ostream &operator<<(ostream &os, const Symbol &symbol);
        bool operator==(const Symbol &rhs) const;
        bool operator!=(const Symbol &rhs) const;
        bool operator<(const Symbol &rhs) const;
        bool operator>(const Symbol &rhs) const;
        bool operator<=(const Symbol &rhs) const;
        bool operator>=(const Symbol &rhs) const;
    };
    ostream &operator<<(ostream &, const Symbol &);

    // 终极符 空串
    Symbol &Epsilon();

    // 终极符 单字符分界符
    Symbol &Plus();//加号 +
    Symbol &Sub();//减号 -
    Symbol &Mul();//乘号 *
    Symbol &Div();//除号 /
    Symbol &LessThan();//小于号 <
    Symbol &Equal();// 等于 =
    Symbol &LeftParenthesis(); // 左小括号 (
    Symbol &RightParenthesis(); // 右小括号 )
    Symbol &LeftBracket();//左中括号 [
    Symbol &RightBracket();//右中括号 ]
    Symbol &Dot();//单字符分界符 . 一个点 用于结构体
    Symbol &Semicolon();// 分号 ;
    Symbol &Comma();// 逗号 ,

    // 终极符 双字符分界符
    Symbol &TwoDots();//数组下标分隔符 两个点 ..
    Symbol &ColonEqual();// 赋值符号 :=

    // 终极符 #
    Symbol &Sharp();//不出现在First集中，出现在Follow集和Predict集中

    // 终极符 关键字
    Symbol &ID();

    Symbol &PROCEDURE();// keyword procedure
    Symbol &PROGRAM(); // keyword program
    Symbol &TYPE(); // keyword type
    Symbol &CHAR();// keyword char
    Symbol &INTEGER();// keyword integer
    Symbol &ARRAY();// keyword array
    Symbol &OF();// keyword of
    Symbol &INTC();// keyword intc
    Symbol &RECORD();// keyword record
    Symbol &END();// keyword end
    Symbol &VAR();// keyword var
    Symbol &BEGIN(); // keyword begin
    Symbol &IF();// keyword if
    Symbol &THEN();// keyword then
    Symbol &ELSE();// keyword else
    Symbol &FI();// keyword fi
    Symbol &WHILE();//keyword while
    Symbol &DO();// keyword do
    Symbol &ENDWH();//keyword endwh
    Symbol &READ();//keyword read
    Symbol &WRITE();//keyword write
    Symbol &RETURN();//keyword return

    // 非终极符
    Symbol &Program();
    Symbol &ProgramHead();
    Symbol &DeclarePart();
    Symbol &ProgramBody();
    Symbol &ProcDecPart();
    Symbol &TypeDec();
    Symbol &TypeDecList();
    Symbol &TypeId();
    Symbol &TypeName();
    Symbol &TypeDecMore();
    Symbol &BaseType();
    Symbol &StructureType();
    Symbol &ArrayType();
    Symbol &RecType();
    Symbol &FieldDecList();
    Symbol &IdList();
    Symbol &FieldDecMore();
    Symbol &IdMore();
    Symbol &VarDec();
    Symbol &VarDecList();
    Symbol &VarIdList();
    Symbol &VarDecMore();
    Symbol &VarIdMore();
    Symbol &ProcDec();
    Symbol &ParamList();
    Symbol &ProcBody();
    Symbol &ProcDeclaration();
    Symbol &ParamDecList();
    Symbol &Param();
    Symbol &ParamMore();
    Symbol &FormList();
    Symbol &FidMore();
    Symbol &StmList();
    Symbol &Stm();
    Symbol &StmMore();
    Symbol &ConditionalStm();
    Symbol &LoopStm();
    Symbol &InputStm();
    Symbol &OutputStm();
    Symbol &ReturnStm();
    Symbol &AssCall();
    Symbol &AssignmentRest();
    Symbol &CallStmRest();
    Symbol &VariMore();
    Symbol &Exp();
    Symbol &ActParamList();
    Symbol &ActParamMore();
    Symbol &OtherExp();
    Symbol &CmpOp();
    Symbol &Term();
    Symbol &OtherTerm();
    Symbol &AddOp();
    Symbol &Factor();
    Symbol &OtherFactor();
    Symbol &MultOp();
    Symbol &Variable();
    Symbol &FieldVar();
    Symbol &FieldVarMore();
    Symbol &TypeDeclaration();
    Symbol &VarDeclaration();
    Symbol &SimpleExp();

    extern char buf[4096];

    // 推导 的结构体 比如 F->(E)
    struct Derivation {
        Symbol head;
        vector<Symbol> symbolVector;

        explicit Derivation(Symbol head, vector<Symbol> symbolVector);
        friend ostream &operator<<(ostream &os, const Derivation &derivation);
        bool operator==(const Derivation &rhs) const;
        bool operator!=(const Derivation &rhs) const;
        bool operator<(const Derivation &rhs) const;
        bool operator>(const Derivation &rhs) const;
        bool operator<=(const Derivation &rhs) const;
        bool operator>=(const Derivation &rhs) const;
    };
    ostream &operator<<(ostream &, const Derivation &);

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

        explicit TokenSymbol(Symbol s, int line);
        friend ostream &operator<<(ostream &os, const TokenSymbol &symbol);
    };
    ostream &operator<<(ostream &, const TokenSymbol &);

    string up(const string &x);

    extern bool initialized;

    struct Node {
        Symbol curr;
        vector<Node *> children;
        Node *parent;
        int id;

        explicit Node(Symbol s, Node *parent, int id);
    };

    // parse之前先init一下就可以了，如果解析出错调用getError()查看出错原因
    class SnlGrammarAnalyser {
    public:
        static void init();
        int parse(const char *code);
        string getError();
        vector<TokenSymbol> getTokenList();
        string getTree();

    private:
        string errMsg;
        list<Word> words;
        vector<TokenSymbol> tokenSymbols;
        vector<Derivation> vTree;// 以Vector表示的语法树
        unsigned int treeIndex;// 指示vTree的下标
        Node *root;// 以链表树表示的语法树
        int global_id;//DOT Language中每个节点的id
        unsigned int global_token_index;//用于将ID转化为变量名后者函数名等
        void dfsBuildTree(Node *&parent);

        /**
         * 语法分析
         */
        int grammarAnalysis();

        void pushWord(const char *s, int l, int type);

        /**
         * 标识符
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeIdentifier(const char *s);

        /**
         * 简单数字
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeNum(const char *s);

        /**
         * 除了.以外的单字符分界符
         * @param s 待处理字符串
         * @return 1
         */
        int takeOneChar(const char *s);

        /**
         * 双字符分界符 :=
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 2 或 失败原因
         */
        int takeTwoChar(const char *s);

        /**
         * 注释 {comment}
         * @param s 待处理字符串
         * @return 处理成功的字符串长度(大于等于2) 或 失败原因
         */
        int takeComment(const char *s);

        /**
         * 一个点 . 或者两个点 ..
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 1 或 2
         */
        int takeDot(const char *s);

        /**
         * 空白符
         * @param s 待处理字符串
         * @return 处理成功的字符串长度 大于等于1
         */
        int takeWhiteSpace(const char *s);

        int analysisOneWord(const char *s);

        /**
         * 将词法分析解析出来的Word序列转换为Symbol序列
         * 去掉注释和空白
         */
        void convertWordsToSymbols();
    };
}

using snl::SnlGrammarAnalyser;

#endif //SNL_SNLGRAMMARANALYSER_H
