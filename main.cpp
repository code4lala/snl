/**
 * 更正了推导式
 * 去掉了snl语言不支持的字符串'string'的解析
 */

#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <cstring>
#include <map>
#include <set>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <limits>

#if _MSC_VER >= 1910
#define strncpy(a,b,c) strncpy_s(a,b,c)
#endif

using namespace std;

enum {
    ERROR_TWO_CHAR = -2,
    ERROR_COMMENT = -3,
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

int global_line_number = 1;

struct Word {
    int type;
    string v;
    int line;

    explicit Word(const string &s, int i) : v(s), type(i), line(global_line_number) {}

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

list<Word> words;
char buf[4096] = {0};

void pushWord(const char *s, int l, int type) {
    strncpy(buf, s, l);
    buf[l] = '\0';
    words.emplace_back(Word(buf, type));
}

/**
 * 将输入流读入为一个string
 * @param in 输入流
 * @return 输入流读入的所有内容
 */
string convertStreamToString(ifstream &in) {
    string r, x;
    while (getline(in, x)) {
        r += x;
        r += '\n';
    }
    return r;
}

/**
 * 标识符
 * @param s 待处理字符串
 * @return 处理成功的字符串长度
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
 * @return 处理成功的字符串长度
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
 * @return 处理成功的字符串长度 或 失败原因
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
 * @return 处理成功的字符串长度 或 失败原因
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
 * @return 处理成功的字符串长度
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
 * @return 处理成功的字符串长度
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
        // 字母打头
        c = takeIdentifier(s);
    } else if (s[0] >= '0' && s[0] <= '9') {
        // 数字打头
        c = takeNum(s);
    } else if (
            s[0] == '+' || s[0] == '-' || s[0] == '*' || s[0] == '/' ||
            s[0] == '(' || s[0] == ')' || s[0] == '[' || s[0] == ']' ||
            s[0] == ';' || s[0] == '<' || s[0] == '=' || s[0] == ','
            ) {
        // 单个字符
        c = takeOneChar(s);
    } else if (s[0] == ':') {
        // 双字符 :=
        c = takeTwoChar(s);
    } else if (s[0] == '{') {
        // 注释
        c = takeComment(s);
    } else if (s[0] == '.') {
        // 可能是单字符中的 . 也可能是双字符中的数组下标界限符 ..
        c = takeDot(s);
    } else if (s[0] == ' ' || s[0] == '\t' || s[0] == '\n') {
        // 空白符
        c = takeWhiteSpace(s);
    } else {
        cerr << "[Error] unexpected character" << endl;
        exit(-1);
    }
    if (c <= 0) {
        cerr << "[Error] error code: " << c << endl;
        exit(c);
    }
    return c;
}


struct Symbol {
    string name;
    bool terminate;
    string v;

    explicit Symbol(const string &name, bool terminate) : name(name), terminate(terminate) { v = ""; }

    explicit Symbol(const string &name, bool terminate, const string &v) : name(name), terminate(terminate), v(v) {}

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

static Symbol
// 终极符 空串
        Epsilon("ε", true, ""),

// 终极符 单字符分界符
        Plus("+", true, "+"),//加号 +
        Sub("-", true, "-"),//减号 -
        Mul("*", true, "*"),//乘号 *
        Div("/", true, "/"),//除号 /
        LessThan("<", true, "<"),//小于号 <
        Equal("=", true, "="),// 等于 =
        LeftParenthesis("(", true, "("), // 左小括号 (
        RightParenthesis(")", true, ")"), // 右小括号 )
        LeftBracket("[", true, "["),//左中括号 [
        RightBracket("]", true, "]"),//右中括号 ]
        Dot(".", true, "."),//单字符分界符 . 一个点 用于结构体
        Semicolon(";", true, ";"),// 分号 ;
        Comma(",", true, ","),// 逗号 ,

// 终极符 双字符分界符
        TwoDots("..", true, ".."),//数组下标分隔符 两个点 ..
        ColonEqual(":=", true, ":="),// 赋值符号 :=

// 终极符 #
        Sharp("#", true, "#"),//不出现在First集中，出现在Follow集和Predict集中

// 终极符 关键字
        ID("ID", true, "ID"), // TODO: ID 的处理
        PROCEDURE("PROCEDURE", true, "PROCEDURE"),// keyword procedure
        PROGRAM("PROGRAM", true, "PROGRAM"), // keyword program
        TYPE("TYPE", true, "TYPE"), // keyword type
        CHAR("CHAR", true, "CHAR"),// keyword char
        INTEGER("INTEGER", true, "INTEGER"),// keyword integer
        ARRAY("ARRAY", true, "ARRAY"),// keyword array
        OF("OF", true, "OF"),// keyword of
        INTC("INTC", true, "INTC"),// keyword intc
        RECORD("RECORD", true, "RECORD"),// keyword record
        END("END", true, "END"),// keyword end
        VAR("VAR", true, "VAR"),// keyword var
        BEGIN("BEGIN", true, "BEGIN"), // keyword begin
        IF("IF", true, "IF"),// keyword if
        THEN("THEN", true, "THEN"),// keyword then
        ELSE("ELSE", true, "ELSE"),// keyword else
        FI("FI", true, "FI"),// keyword fi
        WHILE("WHILE", true, "WHILE"),//keyword while
        DO("DO", true, "DO"),// keyword do
        ENDWH("ENDWH", true, "ENDWH"),//keyword endwh
        READ("READ", true, "READ"),//keyword read
        WRITE("WRITE", true, "WRITE"),//keyword write
        RETURN("RETURN", true, "RETURN"),//keyword return

// 非终极符
        Program("Program", false),
        ProgramHead("ProgramHead", false),
        DeclarePart("DeclarePart", false),
        ProgramBody("ProgramBody", false),
        ProcDecPart("ProcDecPart", false),
        TypeDec("TypeDec", false),
        TypeDecList("TypeDecList", false),
        TypeId("TypeId", false),
        TypeName("TypeName", false),
        TypeDecMore("TypeDecMore", false),
        BaseType("BaseType", false),
        StructureType("StructureType", false),
        ArrayType("ArrayType", false),
        RecType("RecType", false),
        FieldDecList("FieldDecList", false),
        IdList("IdList", false),
        FieldDecMore("FieldDecMore", false),
        IdMore("IdMore", false),
        VarDec("VarDec", false),
        VarDecList("VarDecList", false),
        VarIdList("VarIdList", false),
        VarDecMore("VarDecMore", false),
        VarIdMore("VarIdMore", false),
        ProcDec("ProcDec", false),
        ParamList("ParamList", false),
        ProcBody("ProcBody", false),
        ProcDeclaration("ProcDeclaration", false),
        ParamDecList("ParamDecList", false),
        Param("Param", false),
        ParamMore("ParamMore", false),
        FormList("FormList", false),
        FidMore("FidMore", false),
        StmList("StmList", false),
        Stm("Stm", false),
        StmMore("StmMore", false),
        ConditionalStm("ConditionalStm", false),
        LoopStm("LoopStm", false),
        InputStm("InputStm", false),
        OutputStm("OutputStm", false),
        ReturnStm("ReturnStm", false),
        AssCall("AssCall", false),
        AssignmentRest("AssignmentRest", false),
        CallStmRest("CallStmRest", false),
        VariMore("VariMore", false),
        Exp("Exp", false),
        ActParamList("ActParamList", false),
        ActParamMore("ActParamMore", false),
        OtherExp("OtherExp", false),
        CmpOp("CmpOp", false),
        Term("Term", false),
        OtherTerm("OtherTerm", false),
        AddOp("AddOp", false),
        Factor("Factor", false),
        OtherFactor("OtherFactor", false),
        MultOp("MultOp", false),
        Variable("Variable", false),
        FieldVar("FieldVar", false),
        FieldVarMore("FieldVarMore", false),
        TypeDeclaration("TypeDeclaration", false),
        VarDeclaration("VarDeclaration", false),
        SimpleExp("SimpleExp", false);

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
list<Derivation> allDerivations;
// First集
map<Symbol, set<Symbol>> FirstSet;
// Follow集
map<Symbol, set<Symbol>> FollowSet;
// Predict集
map<Derivation, set<Symbol>> PredictSet;


void buildInit() {
    // 将所有推导放到链表里面
    auto b = [](const Symbol &s, const vector<Symbol> &v) {
        allDerivations.emplace_back(s, v);
    };
    b(Program, {ProgramHead, DeclarePart, ProgramBody, Dot});
    b(ProgramHead, {PROGRAM, ID});
    b(DeclarePart, {TypeDec, VarDec, ProcDec});
    b(TypeDec, {Epsilon});
    b(TypeDec, {TypeDeclaration});
    b(TypeDeclaration, {TYPE, TypeDecList});
    b(TypeDecList, {TypeId, Equal, TypeName, Semicolon, TypeDecMore});
    b(TypeDecMore, {Epsilon});
    b(TypeDecMore, {TypeDecList});
    b(TypeId, {ID});
    b(TypeName, {BaseType});
    b(TypeName, {StructureType});
    b(TypeName, {ID});
    b(BaseType, {INTEGER});
    b(BaseType, {CHAR});
    b(StructureType, {ArrayType});
    b(StructureType, {RecType});
    b(ArrayType, {ARRAY, LeftBracket, INTC, TwoDots, INTC, RightBracket, OF, BaseType});
    b(RecType, {RECORD, FieldDecList, END});
    b(FieldDecList, {BaseType, IdList, Semicolon, FieldDecMore});
    b(FieldDecList, {ArrayType, IdList, Semicolon, FieldDecMore});
    b(FieldDecMore, {Epsilon});
    b(FieldDecMore, {FieldDecList});
    b(IdList, {ID, IdMore});
    b(IdMore, {Epsilon});
    b(IdMore, {Comma, IdList});
    b(VarDec, {Epsilon});
    b(VarDec, {VarDeclaration});
    b(VarDeclaration, {VAR, VarDecList});
    b(VarDecList, {TypeName, VarIdList, Semicolon, VarDecMore});
    b(VarDecMore, {Epsilon});
    b(VarDecMore, {VarDecList});
    b(VarIdList, {ID, VarIdMore});
    b(VarIdMore, {Epsilon});
    b(VarIdMore, {Comma, VarIdList});
    b(ProcDec, {Epsilon});
    b(ProcDec, {ProcDeclaration});
    b(ProcDeclaration, {PROCEDURE,
                        ID, LeftParenthesis, ParamList, RightParenthesis, Semicolon,
                        ProcDecPart,
                        ProcBody,
                        ProcDec
    });
    b(ParamList, {Epsilon});
    b(ParamList, {ParamDecList});
    b(ParamDecList, {Param, ParamMore});
    b(ParamMore, {Epsilon});
    b(ParamMore, {Semicolon, ParamDecList});
    b(Param, {TypeName, FormList});
    b(Param, {VAR, TypeName, FormList});
    b(FormList, {ID, FidMore});
    b(FidMore, {Epsilon});
    b(FidMore, {Comma, FormList});
    b(ProcDecPart, {DeclarePart});
    b(ProcBody, {ProgramBody});
    b(ProgramBody, {BEGIN, StmList, END});
    b(StmList, {Stm, StmMore});
    b(StmMore, {Epsilon});
    b(StmMore, {Semicolon, StmList});
    b(Stm, {ConditionalStm});
    b(Stm, {LoopStm});
    b(Stm, {InputStm});
    b(Stm, {OutputStm});
    b(Stm, {ReturnStm});
    b(Stm, {ID, AssCall});
    b(AssCall, {AssignmentRest});
    b(AssCall, {CallStmRest});
    b(AssignmentRest, {VariMore, ColonEqual, Exp});
    b(ConditionalStm, {IF, Exp, THEN, StmList, ELSE, StmList, FI});
    b(LoopStm, {WHILE, Exp, DO, StmList, ENDWH});
    b(InputStm, {READ, LeftParenthesis, ID, RightParenthesis});
    b(OutputStm, {WRITE, LeftParenthesis, Exp, RightParenthesis});
    b(ReturnStm, {RETURN});
    b(CallStmRest, {LeftParenthesis, ActParamList, RightParenthesis});
    b(ActParamList, {Epsilon});
    b(ActParamList, {Exp, ActParamMore});
    b(ActParamMore, {Epsilon});
    b(ActParamMore, {Comma, ActParamList});
    b(Exp, {SimpleExp, OtherExp});
    b(OtherExp, {Epsilon});
    b(OtherExp, {CmpOp, SimpleExp});
    b(SimpleExp, {Term, OtherTerm});
    b(OtherTerm, {Epsilon});
    b(OtherTerm, {AddOp, SimpleExp});
    b(Term, {Factor, OtherFactor});
    b(OtherFactor, {Epsilon});
    b(OtherFactor, {MultOp, Term});
    b(Factor, {LeftParenthesis, Exp, RightParenthesis});
    b(Factor, {INTC});
    b(Factor, {Variable});
    b(Variable, {ID, VariMore});
    b(VariMore, {Epsilon});
    b(VariMore, {LeftBracket, Exp, RightBracket});
    b(VariMore, {Dot, FieldVar});
    b(FieldVar, {ID, FieldVarMore});
    b(FieldVarMore, {Epsilon});
    b(FieldVarMore, {LeftBracket, Exp, RightBracket});
    b(CmpOp, {LessThan});
    b(CmpOp, {Equal});
    b(AddOp, {Plus});
    b(AddOp, {Sub});
    b(MultOp, {Mul});
    b(MultOp, {Div});
}

int firstSize() {
    int cnt = 0;
    for (const auto &a:FirstSet) {
        cnt += a.second.size();
    }
    return cnt;
}

/**
 * 构造First集
 */
void buildFirst() {
    int lastSize = -1;
    int newSize = 0;
    // 这个链表存储所有 First(b) ∈ First(a) 的关系
    // 表现形式为 a->b... 或者 ( a->c1c2c3b... 但是c1c2c3都能推导出空串 所以这个等价于 a->b... )
    list<pair<Symbol, Symbol>> a_to_b;
    while (lastSize != newSize) {
        for (const Derivation &d : allDerivations) {
            // 求d.head的First集
            Symbol symbol = d.head;
            /* 1. s一定是非终极符，因为s在推导产生式的左侧
             * 2. First(X)={a|X->a...是文法的一个产生式，a∈终极符}
             * 3. 若有X->Epsilon，则{Epsilon}∈First(X)
             * 4. 有产生式X->Y1Y2Y3...Yn，且Y1，Y2，Y3，...，Yi∈非终极符
             */
            set<Symbol> sSet = FirstSet[symbol];
            // 往已有的set里边添加，添加完成后再放回FirstSetSymbol中
            if (d.symbolVector.front() == Epsilon) {
                // symbol能推导出 空串
                sSet.insert(Epsilon);
            } else if (d.symbolVector.front().terminate) {
                // symbol能推导出 终极符打头的串
                sSet.insert(d.symbolVector.front());
            } else {
                // symbol能推导出 非终极符打头的串
                for (const Symbol &c : d.symbolVector) {
                    // c是symbol推导出的串的每个符号 symbol->C1C2C3...Cn
                    // 如果当前的c是终结符则结束
                    if (c.terminate) {
                        sSet.insert(c);
                        break;
                    }
                    a_to_b.emplace_back(pair<Symbol, Symbol>(symbol, c));
                    // 如果前边的c的First集里边有Epsilon，继续，下面判断当前c的First集中是否有Epsilon
                    bool cEpsilon = false;
                    set<Symbol> cSet = FirstSet[c];
                    for (const Symbol &cSymbol : cSet) {
                        if (cSymbol == Epsilon) {
                            cEpsilon = true;
                        } else {
                            // 把c的First集里的复制到symbol的First集里
                            sSet.insert(cSymbol);
                        }
                    }
                    // 即symbol->C1C2C3... C1的First集中没有Epsilon的情形
                    if (!cEpsilon) {
                        break;
                    }
                    // 当前c的First集里边有Epsilon，如果不是最后一个则继续往前走，否则将Epsilon加入s的First集
                    // 即symbol->C1C2C3 C1、C2、C3的First集里都有Epsilon的情形
                    if (c == d.symbolVector.back()) {
                        sSet.insert(Epsilon);
                    }
                    // 默认继续循环则是symbol->C1C2C3... C1的First集中有Epsilon的情形
                }
            }
            // First集查找完毕，放到全局变量里边
            FirstSet[symbol] = sSet;
        }
        int innerLastSize = firstSize() - 1;
        int innerNewSize = innerLastSize + 1;
        while (innerLastSize != innerNewSize) {
            for (const auto &t: a_to_b) {
                Symbol a = t.first, b = t.second;
                // First(b) ∈ First(a)
                for (const auto &s:FirstSet[b]) {
                    FirstSet[a].insert(s);
                }
            }
            innerLastSize = innerNewSize;
            innerNewSize = firstSize();
        }
        // 对比此次循环有没有增加新的
        lastSize = newSize;
        newSize = firstSize();
    }
}

/**
 * FollowSet中set<Symbol>的所有元素个数
 * @return FollowSet中set<Symbol>的所有元素个数
 */
int followSize() {
    int cnt = 0;
    for (const auto &a : FollowSet) {
        cnt += a.second.size();
    }
    return cnt;
}

/**
 * 构造Follow集
 */
void buildFollow() {
    // 初始化为空集
    for (const Derivation &d : allDerivations) {
        FollowSet[d.head] = set<Symbol>{};
    }
    // 文法开始符号的Follow集中有一个#
    FollowSet[allDerivations.front().head] = {Sharp};
    int sizeL = 0;
    int sizeR = followSize();
    /* 这个链表存储所有类似 c->...ab...的推导中的a、b，即a、b满足关系First(b)-{Epsilon}∈Follow(a)
     * 特殊情况比如 c->...a1a2a3b...同时a1、a2、a3能推导出空串，则有
     * First(a2)-{Epsilon}∈Follow(a1)
     * First(a3)-{Epsilon}∈Follow(a1)
     * First(b)-{Epsilon}∈Follow(a1)
     */
    list<pair<Symbol, Symbol>> ab;
    // 遍历所有推导公式 多遍直到没有新元素加入Follow集
    while (sizeL != sizeR) {
        for (const Derivation &d : allDerivations) {
            // 当S2 ->* Epsilon时，First[S3]-{Epsilon}也∈Follow[S1]
            for (int i = 0; i < d.symbolVector.size() - 1; i++) {
                // W -> ...S1S2...
                Symbol s1 = d.symbolVector[i],
                        s2 = d.symbolVector[i + 1];
                // 终极符没有Follow集
                if (s1.terminate)continue;
                if (s2.terminate) {
                    // First(s2)-{Epsilon}∈Follow(s1)
                    FollowSet[s1].insert(s2);
                    continue;
                }
                // S1 S2都是非终极符
                auto it = FirstSet.find(s2);
                if (it == FirstSet.end()) {
                    cerr << "[error]{" << __LINE__ << "} Build Follow set for " << s1 << endl;
                    cerr << s2 << " has no First set" << endl;
                    exit(-1);
                }
                bool bEpsilon = false;
                // First(s2)-{Epsilon}∈Follow(s1)
                ab.emplace_back(pair<Symbol, Symbol>(s1, s2));
                for (const Symbol &smb : FirstSet[s2]) {
                    if (smb != Epsilon) {
                        FollowSet[s1].insert(smb);
                    } else {
                        bEpsilon = true;
                    }
                }
                if (bEpsilon) {
                    // j的初始值位置是 W—>...S1S2S3S4... 中的S3
                    for (int j = i + 2; j < d.symbolVector.size(); j++) {
                        // 如果s3是终极符直接结束
                        if (d.symbolVector[j].terminate) {
                            FollowSet[s1].insert(d.symbolVector[j]);
                            break;
                        }
                        // 此时s3确定不是终结符
                        ab.emplace_back(s1, d.symbolVector[j]);
                        bool hasEpsilon = false;
                        // 判断s3是否能推导出空串
                        for (const Symbol &sss:FirstSet[d.symbolVector[j]]) {
                            if (sss == Epsilon) {
                                hasEpsilon = true;
                                break;
                            }
                        }
                        // 如果s3不能推导出空串就退出
                        if (!hasEpsilon)break;
                    }
                }
            }
            for (const auto &t:ab) {
                Symbol a = t.first;
                Symbol b = t.second;
                // First(b)-{Epsilon}∈Follow(a)
                for (const Symbol &s:FirstSet[b]) {
                    if (s != Epsilon) {
                        FollowSet[a].insert(s);
                    }
                }
            }
            for (int i = d.symbolVector.size() - 1; i >= 0; i--) {
                // W -> ...S1S2
                // Follow(W)∈Follow(S2) 将Follow(W)加入到Follow(S2)中
                Symbol s2 = d.symbolVector[i];
                if (s2.terminate)break;
                for (const Symbol &smb : FollowSet[d.head]) {
                    FollowSet[s2].insert(smb);
                }
                // 判断s2能否推出Epsilon，如果是则继续循环，否则退出循环
                bool bEpsilon = false;
                for (const Derivation &derivation : allDerivations) {
                    if (derivation.head != s2)continue;
                    if (derivation.symbolVector.size() != 1)continue;
                    if (derivation.symbolVector[0] != Epsilon)continue;
                    bEpsilon = true;
                    break;
                }
                if (bEpsilon)continue;
                else break;
            }
        }
        sizeL = sizeR;
        sizeR = followSize();
    }
}

set<Symbol> getFirstBySymbol(const Symbol &s) {
    if (s.terminate) {
        return set<Symbol>{s};
    } else {
        auto it = FirstSet.find(s);
        if (it == FirstSet.end()) {
            cerr << "[error]" << __LINE__ << " can not find " << s << " in First set" << endl;
            exit(-1);
        }
        return FirstSet[s];
    }
}

/**
 * 构造Predict集
 */
void buildPredict() {
    for (const Derivation &d : allDerivations) {
        // Predict(A->B)=First(B), if ε?First(B)
        //              =(First(B)-{ε})∪Follow(A),if ε∈First(B)
        // 先求First(B)
        set<Symbol> firstB = getFirstBySymbol(d.symbolVector[0]);
        int i = 1;
        while (firstB.find(Epsilon) != firstB.end()) {
            if (i < d.symbolVector.size()) {
                firstB.erase(Epsilon);
                set<Symbol> f2 = getFirstBySymbol(d.symbolVector[i]);
                for (const Symbol &smb : f2) {
                    firstB.insert(smb);
                }
            } else break;
            i++;
        }
        if (firstB.find(Epsilon) == firstB.end()) {
            // ε?First(B)
            PredictSet[d] = firstB;
        } else {
            // ε∈First(B)
            firstB.erase(Epsilon);
            for (const Symbol &smb : FollowSet[d.head]) {
                firstB.insert(smb);
            }
            PredictSet[d] = firstB;
        }
    }
}

/**
 * 输出所有推导关系
 */
void displayDerivations() {
    cout << endl << "Derivations:" << endl;
    for (const Derivation &d : allDerivations) {
        cout << d << endl;
    }
}

/**
 * 输出Symbol集合
 * @param l 待输出的Symbol集合
 */
void displaySymbolSet(const set<Symbol> &l) {
    int i = 0;
    cout << "{";
    for (const Symbol &t : l) {
        if (i != l.size() - 1) {
            cout << t.name << ", ";
        } else {
            cout << t.name << "}";
        }
        i++;
    }
    if (i == 0) {
        cout << " }";
    }
}

/**
 * 输出构造的First集
 */
void displayFirst() {
    cout << endl;
    cout << "First sets:" << endl;
    for (const auto &a : FirstSet) {
        Symbol s = a.first;
        cout << "First of [" << s.name << "] = ";
        displaySymbolSet(a.second);
        cout << endl;
    }
}

/**
 * 输出构造的Follow集
 */
void displayFollow() {
    cout << endl;
    cout << "Follow sets:" << endl;
    for (const auto &a : FollowSet) {
        Symbol s = a.first;
        cout << "Follow of [" << s.name << "] = ";
        displaySymbolSet(a.second);
        cout << endl;
    }
}

/**
 * 输出构造的Predict集
 */
void displayPredict() {
    cout << endl;
    cout << "Predict sets:" << endl;
    for (const auto &a : PredictSet) {
        Derivation d = a.first;
        cout << "{{ " << d << " }} -> ";
        displaySymbolSet(a.second);
        cout << endl;
    }
}

// 检查First集合中有没有空集
void checkFirstSets() {
    bool hasEmptySet = false;
    for (const auto &a : FirstSet) {
        if (a.second.empty()) {
            hasEmptySet = true;
            break;
        }
    }
    if (hasEmptySet)
        cerr << "[error] Empty set in First sets" << endl;
    else
        cout << "No empty set in First sets" << endl;
}

// 检查Follow集合中有没有空集
void checkFollowSets() {
    bool hasEmptySet = false;
    for (const auto &a : FollowSet) {
        if (a.second.empty()) {
            hasEmptySet = true;
            break;
        }
    }
    if (hasEmptySet)
        cerr << "[error] Empty set in Follow sets" << endl;
    else
        cout << "No empty set in Follow sets" << endl;
}

// 检查Predict集合中有没有空集
void checkPredictSets() {
    bool hasEmptySet = false;
    for (const auto &a : PredictSet) {
        if (a.second.empty()) {
            hasEmptySet = true;
            break;
        }
    }
    if (hasEmptySet)
        cerr << "[error] Empty set in Predict sets" << endl;
    else
        cout << "No empty set in Predict sets" << endl;

}

struct TokenSymbol {
    Symbol s;
    int line;

    explicit TokenSymbol(const Symbol &s, int line) : s(s), line(line) {}

    friend ostream &operator<<(ostream &os, const TokenSymbol &symbol) {
        os << setw(4) << symbol.line << symbol.s;
        return os;
    }
};

vector<TokenSymbol> tokenSymbols;

string up(const string &x) {
    string y = x;
    transform(y.begin(), y.end(), y.begin(), ::toupper);
    return y;
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
                tokenSymbols.emplace_back(TokenSymbol(TwoDots, w.line));
                break;
            case TYPE_SINGLE_CHAR:
                tokenSymbols.emplace_back(TokenSymbol(Symbol(w.v, true, w.v), w.line));
                break;
            case TYPE_EQUAL:
                tokenSymbols.emplace_back(TokenSymbol(ColonEqual, w.line));
                break;
            case TYPE_UNSIGNED_INT:
                tokenSymbols.emplace_back(TokenSymbol(Symbol("INTC", true, w.v), w.line));
                break;
            case TYPE_IDENTIFIER: {
                string v = up(w.v);
                int l = w.line;
                auto b = [](Symbol s, int l) {
                    tokenSymbols.emplace_back(s, l);
                };
                if (v == PROCEDURE.v) {
                    b(PROCEDURE, l);
                } else if (v == PROGRAM.v) {
                    b(PROGRAM, l);
                } else if (v == TYPE.v) {
                    b(TYPE, l);
                } else if (v == CHAR.v) {
                    b(CHAR, l);
                } else if (v == INTEGER.v) {
                    b(INTEGER, l);
                } else if (v == ARRAY.v) {
                    b(ARRAY, l);
                } else if (v == OF.v) {
                    b(OF, l);
                } else if (v == RECORD.v) {
                    b(RECORD, l);
                } else if (v == END.v) {
                    b(END, l);
                } else if (v == VAR.v) {
                    b(VAR, l);
                } else if (v == BEGIN.v) {
                    b(BEGIN, l);
                } else if (v == IF.v) {
                    b(IF, l);
                } else if (v == THEN.v) {
                    b(THEN, l);
                } else if (v == ELSE.v) {
                    b(ELSE, l);
                } else if (v == FI.v) {
                    b(FI, l);
                } else if (v == WHILE.v) {
                    b(WHILE, l);
                } else if (v == DO.v) {
                    b(DO, l);
                } else if (v == ENDWH.v) {
                    b(ENDWH, l);
                } else if (v == READ.v) {
                    b(READ, l);
                } else if (v == WRITE.v) {
                    b(WRITE, l);
                } else if (v == RETURN.v) {
                    b(RETURN, l);
                } else {
                    // 不是关键字 不转换成大写
                    b(Symbol("ID", true, w.v), l);
                }
                break;
            }
        }
    }
}

void displaySymbolTokens() {
    cout << endl;
    for (const TokenSymbol &t:tokenSymbols) {
        cout << t << endl;
    }
}

/**
 * 输出当前分析栈的最新几位
 * @param s 待输出的分析栈
 */
void displayAnalysisStack(const list<Symbol> &s) {
    int i = s.size() - 5;
    i = i < 0 ? 0 : i;
    auto it = s.begin();
    advance(it, i);
    while (it != s.end()) {
        if (it->terminate)
            cout << it->v << " ";
        else
            cout << it->name << " ";
        it++;
    }
    cout << setw(5) << "|";
}

/**
 * 输出输入流的前几位
 */
void displayInputStream(int n) {
    for (int i = n; i < n + 5 && i < tokenSymbols.size(); i++) {
        if (tokenSymbols[i].s.terminate)
            cout << tokenSymbols[i].s.v << " ";
        else
            cout << tokenSymbols[i].s.name << " ";
    }
    cout << setw(5) << "|";
}

/**
 * 语法分析
 */
void grammarAnalysis() {
    // 例 分析栈 #E   输入流 i+i*i#
    list<Symbol> s;//因为栈不能遍历输出所以用list链表代替
    s.emplace_back(Sharp); // #
    s.emplace_back(allDerivations.front().head); // 文法开始符
    tokenSymbols.emplace_back(TokenSymbol(Sharp, numeric_limits<int>::max()));
    int i = 0;//输入流索引
    while (!s.empty()) {
        displayAnalysisStack(s);
        displayInputStream(i);
        cout << endl;
        Symbol t = s.back();//栈顶元素
        Symbol r = tokenSymbols[i].s;//输入流当前扫描符号
        if (!t.terminate) {
            //非终极符 寻找 t->啥 的Predict集 中有输入流当前索引符号
            bool notFound = true;
            for (const auto &a:PredictSet) {
                if (a.first.head != t)continue;
                set<Symbol>::iterator f;
                if (r.name == "ID") {
                    f = a.second.find(ID);
                } else {
                    f = a.second.find(r);
                }
                if (f == a.second.end())continue;
                // 找到了 t->a.second的Predict集中有 r
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
                cerr << "[error] " << __LINE__ << " " << t << " can not predict " << r << endl;
                cerr << "at line " << tokenSymbols[i].line << endl;
                exit(-1);
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
                    // 出错
                    cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r << endl;
                    cerr << "at line " << tokenSymbols[i].line << endl;
                    exit(-1);
                }
            } else if (t == Epsilon) {
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
                    // 出错
                    cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r << endl;
                    cerr << "at line " << tokenSymbols[i].line << endl;
                    exit(-1);
                }

            } else {
                // 单独的
                if (t == r) {
                    // 匹配了!!!
                    if (t == Sharp) {
                        // 完成了!!!
                        break;
                    }
                    s.pop_back();
                    i++;
                    continue;
                } else {
                    // 出错
                    cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r << endl;
                    cerr << "at line " << tokenSymbols[i].line << endl;
                    exit(-1);
                }
            }
        }
    }
    cout << endl << "Grammar analysis completed! 0 error!" << endl;
}

/**
 * 词法语法分析
 * @param in 待分析的源文件
 */
void analysis(ifstream &in) {
    string x = convertStreamToString(in);
    cout << x;
    const char *s = x.c_str();
    while (*s != '\0') {
        s += analysisOneWord(s);
    }
    convertWordsToSymbols();
    displaySymbolTokens();
    // 词法分析完成 tokenList是tokenSymbols

    buildInit();
    displayDerivations();

    buildFirst();
    displayFirst();
    checkFirstSets();

    buildFollow();
    displayFollow();
    checkFollowSets();

    buildPredict();
    displayPredict();
    checkPredictSets();
    // First Follow Predict三个集合构造完成

    grammarAnalysis();
}


int main() {
    ifstream in;
    in.open("../examples/C8.TXT", ios::in);
    analysis(in);
    in.close();
    return 0;
}
