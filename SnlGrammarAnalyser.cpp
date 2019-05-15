#include "snlgrammaranalyser.h"

using namespace ____NamespaceSnlGrammarAnalyser;

int ____NamespaceSnlGrammarAnalyser::global_line_number = 1;
char ____NamespaceSnlGrammarAnalyser::buf[4096] = { 0 };
bool ____NamespaceSnlGrammarAnalyser::initialized = false;

Symbol
// 终极符 空串
        ____NamespaceSnlGrammarAnalyser::Epsilon("Epsilon", true, ""),

// 终极符 单字符分界符
        ____NamespaceSnlGrammarAnalyser::Plus("+", true, "+"),//加号 +
        ____NamespaceSnlGrammarAnalyser::Sub("-", true, "-"),//减号 -
        ____NamespaceSnlGrammarAnalyser::Mul("*", true, "*"),//乘号 *
        ____NamespaceSnlGrammarAnalyser::Div("/", true, "/"),//除号 /
        ____NamespaceSnlGrammarAnalyser::LessThan("<", true, "<"),//小于号 <
        ____NamespaceSnlGrammarAnalyser::Equal("=", true, "="),// 等于 =
        ____NamespaceSnlGrammarAnalyser::LeftParenthesis("(", true, "("), // 左小括号 (
        ____NamespaceSnlGrammarAnalyser::RightParenthesis(")", true, ")"), // 右小括号 )
        ____NamespaceSnlGrammarAnalyser::LeftBracket("[", true, "["),//左中括号 [
        ____NamespaceSnlGrammarAnalyser::RightBracket("]", true, "]"),//右中括号 ]
        ____NamespaceSnlGrammarAnalyser::Dot(".", true, "."),//单字符分界符 . 一个点 用于结构体
        ____NamespaceSnlGrammarAnalyser::Semicolon(";", true, ";"),// 分号 ;
        ____NamespaceSnlGrammarAnalyser::Comma(",", true, ","),// 逗号 ,

// 终极符 双字符分界符
        ____NamespaceSnlGrammarAnalyser::TwoDots("..", true, ".."),//数组下标分隔符 两个点 ..
        ____NamespaceSnlGrammarAnalyser::ColonEqual(":=", true, ":="),// 赋值符号 :=

// 终极符 #
        ____NamespaceSnlGrammarAnalyser::Sharp("#", true, "#"),//不出现在First集中，出现在Follow集和Predict集中

// 终极符 关键字
        ____NamespaceSnlGrammarAnalyser::ID("ID", true, "ID"),
        ____NamespaceSnlGrammarAnalyser::PROCEDURE("PROCEDURE", true, "PROCEDURE"),// keyword procedure
        ____NamespaceSnlGrammarAnalyser::PROGRAM("PROGRAM", true, "PROGRAM"), // keyword program
        ____NamespaceSnlGrammarAnalyser::TYPE("TYPE", true, "TYPE"), // keyword type
        ____NamespaceSnlGrammarAnalyser::CHAR("CHAR", true, "CHAR"),// keyword char
        ____NamespaceSnlGrammarAnalyser::INTEGER("INTEGER", true, "INTEGER"),// keyword integer
        ____NamespaceSnlGrammarAnalyser::ARRAY("ARRAY", true, "ARRAY"),// keyword array
        ____NamespaceSnlGrammarAnalyser::OF("OF", true, "OF"),// keyword of
        ____NamespaceSnlGrammarAnalyser::INTC("INTC", true, "INTC"),// keyword intc
        ____NamespaceSnlGrammarAnalyser::RECORD("RECORD", true, "RECORD"),// keyword record
        ____NamespaceSnlGrammarAnalyser::END("END", true, "END"),// keyword end
        ____NamespaceSnlGrammarAnalyser::VAR("VAR", true, "VAR"),// keyword var
        ____NamespaceSnlGrammarAnalyser::BEGIN("BEGIN", true, "BEGIN"), // keyword begin
        ____NamespaceSnlGrammarAnalyser::IF("IF", true, "IF"),// keyword if
        ____NamespaceSnlGrammarAnalyser::THEN("THEN", true, "THEN"),// keyword then
        ____NamespaceSnlGrammarAnalyser::ELSE("ELSE", true, "ELSE"),// keyword else
        ____NamespaceSnlGrammarAnalyser::FI("FI", true, "FI"),// keyword fi
        ____NamespaceSnlGrammarAnalyser::WHILE("WHILE", true, "WHILE"),//keyword while
        ____NamespaceSnlGrammarAnalyser::DO("DO", true, "DO"),// keyword do
        ____NamespaceSnlGrammarAnalyser::ENDWH("ENDWH", true, "ENDWH"),//keyword endwh
        ____NamespaceSnlGrammarAnalyser::READ("READ", true, "READ"),//keyword read
        ____NamespaceSnlGrammarAnalyser::WRITE("WRITE", true, "WRITE"),//keyword write
        ____NamespaceSnlGrammarAnalyser::RETURN("RETURN", true, "RETURN"),//keyword return

// 非终极符
        ____NamespaceSnlGrammarAnalyser::Program("Program", false),
        ____NamespaceSnlGrammarAnalyser::ProgramHead("ProgramHead", false),
        ____NamespaceSnlGrammarAnalyser::DeclarePart("DeclarePart", false),
        ____NamespaceSnlGrammarAnalyser::ProgramBody("ProgramBody", false),
        ____NamespaceSnlGrammarAnalyser::ProcDecPart("ProcDecPart", false),
        ____NamespaceSnlGrammarAnalyser::TypeDec("TypeDec", false),
        ____NamespaceSnlGrammarAnalyser::TypeDecList("TypeDecList", false),
        ____NamespaceSnlGrammarAnalyser::TypeId("TypeId", false),
        ____NamespaceSnlGrammarAnalyser::TypeName("TypeName", false),
        ____NamespaceSnlGrammarAnalyser::TypeDecMore("TypeDecMore", false),
        ____NamespaceSnlGrammarAnalyser::BaseType("BaseType", false),
        ____NamespaceSnlGrammarAnalyser::StructureType("StructureType", false),
        ____NamespaceSnlGrammarAnalyser::ArrayType("ArrayType", false),
        ____NamespaceSnlGrammarAnalyser::RecType("RecType", false),
        ____NamespaceSnlGrammarAnalyser::FieldDecList("FieldDecList", false),
        ____NamespaceSnlGrammarAnalyser::IdList("IdList", false),
        ____NamespaceSnlGrammarAnalyser::FieldDecMore("FieldDecMore", false),
        ____NamespaceSnlGrammarAnalyser::IdMore("IdMore", false),
        ____NamespaceSnlGrammarAnalyser::VarDec("VarDec", false),
        ____NamespaceSnlGrammarAnalyser::VarDecList("VarDecList", false),
        ____NamespaceSnlGrammarAnalyser::VarIdList("VarIdList", false),
        ____NamespaceSnlGrammarAnalyser::VarDecMore("VarDecMore", false),
        ____NamespaceSnlGrammarAnalyser::VarIdMore("VarIdMore", false),
        ____NamespaceSnlGrammarAnalyser::ProcDec("ProcDec", false),
        ____NamespaceSnlGrammarAnalyser::ParamList("ParamList", false),
        ____NamespaceSnlGrammarAnalyser::ProcBody("ProcBody", false),
        ____NamespaceSnlGrammarAnalyser::ProcDeclaration("ProcDeclaration", false),
        ____NamespaceSnlGrammarAnalyser::ParamDecList("ParamDecList", false),
        ____NamespaceSnlGrammarAnalyser::Param("Param", false),
        ____NamespaceSnlGrammarAnalyser::ParamMore("ParamMore", false),
        ____NamespaceSnlGrammarAnalyser::FormList("FormList", false),
        ____NamespaceSnlGrammarAnalyser::FidMore("FidMore", false),
        ____NamespaceSnlGrammarAnalyser::StmList("StmList", false),
        ____NamespaceSnlGrammarAnalyser::Stm("Stm", false),
        ____NamespaceSnlGrammarAnalyser::StmMore("StmMore", false),
        ____NamespaceSnlGrammarAnalyser::ConditionalStm("ConditionalStm", false),
        ____NamespaceSnlGrammarAnalyser::LoopStm("LoopStm", false),
        ____NamespaceSnlGrammarAnalyser::InputStm("InputStm", false),
        ____NamespaceSnlGrammarAnalyser::OutputStm("OutputStm", false),
        ____NamespaceSnlGrammarAnalyser::ReturnStm("ReturnStm", false),
        ____NamespaceSnlGrammarAnalyser::AssCall("AssCall", false),
        ____NamespaceSnlGrammarAnalyser::AssignmentRest("AssignmentRest", false),
        ____NamespaceSnlGrammarAnalyser::CallStmRest("CallStmRest", false),
        ____NamespaceSnlGrammarAnalyser::VariMore("VariMore", false),
        ____NamespaceSnlGrammarAnalyser::Exp("Exp", false),
        ____NamespaceSnlGrammarAnalyser::ActParamList("ActParamList", false),
        ____NamespaceSnlGrammarAnalyser::ActParamMore("ActParamMore", false),
        ____NamespaceSnlGrammarAnalyser::OtherExp("OtherExp", false),
        ____NamespaceSnlGrammarAnalyser::CmpOp("CmpOp", false),
        ____NamespaceSnlGrammarAnalyser::Term("Term", false),
        ____NamespaceSnlGrammarAnalyser::OtherTerm("OtherTerm", false),
        ____NamespaceSnlGrammarAnalyser::AddOp("AddOp", false),
        ____NamespaceSnlGrammarAnalyser::Factor("Factor", false),
        ____NamespaceSnlGrammarAnalyser::OtherFactor("OtherFactor", false),
        ____NamespaceSnlGrammarAnalyser::MultOp("MultOp", false),
        ____NamespaceSnlGrammarAnalyser::Variable("Variable", false),
        ____NamespaceSnlGrammarAnalyser::FieldVar("FieldVar", false),
        ____NamespaceSnlGrammarAnalyser::FieldVarMore("FieldVarMore", false),
        ____NamespaceSnlGrammarAnalyser::TypeDeclaration("TypeDeclaration", false),
        ____NamespaceSnlGrammarAnalyser::VarDeclaration("VarDeclaration", false),
        ____NamespaceSnlGrammarAnalyser::SimpleExp("SimpleExp", false);

// 所有推导的链表
list<Derivation> ____NamespaceSnlGrammarAnalyser::allDerivations;
// First集
map<Symbol, set<Symbol>> ____NamespaceSnlGrammarAnalyser::FirstSet;
// Follow集
map<Symbol, set<Symbol>> ____NamespaceSnlGrammarAnalyser::FollowSet;
// Predict集
map<Derivation, set<Symbol>> ____NamespaceSnlGrammarAnalyser::PredictSet;

void ____NamespaceSnlGrammarAnalyser::buildInit() {
    // 将所有推导放到链表里面
    auto b = [](const Symbol &s, const vector<Symbol> &v) {
        allDerivations.emplace_back(s, v);
    };
    b(Program, { ProgramHead, DeclarePart, ProgramBody, Dot });
    b(ProgramHead, { PROGRAM, ID });
    b(DeclarePart, { TypeDec, VarDec, ProcDec });
    b(TypeDec, { Epsilon });
    b(TypeDec, { TypeDeclaration });
    b(TypeDeclaration, { TYPE, TypeDecList });
    b(TypeDecList, { TypeId, Equal, TypeName, Semicolon, TypeDecMore });
    b(TypeDecMore, { Epsilon });
    b(TypeDecMore, { TypeDecList });
    b(TypeId, { ID });
    b(TypeName, { BaseType });
    b(TypeName, { StructureType });
    b(TypeName, { ID });
    b(BaseType, { INTEGER });
    b(BaseType, { CHAR });
    b(StructureType, { ArrayType });
    b(StructureType, { RecType });
    b(ArrayType, { ARRAY, LeftBracket, INTC, TwoDots, INTC, RightBracket, OF, BaseType });
    b(RecType, { RECORD, FieldDecList, END });
    b(FieldDecList, { BaseType, IdList, Semicolon, FieldDecMore });
    b(FieldDecList, { ArrayType, IdList, Semicolon, FieldDecMore });
    b(FieldDecMore, { Epsilon });
    b(FieldDecMore, { FieldDecList });
    b(IdList, { ID, IdMore });
    b(IdMore, { Epsilon });
    b(IdMore, { Comma, IdList });
    b(VarDec, { Epsilon });
    b(VarDec, { VarDeclaration });
    b(VarDeclaration, { VAR, VarDecList });
    b(VarDecList, { TypeName, VarIdList, Semicolon, VarDecMore });
    b(VarDecMore, { Epsilon });
    b(VarDecMore, { VarDecList });
    b(VarIdList, { ID, VarIdMore });
    b(VarIdMore, { Epsilon });
    b(VarIdMore, { Comma, VarIdList });
    b(ProcDec, { Epsilon });
    b(ProcDec, { ProcDeclaration });
    b(ProcDeclaration, { PROCEDURE,
                         ID, LeftParenthesis, ParamList, RightParenthesis, Semicolon,
                         ProcDecPart,
                         ProcBody,
                         ProcDec
    });
    b(ParamList, { Epsilon });
    b(ParamList, { ParamDecList });
    b(ParamDecList, { Param, ParamMore });
    b(ParamMore, { Epsilon });
    b(ParamMore, { Semicolon, ParamDecList });
    b(Param, { TypeName, FormList });
    b(Param, { VAR, TypeName, FormList });
    b(FormList, { ID, FidMore });
    b(FidMore, { Epsilon });
    b(FidMore, { Comma, FormList });
    b(ProcDecPart, { DeclarePart });
    b(ProcBody, { ProgramBody });
    b(ProgramBody, { BEGIN, StmList, END });
    b(StmList, { Stm, StmMore });
    b(StmMore, { Epsilon });
    b(StmMore, { Semicolon, StmList });
    b(Stm, { ConditionalStm });
    b(Stm, { LoopStm });
    b(Stm, { InputStm });
    b(Stm, { OutputStm });
    b(Stm, { ReturnStm });
    b(Stm, { ID, AssCall });
    b(AssCall, { AssignmentRest });
    b(AssCall, { CallStmRest });
    b(AssignmentRest, { VariMore, ColonEqual, Exp });
    b(ConditionalStm, { IF, Exp, THEN, StmList, ELSE, StmList, FI });
    b(LoopStm, { WHILE, Exp, DO, StmList, ENDWH });
    b(InputStm, { READ, LeftParenthesis, ID, RightParenthesis });
    b(OutputStm, { WRITE, LeftParenthesis, Exp, RightParenthesis });
    b(ReturnStm, { RETURN });
    b(CallStmRest, { LeftParenthesis, ActParamList, RightParenthesis });
    b(ActParamList, { Epsilon });
    b(ActParamList, { Exp, ActParamMore });
    b(ActParamMore, { Epsilon });
    b(ActParamMore, { Comma, ActParamList });
    b(Exp, { SimpleExp, OtherExp });
    b(OtherExp, { Epsilon });
    b(OtherExp, { CmpOp, SimpleExp });
    b(SimpleExp, { Term, OtherTerm });
    b(OtherTerm, { Epsilon });
    b(OtherTerm, { AddOp, SimpleExp });
    b(Term, { Factor, OtherFactor });
    b(OtherFactor, { Epsilon });
    b(OtherFactor, { MultOp, Term });
    b(Factor, { LeftParenthesis, Exp, RightParenthesis });
    b(Factor, { INTC });
    b(Factor, { Variable });
    b(Variable, { ID, VariMore });
    b(VariMore, { Epsilon });
    b(VariMore, { LeftBracket, Exp, RightBracket });
    b(VariMore, { Dot, FieldVar });
    b(FieldVar, { ID, FieldVarMore });
    b(FieldVarMore, { Epsilon });
    b(FieldVarMore, { LeftBracket, Exp, RightBracket });
    b(CmpOp, { LessThan });
    b(CmpOp, { Equal });
    b(AddOp, { Plus });
    b(AddOp, { Sub });
    b(MultOp, { Mul });
    b(MultOp, { Div });
}

int ____NamespaceSnlGrammarAnalyser::firstSize() {
    int cnt = 0;
    for (const auto &a : FirstSet) {
        cnt += static_cast<int>(a.second.size());
    }
    return cnt;
}

void ____NamespaceSnlGrammarAnalyser::buildFirst() {
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
            }
            else if (d.symbolVector.front().terminate) {
                // symbol能推导出 终极符打头的串
                sSet.insert(d.symbolVector.front());
            }
            else {
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
                        }
                        else {
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
            for (const auto &t : a_to_b) {
                Symbol a = t.first, b = t.second;
                // First(b) ∈ First(a)
                for (const auto &s : FirstSet[b]) {
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

int ____NamespaceSnlGrammarAnalyser::followSize() {
    int cnt = 0;
    for (const auto &a : FollowSet) {
        cnt += static_cast<int>(a.second.size());
    }
    return cnt;
}

void ____NamespaceSnlGrammarAnalyser::buildFollow() {
    // 初始化为空集
    for (const Derivation &d : allDerivations) {
        FollowSet[d.head] = set<Symbol>{};
    }
    // 文法开始符号的Follow集中有一个#
    FollowSet[allDerivations.front().head] = { Sharp };
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
            for (int i = 0; i < static_cast<int>(d.symbolVector.size() - 1); i++) {
                // W -> ...S1S2...
                Symbol s1 = d.symbolVector[static_cast<unsigned int>(i)],
                        s2 = d.symbolVector[static_cast<unsigned int>(i + 1)];
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
                    }
                    else {
                        bEpsilon = true;
                    }
                }
                if (bEpsilon) {
                    // j的初始值位置是 W—>...S1S2S3S4... 中的S3
                    for (int j = i + 2; j < static_cast<int>(d.symbolVector.size()); j++) {
                        // 如果s3是终极符直接结束
                        if (d.symbolVector[static_cast<unsigned int>(j)].terminate) {
                            FollowSet[s1].insert(d.symbolVector[static_cast<unsigned int>(j)]);
                            break;
                        }
                        // 此时s3确定不是终结符
                        ab.emplace_back(s1, d.symbolVector[static_cast<unsigned int>(j)]);
                        bool hasEpsilon = false;
                        // 判断s3是否能推导出空串
                        for (const Symbol &sss : FirstSet[d.symbolVector[static_cast<unsigned int>(j)]]) {
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
            for (const auto &t : ab) {
                Symbol a = t.first;
                Symbol b = t.second;
                // First(b)-{Epsilon}∈Follow(a)
                for (const Symbol &s : FirstSet[b]) {
                    if (s != Epsilon) {
                        FollowSet[a].insert(s);
                    }
                }
            }
            for (int i = static_cast<int>(d.symbolVector.size() - 1); i >= 0; i--) {
                // W -> ...S1S2
                // Follow(W)∈Follow(S2) 将Follow(W)加入到Follow(S2)中
                Symbol s2 = d.symbolVector[static_cast<unsigned int>(i)];
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

set<Symbol> ____NamespaceSnlGrammarAnalyser::getFirstBySymbol(const Symbol &s) {
    if (s.terminate) {
        return set<Symbol>{s};
    }
    else {
        auto it = FirstSet.find(s);
        if (it == FirstSet.end()) {
            cerr << "[error]" << __LINE__ << " can not find " << s << " in First set" << endl;
            exit(-1);
        }
        return FirstSet[s];
    }
}

void ____NamespaceSnlGrammarAnalyser::buildPredict() {
    for (const Derivation &d : allDerivations) {
        // Predict(A->B)=First(B), if ε?First(B)
        //              =(First(B)-{ε})∪Follow(A),if ε∈First(B)
        // 先求First(B)
        set<Symbol> firstB = getFirstBySymbol(d.symbolVector[0]);
        int i = 1;
        while (firstB.find(Epsilon) != firstB.end()) {
            if (i < static_cast<int>(d.symbolVector.size())) {
                firstB.erase(Epsilon);
                set<Symbol> f2 = getFirstBySymbol(d.symbolVector[static_cast<unsigned int>(i)]);
                for (const Symbol &smb : f2) {
                    firstB.insert(smb);
                }
            }
            else break;
            i++;
        }
        if (firstB.find(Epsilon) == firstB.end()) {
            // ε?First(B)
            PredictSet[d] = firstB;
        }
        else {
            // ε∈First(B)
            firstB.erase(Epsilon);
            for (const Symbol &smb : FollowSet[d.head]) {
                firstB.insert(smb);
            }
            PredictSet[d] = firstB;
        }
    }
}

string ____NamespaceSnlGrammarAnalyser::up(const string &x) {
    string y = x;
    transform(y.begin(), y.end(), y.begin(), ::toupper);
    return y;
}
