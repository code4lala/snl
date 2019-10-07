#include "SnlGrammarAnalyser.h"

using namespace snl;
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

int snl::global_line_number = 1;
char snl::buf[4096] = {0};
bool snl::initialized = false;

// 终极符 空串

Symbol &snl::Epsilon() {
    static Symbol Epsilon("Epsilon", true, "");
    return Epsilon;
}


// 终极符 单字符分界符
//加号 +
Symbol &snl::Plus() {
    static Symbol Plus("+", true, "+");
    return Plus;
}

//减号 -
Symbol &snl::Sub() {
    static Symbol Sub("-", true, "-");
    return Sub;
}

//乘号 *
Symbol &snl::Mul() {
    static Symbol Mul("*", true, "*");
    return Mul;
}

//除号 /
Symbol &snl::Div() {
    static Symbol Div("/", true, "/");
    return Div;
}

//小于号 <
Symbol &snl::LessThan() {
    static Symbol LessThan("<", true, "<");
    return LessThan;
}

// 等于 =
Symbol &snl::Equal() {
    static Symbol Equal("=", true, "=");
    return Equal;
}

// 左小括号 (
Symbol &snl::LeftParenthesis() {
    static Symbol LeftParenthesis("(", true, "(");
    return LeftParenthesis;
}

// 右小括号 )
Symbol &snl::RightParenthesis() {
    static Symbol RightParenthesis(")", true, ")");
    return RightParenthesis;
}

//左中括号 [
Symbol &snl::LeftBracket() {
    static Symbol LeftBracket("[", true, "[");
    return LeftBracket;
}

//右中括号 ]
Symbol &snl::RightBracket() {
    static Symbol RightBracket("]", true, "]");
    return RightBracket;
}

//单字符分界符 . 一个点 用于结构体
Symbol &snl::Dot() {
    static Symbol Dot(".", true, ".");
    return Dot;
}

// 分号 ;
Symbol &snl::Semicolon() {
    static Symbol Semicolon(";", true, ";");
    return Semicolon;
}

// 逗号 ,
Symbol &snl::Comma() {
    static Symbol Comma(",", true, ",");
    return Comma;
}


// 终极符 双字符分界符
//数组下标分隔符 两个点 ..
Symbol &snl::TwoDots() {
    static Symbol TwoDots("..", true, "..");
    return TwoDots;
}

// 赋值符号 :=
Symbol &snl::ColonEqual() {
    static Symbol ColonEqual(":=", true, ":=");
    return ColonEqual;
}


// 终极符 #
//不出现在First集中，出现在Follow集和Predict集中
Symbol &snl::Sharp() {
    static Symbol Sharp("#", true, "#");
    return Sharp;
}


// 终极符 关键字

Symbol &snl::ID() {
    static Symbol ID("ID", true, "ID");
    return ID;
}

// keyword procedure
Symbol &snl::PROCEDURE() {
    static Symbol PROCEDURE("PROCEDURE", true, "PROCEDURE");
    return PROCEDURE;
}

// keyword program
Symbol &snl::PROGRAM() {
    static Symbol PROGRAM("PROGRAM", true, "PROGRAM");
    return PROGRAM;
}

// keyword type
Symbol &snl::TYPE() {
    static Symbol TYPE("TYPE", true, "TYPE");
    return TYPE;
}

// keyword char
Symbol &snl::CHAR() {
    static Symbol CHAR("CHAR", true, "CHAR");
    return CHAR;
}

// keyword integer
Symbol &snl::INTEGER() {
    static Symbol INTEGER("INTEGER", true, "INTEGER");
    return INTEGER;
}

// keyword array
Symbol &snl::ARRAY() {
    static Symbol ARRAY("ARRAY", true, "ARRAY");
    return ARRAY;
}

// keyword of
Symbol &snl::OF() {
    static Symbol OF("OF", true, "OF");
    return OF;
}

// keyword intc
Symbol &snl::INTC() {
    static Symbol INTC("INTC", true, "INTC");
    return INTC;
}

// keyword record
Symbol &snl::RECORD() {
    static Symbol RECORD("RECORD", true, "RECORD");
    return RECORD;
}

// keyword end
Symbol &snl::END() {
    static Symbol END("END", true, "END");
    return END;
}

// keyword var
Symbol &snl::VAR() {
    static Symbol VAR("VAR", true, "VAR");
    return VAR;
}

// keyword begin
Symbol &snl::BEGIN() {
    static Symbol BEGIN("BEGIN", true, "BEGIN");
    return BEGIN;
}

// keyword if
Symbol &snl::IF() {
    static Symbol IF("IF", true, "IF");
    return IF;
}

// keyword then
Symbol &snl::THEN() {
    static Symbol THEN("THEN", true, "THEN");
    return THEN;
}

// keyword else
Symbol &snl::ELSE() {
    static Symbol ELSE("ELSE", true, "ELSE");
    return ELSE;
}

// keyword fi
Symbol &snl::FI() {
    static Symbol FI("FI", true, "FI");
    return FI;
}

//keyword while
Symbol &snl::WHILE() {
    static Symbol WHILE("WHILE", true, "WHILE");
    return WHILE;
}

// keyword do
Symbol &snl::DO() {
    static Symbol DO("DO", true, "DO");
    return DO;
}

//keyword endwh
Symbol &snl::ENDWH() {
    static Symbol ENDWH("ENDWH", true, "ENDWH");
    return ENDWH;
}

//keyword read
Symbol &snl::READ() {
    static Symbol READ("READ", true, "READ");
    return READ;
}

//keyword write
Symbol &snl::WRITE() {
    static Symbol WRITE("WRITE", true, "WRITE");
    return WRITE;
}

//keyword return
Symbol &snl::RETURN() {
    static Symbol RETURN("RETURN", true, "RETURN");
    return RETURN;
}

// 非终极符
Symbol &snl::Program() {
    static Symbol Program("Program", false);
    return Program;
}

Symbol &snl::ProgramHead() {
    static Symbol ProgramHead("ProgramHead", false);
    return ProgramHead;
}

Symbol &snl::DeclarePart() {
    static Symbol DeclarePart("DeclarePart", false);
    return DeclarePart;
}

Symbol &snl::ProgramBody() {
    static Symbol ProgramBody("ProgramBody", false);
    return ProgramBody;
}

Symbol &snl::ProcDecPart() {
    static Symbol ProcDecPart("ProcDecPart", false);
    return ProcDecPart;
}

Symbol &snl::TypeDec() {
    static Symbol TypeDec("TypeDec", false);
    return TypeDec;
}

Symbol &snl::TypeDecList() {
    static Symbol TypeDecList("TypeDecList", false);
    return TypeDecList;
}

Symbol &snl::TypeId() {
    static Symbol TypeId("TypeId", false);
    return TypeId;
}

Symbol &snl::TypeName() {
    static Symbol TypeName("TypeName", false);
    return TypeName;
}

Symbol &snl::TypeDecMore() {
    static Symbol TypeDecMore("TypeDecMore", false);
    return TypeDecMore;
}

Symbol &snl::BaseType() {
    static Symbol BaseType("BaseType", false);
    return BaseType;
}

Symbol &snl::StructureType() {
    static Symbol StructureType("StructureType", false);
    return StructureType;
}

Symbol &snl::ArrayType() {
    static Symbol ArrayType("ArrayType", false);
    return ArrayType;
}

Symbol &snl::RecType() {
    static Symbol RecType("RecType", false);
    return RecType;
}

Symbol &snl::FieldDecList() {
    static Symbol FieldDecList("FieldDecList", false);
    return FieldDecList;
}

Symbol &snl::IdList() {
    static Symbol IdList("IdList", false);
    return IdList;
}

Symbol &snl::FieldDecMore() {
    static Symbol FieldDecMore("FieldDecMore", false);
    return FieldDecMore;
}

Symbol &snl::IdMore() {
    static Symbol IdMore("IdMore", false);
    return IdMore;
}

Symbol &snl::VarDec() {
    static Symbol VarDec("VarDec", false);
    return VarDec;
}

Symbol &snl::VarDecList() {
    static Symbol VarDecList("VarDecList", false);
    return VarDecList;
}

Symbol &snl::VarIdList() {
    static Symbol VarIdList("VarIdList", false);
    return VarIdList;
}

Symbol &snl::VarDecMore() {
    static Symbol VarDecMore("VarDecMore", false);
    return VarDecMore;
}

Symbol &snl::VarIdMore() {
    static Symbol VarIdMore("VarIdMore", false);
    return VarIdMore;
}

Symbol &snl::ProcDec() {
    static Symbol ProcDec("ProcDec", false);
    return ProcDec;
}

Symbol &snl::ParamList() {
    static Symbol ParamList("ParamList", false);
    return ParamList;
}

Symbol &snl::ProcBody() {
    static Symbol ProcBody("ProcBody", false);
    return ProcBody;
}

Symbol &snl::ProcDeclaration() {
    static Symbol ProcDeclaration("ProcDeclaration", false);
    return ProcDeclaration;
}

Symbol &snl::ParamDecList() {
    static Symbol ParamDecList("ParamDecList", false);
    return ParamDecList;
}

Symbol &snl::Param() {
    static Symbol Param("Param", false);
    return Param;
}

Symbol &snl::ParamMore() {
    static Symbol ParamMore("ParamMore", false);
    return ParamMore;
}

Symbol &snl::FormList() {
    static Symbol FormList("FormList", false);
    return FormList;
}

Symbol &snl::FidMore() {
    static Symbol FidMore("FidMore", false);
    return FidMore;
}

Symbol &snl::StmList() {
    static Symbol StmList("StmList", false);
    return StmList;
}

Symbol &snl::Stm() {
    static Symbol Stm("Stm", false);
    return Stm;
}

Symbol &snl::StmMore() {
    static Symbol StmMore("StmMore", false);
    return StmMore;
}

Symbol &snl::ConditionalStm() {
    static Symbol ConditionalStm("ConditionalStm", false);
    return ConditionalStm;
}

Symbol &snl::LoopStm() {
    static Symbol LoopStm("LoopStm", false);
    return LoopStm;
}

Symbol &snl::InputStm() {
    static Symbol InputStm("InputStm", false);
    return InputStm;
}

Symbol &snl::OutputStm() {
    static Symbol OutputStm("OutputStm", false);
    return OutputStm;
}

Symbol &snl::ReturnStm() {
    static Symbol ReturnStm("ReturnStm", false);
    return ReturnStm;
}

Symbol &snl::AssCall() {
    static Symbol AssCall("AssCall", false);
    return AssCall;
}

Symbol &snl::AssignmentRest() {
    static Symbol AssignmentRest("AssignmentRest", false);
    return AssignmentRest;
}

Symbol &snl::CallStmRest() {
    static Symbol CallStmRest("CallStmRest", false);
    return CallStmRest;
}

Symbol &snl::VariMore() {
    static Symbol VariMore("VariMore", false);
    return VariMore;
}

Symbol &snl::Exp() {
    static Symbol Exp("Exp", false);
    return Exp;
}

Symbol &snl::ActParamList() {
    static Symbol ActParamList("ActParamList", false);
    return ActParamList;
}

Symbol &snl::ActParamMore() {
    static Symbol ActParamMore("ActParamMore", false);
    return ActParamMore;
}

Symbol &snl::OtherExp() {
    static Symbol OtherExp("OtherExp", false);
    return OtherExp;
}

Symbol &snl::CmpOp() {
    static Symbol CmpOp("CmpOp", false);
    return CmpOp;
}

Symbol &snl::Term() {
    static Symbol Term("Term", false);
    return Term;
}

Symbol &snl::OtherTerm() {
    static Symbol OtherTerm("OtherTerm", false);
    return OtherTerm;
}

Symbol &snl::AddOp() {
    static Symbol AddOp("AddOp", false);
    return AddOp;
}

Symbol &snl::Factor() {
    static Symbol Factor("Factor", false);
    return Factor;
}

Symbol &snl::OtherFactor() {
    static Symbol OtherFactor("OtherFactor", false);
    return OtherFactor;
}

Symbol &snl::MultOp() {
    static Symbol MultOp("MultOp", false);
    return MultOp;
}

Symbol &snl::Variable() {
    static Symbol Variable("Variable", false);
    return Variable;
}

Symbol &snl::FieldVar() {
    static Symbol FieldVar("FieldVar", false);
    return FieldVar;
}

Symbol &snl::FieldVarMore() {
    static Symbol FieldVarMore("FieldVarMore", false);
    return FieldVarMore;
}

Symbol &snl::TypeDeclaration() {
    static Symbol TypeDeclaration("TypeDeclaration", false);
    return TypeDeclaration;
}

Symbol &snl::VarDeclaration() {
    static Symbol VarDeclaration("VarDeclaration", false);
    return VarDeclaration;
}

Symbol &snl::SimpleExp() {
    static Symbol SimpleExp("SimpleExp", false);
    return SimpleExp;
}


// 所有推导的链表
list<Derivation> snl::allDerivations;
// First集
map<Symbol, set<Symbol>> snl::FirstSet;
// Follow集
map<Symbol, set<Symbol>> snl::FollowSet;
// Predict集
map<Derivation, set<Symbol>> snl::PredictSet;

void snl::buildInit() {
    // 将所有推导放到链表里面
    auto b = [](const Symbol &s, const vector<Symbol> &v) {
        allDerivations.emplace_back(s, v);
    };
    b(Program(), {ProgramHead(), DeclarePart(), ProgramBody(), Dot()});
    b(ProgramHead(), {PROGRAM(), ID()});
    b(DeclarePart(), {TypeDec(), VarDec(), ProcDec()});
    b(TypeDec(), {Epsilon()});
    b(TypeDec(), {TypeDeclaration()});
    b(TypeDeclaration(), {TYPE(), TypeDecList()});
    b(TypeDecList(), {TypeId(), Equal(), TypeName(), Semicolon(), TypeDecMore()});
    b(TypeDecMore(), {Epsilon()});
    b(TypeDecMore(), {TypeDecList()});
    b(TypeId(), {ID()});
    b(TypeName(), {BaseType()});
    b(TypeName(), {StructureType()});
    b(TypeName(), {ID()});
    b(BaseType(), {INTEGER()});
    b(BaseType(), {CHAR()});
    b(StructureType(), {ArrayType()});
    b(StructureType(), {RecType()});
    b(ArrayType(), {ARRAY(), LeftBracket(), INTC(), TwoDots(), INTC(), RightBracket(), OF(), BaseType()});
    b(RecType(), {RECORD(), FieldDecList(), END()});
    b(FieldDecList(), {BaseType(), IdList(), Semicolon(), FieldDecMore()});
    b(FieldDecList(), {ArrayType(), IdList(), Semicolon(), FieldDecMore()});
    b(FieldDecMore(), {Epsilon()});
    b(FieldDecMore(), {FieldDecList()});
    b(IdList(), {ID(), IdMore()});
    b(IdMore(), {Epsilon()});
    b(IdMore(), {Comma(), IdList()});
    b(VarDec(), {Epsilon()});
    b(VarDec(), {VarDeclaration()});
    b(VarDeclaration(), {VAR(), VarDecList()});
    b(VarDecList(), {TypeName(), VarIdList(), Semicolon(), VarDecMore()});
    b(VarDecMore(), {Epsilon()});
    b(VarDecMore(), {VarDecList()});
    b(VarIdList(), {ID(), VarIdMore()});
    b(VarIdMore(), {Epsilon()});
    b(VarIdMore(), {Comma(), VarIdList()});
    b(ProcDec(), {Epsilon()});
    b(ProcDec(), {ProcDeclaration()});
    b(ProcDeclaration(), {PROCEDURE(),
                          ID(), LeftParenthesis(), ParamList(), RightParenthesis(), Semicolon(),
                          ProcDecPart(),
                          ProcBody(),
                          ProcDec()
    });
    b(ParamList(), {Epsilon()});
    b(ParamList(), {ParamDecList()});
    b(ParamDecList(), {Param(), ParamMore()});
    b(ParamMore(), {Epsilon()});
    b(ParamMore(), {Semicolon(), ParamDecList()});
    b(Param(), {TypeName(), FormList()});
    b(Param(), {VAR(), TypeName(), FormList()});
    b(FormList(), {ID(), FidMore()});
    b(FidMore(), {Epsilon()});
    b(FidMore(), {Comma(), FormList()});
    b(ProcDecPart(), {DeclarePart()});
    b(ProcBody(), {ProgramBody()});
    b(ProgramBody(), {BEGIN(), StmList(), END()});
    b(StmList(), {Stm(), StmMore()});
    b(StmMore(), {Epsilon()});
    b(StmMore(), {Semicolon(), StmList()});
    b(Stm(), {ConditionalStm()});
    b(Stm(), {LoopStm()});
    b(Stm(), {InputStm()});
    b(Stm(), {OutputStm()});
    b(Stm(), {ReturnStm()});
    b(Stm(), {ID(), AssCall()});
    b(AssCall(), {AssignmentRest()});
    b(AssCall(), {CallStmRest()});
    b(AssignmentRest(), {VariMore(), ColonEqual(), Exp()});
    b(ConditionalStm(), {IF(), Exp(), THEN(), StmList(), ELSE(), StmList(), FI()});
    b(LoopStm(), {WHILE(), Exp(), DO(), StmList(), ENDWH()});
    b(InputStm(), {READ(), LeftParenthesis(), ID(), RightParenthesis()});
    b(OutputStm(), {WRITE(), LeftParenthesis(), Exp(), RightParenthesis()});
    b(ReturnStm(), {RETURN()});
    b(CallStmRest(), {LeftParenthesis(), ActParamList(), RightParenthesis()});
    b(ActParamList(), {Epsilon()});
    b(ActParamList(), {Exp(), ActParamMore()});
    b(ActParamMore(), {Epsilon()});
    b(ActParamMore(), {Comma(), ActParamList()});
    b(Exp(), {SimpleExp(), OtherExp()});
    b(OtherExp(), {Epsilon()});
    b(OtherExp(), {CmpOp(), SimpleExp()});
    b(SimpleExp(), {Term(), OtherTerm()});
    b(OtherTerm(), {Epsilon()});
    b(OtherTerm(), {AddOp(), SimpleExp()});
    b(Term(), {Factor(), OtherFactor()});
    b(OtherFactor(), {Epsilon()});
    b(OtherFactor(), {MultOp(), Term()});
    b(Factor(), {LeftParenthesis(), Exp(), RightParenthesis()});
    b(Factor(), {INTC()});
    b(Factor(), {Variable()});
    b(Variable(), {ID(), VariMore()});
    b(VariMore(), {Epsilon()});
    b(VariMore(), {LeftBracket(), Exp(), RightBracket()});
    b(VariMore(), {Dot(), FieldVar()});
    b(FieldVar(), {ID(), FieldVarMore()});
    b(FieldVarMore(), {Epsilon()});
    b(FieldVarMore(), {LeftBracket(), Exp(), RightBracket()});
    b(CmpOp(), {LessThan()});
    b(CmpOp(), {Equal()});
    b(AddOp(), {Plus()});
    b(AddOp(), {Sub()});
    b(MultOp(), {Mul()});
    b(MultOp(), {Div()});
}

int snl::firstSize() {
    int cnt = 0;
    for (const auto &a : FirstSet) {
        cnt += static_cast<int>(a.second.size());
    }
    return cnt;
}

void snl::buildFirst() {
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
            if (d.symbolVector.front() == Epsilon()) {
                // symbol能推导出 空串
                sSet.insert(Epsilon());
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
                    // 如果前边的c的First集里边有Epsilon()，继续，下面判断当前c的First集中是否有Epsilon()
                    bool cEpsilon = false;
                    set<Symbol> cSet = FirstSet[c];
                    for (const Symbol &cSymbol : cSet) {
                        if (cSymbol == Epsilon()) {
                            cEpsilon = true;
                        } else {
                            // 把c的First集里的复制到symbol的First集里
                            sSet.insert(cSymbol);
                        }
                    }
                    // 即symbol->C1C2C3... C1的First集中没有Epsilon()的情形
                    if (!cEpsilon) {
                        break;
                    }
                    // 当前c的First集里边有Epsilon()，如果不是最后一个则继续往前走，否则将Epsilon()加入s的First集
                    // 即symbol->C1C2C3 C1、C2、C3的First集里都有Epsilon()的情形
                    if (c == d.symbolVector.back()) {
                        sSet.insert(Epsilon());
                    }
                    // 默认继续循环则是symbol->C1C2C3... C1的First集中有Epsilon()的情形
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

int snl::followSize() {
    int cnt = 0;
    for (const auto &a : FollowSet) {
        cnt += static_cast<int>(a.second.size());
    }
    return cnt;
}

void snl::buildFollow() {
    // 初始化为空集
    for (const Derivation &d : allDerivations) {
        FollowSet[d.head] = set<Symbol>{};
    }
    // 文法开始符号的Follow集中有一个#
    FollowSet[allDerivations.front().head] = {Sharp()};
    int sizeL = 0;
    int sizeR = followSize();
    /* 这个链表存储所有类似 c->...ab...的推导中的a、b，即a、b满足关系First(b)-{Epsilon()}∈Follow(a)
         * 特殊情况比如 c->...a1a2a3b...同时a1、a2、a3能推导出空串，则有
         * First(a2)-{Epsilon()}∈Follow(a1)
         * First(a3)-{Epsilon()}∈Follow(a1)
         * First(b)-{Epsilon()}∈Follow(a1)
         */
    list<pair<Symbol, Symbol>> ab;
    // 遍历所有推导公式 多遍直到没有新元素加入Follow集
    while (sizeL != sizeR) {
        for (const Derivation &d : allDerivations) {
            // 当S2 ->* Epsilon()时，First[S3]-{Epsilon()}也∈Follow[S1]
            for (int i = 0; i < static_cast<int>(d.symbolVector.size() - 1); i++) {
                // W -> ...S1S2...
                Symbol s1 = d.symbolVector[static_cast<unsigned int>(i)],
                        s2 = d.symbolVector[static_cast<unsigned int>(i + 1)];
                // 终极符没有Follow集
                if (s1.terminate)continue;
                if (s2.terminate) {
                    // First(s2)-{Epsilon()}∈Follow(s1)
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
                // First(s2)-{Epsilon()}∈Follow(s1)
                ab.emplace_back(pair<Symbol, Symbol>(s1, s2));
                for (const Symbol &smb : FirstSet[s2]) {
                    if (smb != Epsilon()) {
                        FollowSet[s1].insert(smb);
                    } else {
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
                            if (sss == Epsilon()) {
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
                // First(b)-{Epsilon()}∈Follow(a)
                for (const Symbol &s : FirstSet[b]) {
                    if (s != Epsilon()) {
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
                // 判断s2能否推出Epsilon()，如果是则继续循环，否则退出循环
                bool bEpsilon = false;
                for (const Derivation &derivation : allDerivations) {
                    if (derivation.head != s2)continue;
                    if (derivation.symbolVector.size() != 1)continue;
                    if (derivation.symbolVector[0] != Epsilon())continue;
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

set<Symbol> snl::getFirstBySymbol(const Symbol &s) {
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

void snl::buildPredict() {
    for (const Derivation &d : allDerivations) {
        // Predict(A->B)=First(B), if ε?First(B)
        //              =(First(B)-{ε})∪Follow(A),if ε∈First(B)
        // 先求First(B)
        set<Symbol> firstB = getFirstBySymbol(d.symbolVector[0]);
        int i = 1;
        while (firstB.find(Epsilon()) != firstB.end()) {
            if (i < static_cast<int>(d.symbolVector.size())) {
                firstB.erase(Epsilon());
                set<Symbol> f2 = getFirstBySymbol(d.symbolVector[static_cast<unsigned int>(i)]);
                for (const Symbol &smb : f2) {
                    firstB.insert(smb);
                }
            } else break;
            i++;
        }
        if (firstB.find(Epsilon()) == firstB.end()) {
            // ε?First(B)
            PredictSet[d] = firstB;
        } else {
            // ε∈First(B)
            firstB.erase(Epsilon());
            for (const Symbol &smb : FollowSet[d.head]) {
                firstB.insert(smb);
            }
            PredictSet[d] = firstB;
        }
    }
}

string snl::up(const string &x) {
    string y = x;
    transform(y.begin(), y.end(), y.begin(), ::toupper);
    return y;
}

vector<TokenSymbol> snl::SnlGrammarAnalyser::getTokenList() {
    return tokenSymbols;
}

void snl::SnlGrammarAnalyser::init() {
    if (initialized)return;
    buildInit();
    buildFirst();
    buildFollow();
    buildPredict();
    initialized = true;
}

int snl::SnlGrammarAnalyser::parse(const char *code) {
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
                default:
                    errMsg += "Unknown error";
                    return E_UNKNOWN;
            }
        } else {
            code += r;
        }
    }

    convertWordsToSymbols();

    // 然后进行语法分析
    return grammarAnalysis();
}

Word::Word(string s, int i) : type(i), v(std::move(s)), line(global_line_number) {}

string Word::getType(int i) {
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

ostream &snl::operator<<(ostream &os, const Word &word) {
    os << "[" << word.getType(word.type) << "]" << setw(4) << word.line << "{" << word.v << "}";
    return os;
}

Symbol::Symbol(string name, bool terminate) : name(std::move(name)), terminate(terminate), v("") {}

Symbol::Symbol(string name, bool terminate, string v) : name(std::move(name)), terminate(terminate),
                                                        v(std::move(v)) {}

ostream &snl::operator<<(ostream &os, const Symbol &symbol) {
    os << setw(10) << symbol.name << " " << (symbol.terminate ? "T" : "N");
    if (symbol.v != symbol.name)
        os << " " << setw(10) << symbol.v;
    return os;
}

bool Symbol::operator==(const Symbol &rhs) const {
    return name == rhs.name &&
           terminate == rhs.terminate &&
           v == rhs.v;
}

bool Symbol::operator!=(const Symbol &rhs) const {
    return !(rhs == *this);
}

bool Symbol::operator<(const Symbol &rhs) const {
    return name < rhs.name;
}

bool Symbol::operator>(const Symbol &rhs) const {
    return rhs < *this;
}

bool Symbol::operator<=(const Symbol &rhs) const {
    return !(rhs < *this);
}

bool Symbol::operator>=(const Symbol &rhs) const {
    return !(*this < rhs);
}

Derivation::Derivation(Symbol head, vector<Symbol> symbolVector) : head(std::move(head)),
                                                                   symbolVector(std::move(symbolVector)) {}

bool Derivation::operator==(const Derivation &rhs) const {
    return head == rhs.head &&
           symbolVector == rhs.symbolVector;
}

bool Derivation::operator!=(const Derivation &rhs) const {
    return !(rhs == *this);
}

ostream &snl::operator<<(ostream &os, const Derivation &derivation) {
    os << "[" << derivation.head.name << "] -> [ ";
    for (const Symbol &s : derivation.symbolVector) {
        os << s.name << " ";
    }
    os << "]";
    return os;
}

bool Derivation::operator<(const Derivation &rhs) const {
    if (head < rhs.head)
        return true;
    if (rhs.head < head)
        return false;
    return symbolVector < rhs.symbolVector;
}

bool Derivation::operator>(const Derivation &rhs) const {
    return rhs < *this;
}

bool Derivation::operator<=(const Derivation &rhs) const {
    return !(rhs < *this);
}

bool Derivation::operator>=(const Derivation &rhs) const {
    return !(*this < rhs);
}

TokenSymbol::TokenSymbol(Symbol s, int line) : s(std::move(s)), line(line) {}

ostream &snl::operator<<(ostream &os, const TokenSymbol &symbol) {
    os << setw(4) << symbol.line << symbol.s;
    return os;
}

Node::Node(Symbol s, Node *parent, int id) : curr(std::move(s)), parent(parent), id(id) {}

string snl::SnlGrammarAnalyser::getError() { return errMsg; }

string snl::SnlGrammarAnalyser::getTree() {
    if (!initialized)return "";
    if (!errMsg.empty())return "";
    if (vTree.empty())return "";

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
    for (auto &i : root->children) {
        if (i->curr.terminate)continue;
        dfsBuildTree(i);
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

        if (c->children.empty()) {
            // 叶子结点
            continue;
        }

        // 跟其他节点的关系
        string children;
        for (auto &i : c->children) {
            children += "\"" + to_string(i->id) + "\"; "; // "id; "
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

void snl::SnlGrammarAnalyser::dfsBuildTree(Node *&parent) {
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
    for (const auto &i : vTree[treeIndex].symbolVector) {
        parent->children.emplace_back(new Node(i, parent, global_id++));
    }
    // 构造完推导式下标就加加
    treeIndex++;
    for (auto &i : parent->children) {
        if (i->curr.terminate) {
            // 将ID转化为实际值
            if (i->curr == Epsilon()) {}
            else {
                //cout << tokenSymbols[global_token_index] <<"\t\ttokenlist"<< endl;
                //cout << parent->children[i]->curr << endl;
                //cout << endl;
                i->curr.v = tokenSymbols[global_token_index].s.v;
                global_token_index++;
            }
            continue;// 终极符是叶节点
        }
        dfsBuildTree(i);
    }
}

int snl::SnlGrammarAnalyser::grammarAnalysis() {
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

void snl::SnlGrammarAnalyser::pushWord(const char *s, int l, int type) {
    strncpy(buf, s, static_cast<unsigned int>(l));
    buf[l] = '\0';
    words.emplace_back(Word(buf, type));
}

int snl::SnlGrammarAnalyser::takeIdentifier(const char *s) {
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

int snl::SnlGrammarAnalyser::takeNum(const char *s) {
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

int snl::SnlGrammarAnalyser::takeOneChar(const char *s) {
    pushWord(s, 1, TYPE_SINGLE_CHAR);
    return 1;
}

int snl::SnlGrammarAnalyser::takeTwoChar(const char *s) {
    if (s[1] == '=') {
        pushWord(s, 2, TYPE_EQUAL);
        return 2;
    }
    return ERROR_TWO_CHAR;
}

int snl::SnlGrammarAnalyser::takeComment(const char *s) {
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

int snl::SnlGrammarAnalyser::takeDot(const char *s) {
    if (s[1] == '.') {
        pushWord(s, 2, TYPE_ARRAY);
        return 2;
    }
    pushWord(s, 1, TYPE_SINGLE_CHAR);
    return 1;
}

int snl::SnlGrammarAnalyser::takeWhiteSpace(const char *s) {
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

int snl::SnlGrammarAnalyser::analysisOneWord(const char *s) {
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

void snl::SnlGrammarAnalyser::convertWordsToSymbols() {
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
                auto b = [](const Symbol &s, int l, vector<TokenSymbol> &t) {
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
