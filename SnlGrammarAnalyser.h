#ifndef SNL_SNLGRAMMARANALYSER_H
#define SNL_SNLGRAMMARANALYSER_H

/***
 * �ؼ��ֲ����ִ�Сд ID���ִ�Сд snl��֧���ַ��� ��˲������ַ���
 * �������Ƶ�ʽ�еĴ��� ����LL(1)������ �����First Follow��Predict��
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

namespace SnlGrammarAnalyserNamespace {

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


    // ���ظ��ϲ�Ĵ���ֵ
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
        // ID �� �ؼ���
                TYPE_IDENTIFIER = 1,
        // INTC
                TYPE_UNSIGNED_INT,
        // ���ַ��ֽ��
                TYPE_SINGLE_CHAR,
        // ˫�ַ��ֽ���е� :=
                TYPE_EQUAL,
        // ע��
                TYPE_COMMENT,
        // ˫�ַ��ֽ���е� ..
                TYPE_ARRAY,
        // �հ�
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

    static Symbol
    // �ռ��� �մ�
            Epsilon("Epsilon", true, ""),

    // �ռ��� ���ַ��ֽ��
            Plus("+", true, "+"),//�Ӻ� +
            Sub("-", true, "-"),//���� -
            Mul("*", true, "*"),//�˺� *
            Div("/", true, "/"),//���� /
            LessThan("<", true, "<"),//С�ں� <
            Equal("=", true, "="),// ���� =
            LeftParenthesis("(", true, "("), // ��С���� (
            RightParenthesis(")", true, ")"), // ��С���� )
            LeftBracket("[", true, "["),//�������� [
            RightBracket("]", true, "]"),//�������� ]
            Dot(".", true, "."),//���ַ��ֽ�� . һ���� ���ڽṹ��
            Semicolon(";", true, ";"),// �ֺ� ;
            Comma(",", true, ","),// ���� ,

    // �ռ��� ˫�ַ��ֽ��
            TwoDots("..", true, ".."),//�����±�ָ��� ������ ..
            ColonEqual(":=", true, ":="),// ��ֵ���� :=

    // �ռ��� #
            Sharp("#", true, "#"),//��������First���У�������Follow����Predict����

    // �ռ��� �ؼ���
            ID("ID", true, "ID"),
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

    // ���ռ���
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

    char buf[4096] = {0};


    // �Ƶ� �Ľṹ�� ���� F->(E)
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

    // �����Ƶ�������
    list<Derivation> allDerivations;
    // First��
    map<Symbol, set<Symbol>> FirstSet;
    // Follow��
    map<Symbol, set<Symbol>> FollowSet;
    // Predict��
    map<Derivation, set<Symbol>> PredictSet;

    void buildInit() {
        // �������Ƶ��ŵ���������
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
        for (const auto &a : FirstSet) {
            cnt += (int) a.second.size();
        }
        return cnt;
    }

    /**
     * ����First��
     */
    void buildFirst() {
        int lastSize = -1;
        int newSize = 0;
        // �������洢���� First(b) �� First(a) �Ĺ�ϵ
        // ������ʽΪ a->b... ���� ( a->c1c2c3b... ����c1c2c3�����Ƶ����մ� ��������ȼ��� a->b... )
        list<pair<Symbol, Symbol>> a_to_b;
        while (lastSize != newSize) {
            for (const Derivation &d : allDerivations) {
                // ��d.head��First��
                Symbol symbol = d.head;
                /* 1. sһ���Ƿ��ռ�������Ϊs���Ƶ�����ʽ�����
                 * 2. First(X)={a|X->a...���ķ���һ������ʽ��a���ռ���}
                 * 3. ����X->Epsilon����{Epsilon}��First(X)
                 * 4. �в���ʽX->Y1Y2Y3...Yn����Y1��Y2��Y3��...��Yi�ʷ��ռ���
                 */
                set<Symbol> sSet = FirstSet[symbol];
                // �����е�set�����ӣ������ɺ��ٷŻ�FirstSetSymbol��
                if (d.symbolVector.front() == Epsilon) {
                    // symbol���Ƶ��� �մ�
                    sSet.insert(Epsilon);
                } else if (d.symbolVector.front().terminate) {
                    // symbol���Ƶ��� �ռ�����ͷ�Ĵ�
                    sSet.insert(d.symbolVector.front());
                } else {
                    // symbol���Ƶ��� ���ռ�����ͷ�Ĵ�
                    for (const Symbol &c : d.symbolVector) {
                        // c��symbol�Ƶ����Ĵ���ÿ������ symbol->C1C2C3...Cn
                        // �����ǰ��c���ս�������
                        if (c.terminate) {
                            sSet.insert(c);
                            break;
                        }
                        a_to_b.emplace_back(pair<Symbol, Symbol>(symbol, c));
                        // ���ǰ�ߵ�c��First�������Epsilon�������������жϵ�ǰc��First�����Ƿ���Epsilon
                        bool cEpsilon = false;
                        set<Symbol> cSet = FirstSet[c];
                        for (const Symbol &cSymbol : cSet) {
                            if (cSymbol == Epsilon) {
                                cEpsilon = true;
                            } else {
                                // ��c��First����ĸ��Ƶ�symbol��First����
                                sSet.insert(cSymbol);
                            }
                        }
                        // ��symbol->C1C2C3... C1��First����û��Epsilon������
                        if (!cEpsilon) {
                            break;
                        }
                        // ��ǰc��First�������Epsilon������������һ���������ǰ�ߣ�����Epsilon����s��First��
                        // ��symbol->C1C2C3 C1��C2��C3��First���ﶼ��Epsilon������
                        if (c == d.symbolVector.back()) {
                            sSet.insert(Epsilon);
                        }
                        // Ĭ�ϼ���ѭ������symbol->C1C2C3... C1��First������Epsilon������
                    }
                }
                // First��������ϣ��ŵ�ȫ�ֱ������
                FirstSet[symbol] = sSet;
            }
            int innerLastSize = firstSize() - 1;
            int innerNewSize = innerLastSize + 1;
            while (innerLastSize != innerNewSize) {
                for (const auto &t : a_to_b) {
                    Symbol a = t.first, b = t.second;
                    // First(b) �� First(a)
                    for (const auto &s : FirstSet[b]) {
                        FirstSet[a].insert(s);
                    }
                }
                innerLastSize = innerNewSize;
                innerNewSize = firstSize();
            }
            // �Աȴ˴�ѭ����û�������µ�
            lastSize = newSize;
            newSize = firstSize();
        }
    }

    /**
     * FollowSet��set<Symbol>������Ԫ�ظ���
     * @return FollowSet��set<Symbol>������Ԫ�ظ���
     */
    int followSize() {
        int cnt = 0;
        for (const auto &a : FollowSet) {
            cnt += (int) a.second.size();
        }
        return cnt;
    }

    /**
     * ����Follow��
     */
    void buildFollow() {
        // ��ʼ��Ϊ�ռ�
        for (const Derivation &d : allDerivations) {
            FollowSet[d.head] = set<Symbol>{};
        }
        // �ķ���ʼ���ŵ�Follow������һ��#
        FollowSet[allDerivations.front().head] = {Sharp};
        int sizeL = 0;
        int sizeR = followSize();
        /* �������洢�������� c->...ab...���Ƶ��е�a��b����a��b�����ϵFirst(b)-{Epsilon}��Follow(a)
         * ����������� c->...a1a2a3b...ͬʱa1��a2��a3���Ƶ����մ�������
         * First(a2)-{Epsilon}��Follow(a1)
         * First(a3)-{Epsilon}��Follow(a1)
         * First(b)-{Epsilon}��Follow(a1)
         */
        list<pair<Symbol, Symbol>> ab;
        // ���������Ƶ���ʽ ���ֱ��û����Ԫ�ؼ���Follow��
        while (sizeL != sizeR) {
            for (const Derivation &d : allDerivations) {
                // ��S2 ->* Epsilonʱ��First[S3]-{Epsilon}Ҳ��Follow[S1]
                for (int i = 0; i < (int) d.symbolVector.size() - 1; i++) {
                    // W -> ...S1S2...
                    Symbol s1 = d.symbolVector[i],
                            s2 = d.symbolVector[i + 1];
                    // �ռ���û��Follow��
                    if (s1.terminate)continue;
                    if (s2.terminate) {
                        // First(s2)-{Epsilon}��Follow(s1)
                        FollowSet[s1].insert(s2);
                        continue;
                    }
                    // S1 S2���Ƿ��ռ���
                    auto it = FirstSet.find(s2);
                    if (it == FirstSet.end()) {
                        cerr << "[error]{" << __LINE__ << "} Build Follow set for " << s1 << endl;
                        cerr << s2 << " has no First set" << endl;
                        exit(-1);
                    }
                    bool bEpsilon = false;
                    // First(s2)-{Epsilon}��Follow(s1)
                    ab.emplace_back(pair<Symbol, Symbol>(s1, s2));
                    for (const Symbol &smb : FirstSet[s2]) {
                        if (smb != Epsilon) {
                            FollowSet[s1].insert(smb);
                        } else {
                            bEpsilon = true;
                        }
                    }
                    if (bEpsilon) {
                        // j�ĳ�ʼֵλ���� W��>...S1S2S3S4... �е�S3
                        for (int j = i + 2; j < (int) d.symbolVector.size(); j++) {
                            // ���s3���ռ���ֱ�ӽ���
                            if (d.symbolVector[j].terminate) {
                                FollowSet[s1].insert(d.symbolVector[j]);
                                break;
                            }
                            // ��ʱs3ȷ�������ս��
                            ab.emplace_back(s1, d.symbolVector[j]);
                            bool hasEpsilon = false;
                            // �ж�s3�Ƿ����Ƶ����մ�
                            for (const Symbol &sss : FirstSet[d.symbolVector[j]]) {
                                if (sss == Epsilon) {
                                    hasEpsilon = true;
                                    break;
                                }
                            }
                            // ���s3�����Ƶ����մ����˳�
                            if (!hasEpsilon)break;
                        }
                    }
                }
                for (const auto &t : ab) {
                    Symbol a = t.first;
                    Symbol b = t.second;
                    // First(b)-{Epsilon}��Follow(a)
                    for (const Symbol &s : FirstSet[b]) {
                        if (s != Epsilon) {
                            FollowSet[a].insert(s);
                        }
                    }
                }
                for (int i = (int) d.symbolVector.size() - 1; i >= 0; i--) {
                    // W -> ...S1S2
                    // Follow(W)��Follow(S2) ��Follow(W)���뵽Follow(S2)��
                    Symbol s2 = d.symbolVector[i];
                    if (s2.terminate)break;
                    for (const Symbol &smb : FollowSet[d.head]) {
                        FollowSet[s2].insert(smb);
                    }
                    // �ж�s2�ܷ��Ƴ�Epsilon������������ѭ���������˳�ѭ��
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
     * ����Predict��
     */
    void buildPredict() {
        for (const Derivation &d : allDerivations) {
            // Predict(A->B)=First(B), if ��?First(B)
            //              =(First(B)-{��})��Follow(A),if �š�First(B)
            // ����First(B)
            set<Symbol> firstB = getFirstBySymbol(d.symbolVector[0]);
            int i = 1;
            while (firstB.find(Epsilon) != firstB.end()) {
                if (i < (int) d.symbolVector.size()) {
                    firstB.erase(Epsilon);
                    set<Symbol> f2 = getFirstBySymbol(d.symbolVector[i]);
                    for (const Symbol &smb : f2) {
                        firstB.insert(smb);
                    }
                } else break;
                i++;
            }
            if (firstB.find(Epsilon) == firstB.end()) {
                // ��?First(B)
                PredictSet[d] = firstB;
            } else {
                // �š�First(B)
                firstB.erase(Epsilon);
                for (const Symbol &smb : FollowSet[d.head]) {
                    firstB.insert(smb);
                }
                PredictSet[d] = firstB;
            }
        }
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


    string up(const string &x) {
        string y = x;
        transform(y.begin(), y.end(), y.begin(), ::toupper);
        return y;
    }


    static bool initialized = false;

    // parse֮ǰ��initһ�¾Ϳ����ˣ���������������getError()�鿴����ԭ��
    class SnlGrammarAnalyser {
    public:
        void init() {
            if (initialized)return;
            buildInit();
            buildFirst();
            buildFollow();
            buildPredict();
            initialized = true;
        }

        int parse(const char *code) {
            errMsg = "";
            if (!initialized)return -1;
            // �Ƚ��дʷ�����
            global_line_number = 1;
            while (*code != '\0') {
                int r = analysisOneWord(code);
                if (r <= 0) {
                    errMsg = "[�ʷ���������] �к�" + to_string(global_line_number) + " ��ǰ����������" + *code + " ";
                    // ���
                    words.clear();
                    tokenSymbols.clear();
                    // ����
                    switch (r) {
                        case ERROR_COMMENT:
                            errMsg += "��һ���Ҵ����ŵ���ע��û�бպ�";
                            return E_COMMENT_NOT_TERMINATED;
                        case ERROR_TWO_CHAR:
                            errMsg += "ð�Ÿ��еȺ�ȴû��";
                            return E_ONLY_COLON_BUT_NO_EQUAL;
                        case ERROR_UNEXPECTED_CHAR:
                            errMsg += "������snl�ķ��涨����ķ���";
                            return E_UNEXPECTED_CHAR;
                    }
                    return E_UNKNOWN;
                } else {
                    code += r;
                }
            }
            convertWordsToSymbols();

            // Ȼ������﷨����
            int r = grammarAnalysis();
            if (r != 0) {
                // ���
                words.clear();
                tokenSymbols.clear();
                return r;
            }

            // ���
            words.clear();
            tokenSymbols.clear();

            return 0;
        }

        string getError() { return errMsg; }

    private:
        string errMsg;
        list<Word> words;
        vector<TokenSymbol> tokenSymbols;

        /**
         * �﷨����
         */
        int grammarAnalysis() {
            // �� ����ջ #E   ������ i+i*i#
            list<Symbol> s;//��Ϊջ���ܱ������������list�������
            s.emplace_back(Sharp); // #
            s.emplace_back(allDerivations.front().head); // �ķ���ʼ��
            tokenSymbols.emplace_back(TokenSymbol(Sharp, numeric_limits<int>::max()));
            int i = 0;//����������
            while (!s.empty()) {
                Symbol t = s.back();//ջ��Ԫ��
                Symbol r = tokenSymbols[i].s;//��������ǰɨ�����
                if (!t.terminate) {
                    //���ռ��� Ѱ�� t->ɶ ��Predict�� ������������ǰ��������
                    bool notFound = true;
                    for (const auto &a : PredictSet) {
                        if (a.first.head != t)continue;
                        set<Symbol>::iterator f;
                        if (r.name == "ID") {
                            f = a.second.find(ID);
                        } else {
                            f = a.second.find(r);
                        }
                        if (f == a.second.end())continue;
                        // �ҵ��� t->a.second��Predict������ r
                        notFound = false;
                        s.pop_back();//�ȵ�ջ
                        // Ȼ���Ƶ�ʽ������ջ
                        for (auto it = a.first.symbolVector.rbegin(); it != a.first.symbolVector.rend(); it++) {
                            s.emplace_back(*it);
                        }
                        break;
                    }
                    if (notFound) {
                        // ����
                        errMsg = "[�﷨��������] �к�" + to_string(tokenSymbols[i].line) + " ��ǰ����������" + r.v;
                        return E_GRAMMAR;
                        //cerr << "[error] " << __LINE__ << " " << t << " can not predict " << r << endl;
                        //cerr << "at line " << tokenSymbols[i].line << endl;
                        //exit(-1);
                    } else {
                        continue;// ����
                    }
                } else {
                    if (t.name == "ID") {
                        // �ǹؼ��ֵı�ʶ��
                        if (r.name == "ID") {
                            // ƥ����!!!
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            errMsg = "[�﷨��������] �к�" + to_string(tokenSymbols[i].line) + " ��ǰ����������" + r.v;
                            return E_GRAMMAR;
                            // ����
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }
                    } else if (t == Epsilon) {
                        // �մ���ֱ�ӵ�ջ����
                        s.pop_back();
                        continue;
                    } else if (t.name == "INTC") {
                        // �޷�������
                        if (r.name == "INTC") {
                            // ƥ����!!!
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            errMsg = "[�﷨��������] �к�" + to_string(tokenSymbols[i].line) + " ��ǰ����������" + r.v;
                            return E_GRAMMAR;
                            // ����
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }

                    } else {
                        // ������
                        if (t == r) {
                            // ƥ����!!!
                            if (t == Sharp) {
                                // �����!!!
                                break;
                            }
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            // ����
                            errMsg = "[�﷨��������] �к�" + to_string(tokenSymbols[i].line) + " ��ǰ����������" + r.v;
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
            strncpy(buf, s, l);
            buf[l] = '\0';
            words.emplace_back(Word(buf, type));
        }

        /**
         * ��ʶ��
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
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
         * ������
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
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
         * ����.����ĵ��ַ��ֽ��
         * @param s �������ַ���
         * @return 1
         */
        int takeOneChar(const char *s) {
            pushWord(s, 1, TYPE_SINGLE_CHAR);
            return 1;
        }

        /**
         * ˫�ַ��ֽ�� :=
         * @param s �������ַ���
         * @return ����ɹ����ַ������� 2 �� ʧ��ԭ��
         */
        int takeTwoChar(const char *s) {
            if (s[1] == '=') {
                pushWord(s, 2, TYPE_EQUAL);
                return 2;
            }
            return ERROR_TWO_CHAR;
        }

        /**
         * ע�� {comment}
         * @param s �������ַ���
         * @return ����ɹ����ַ�������(���ڵ���2) �� ʧ��ԭ��
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
         * һ���� . ���������� ..
         * @param s �������ַ���
         * @return ����ɹ����ַ������� 1 �� 2
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
         * �հ׷�
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
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
                // ��ĸ��ͷ			���� ���ڵ���1
                c = takeIdentifier(s);
            } else if (s[0] >= '0' && s[0] <= '9') {
                // ���ִ�ͷ			���� ���ڵ���1
                c = takeNum(s);
            } else if (
                    s[0] == '+' || s[0] == '-' || s[0] == '*' || s[0] == '/' ||
                    s[0] == '(' || s[0] == ')' || s[0] == '[' || s[0] == ']' ||
                    s[0] == ';' || s[0] == '<' || s[0] == '=' || s[0] == ','
                    ) {
                // �����ַ�			���� ���ڵ���1
                c = takeOneChar(s);
            } else if (s[0] == ':') {
                // ˫�ַ� :=			���� 2 �� ����
                c = takeTwoChar(s);
            } else if (s[0] == '{') {
                // ע��				���� ���ڵ���2 �� ����
                c = takeComment(s);
            } else if (s[0] == '.') {
                // �����ǵ��ַ��е� .	���� 1 �� 2
                // Ҳ������˫�ַ��е������±���޷� ..
                c = takeDot(s);
            } else if (s[0] == ' ' || s[0] == '\t' || s[0] == '\n') {
                // �հ׷�			���� ���ڵ���1
                c = takeWhiteSpace(s);
            } else {
                return ERROR_UNEXPECTED_CHAR;
            }
            return c;
        }

        /**
         * ���ʷ���������������Word����ת��ΪSymbol����
         * ȥ��ע�ͺͿհ�
         */
        void convertWordsToSymbols() {
            for (const Word &w : words) {
                switch (w.type) {
                    // ����ע�ͺͿհ׷�
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
                        auto b = [](Symbol s, int l, vector<TokenSymbol> &t) {
                            t.emplace_back(s, l);
                        };
                        if (v == PROCEDURE.v) {
                            b(PROCEDURE, l, tokenSymbols);
                        } else if (v == PROGRAM.v) {
                            b(PROGRAM, l, tokenSymbols);
                        } else if (v == TYPE.v) {
                            b(TYPE, l, tokenSymbols);
                        } else if (v == CHAR.v) {
                            b(CHAR, l, tokenSymbols);
                        } else if (v == INTEGER.v) {
                            b(INTEGER, l, tokenSymbols);
                        } else if (v == ARRAY.v) {
                            b(ARRAY, l, tokenSymbols);
                        } else if (v == OF.v) {
                            b(OF, l, tokenSymbols);
                        } else if (v == RECORD.v) {
                            b(RECORD, l, tokenSymbols);
                        } else if (v == END.v) {
                            b(END, l, tokenSymbols);
                        } else if (v == VAR.v) {
                            b(VAR, l, tokenSymbols);
                        } else if (v == BEGIN.v) {
                            b(BEGIN, l, tokenSymbols);
                        } else if (v == IF.v) {
                            b(IF, l, tokenSymbols);
                        } else if (v == THEN.v) {
                            b(THEN, l, tokenSymbols);
                        } else if (v == ELSE.v) {
                            b(ELSE, l, tokenSymbols);
                        } else if (v == FI.v) {
                            b(FI, l, tokenSymbols);
                        } else if (v == WHILE.v) {
                            b(WHILE, l, tokenSymbols);
                        } else if (v == DO.v) {
                            b(DO, l, tokenSymbols);
                        } else if (v == ENDWH.v) {
                            b(ENDWH, l, tokenSymbols);
                        } else if (v == READ.v) {
                            b(READ, l, tokenSymbols);
                        } else if (v == WRITE.v) {
                            b(WRITE, l, tokenSymbols);
                        } else if (v == RETURN.v) {
                            b(RETURN, l, tokenSymbols);
                        } else {
                            // ���ǹؼ��� ��ת���ɴ�д
                            b(Symbol("ID", true, w.v), l, tokenSymbols);
                        }
                        break;
                    }
                }
            }
        }

    };

}

using SnlGrammarAnalyserNamespace::SnlGrammarAnalyser;
#endif //SNL_SNLGRAMMARANALYSER_H