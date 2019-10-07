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

    // �ռ��� �մ�
    Symbol &Epsilon();

    // �ռ��� ���ַ��ֽ��
    Symbol &Plus();//�Ӻ� +
    Symbol &Sub();//���� -
    Symbol &Mul();//�˺� *
    Symbol &Div();//���� /
    Symbol &LessThan();//С�ں� <
    Symbol &Equal();// ���� =
    Symbol &LeftParenthesis(); // ��С���� (
    Symbol &RightParenthesis(); // ��С���� )
    Symbol &LeftBracket();//�������� [
    Symbol &RightBracket();//�������� ]
    Symbol &Dot();//���ַ��ֽ�� . һ���� ���ڽṹ��
    Symbol &Semicolon();// �ֺ� ;
    Symbol &Comma();// ���� ,

    // �ռ��� ˫�ַ��ֽ��
    Symbol &TwoDots();//�����±�ָ��� ������ ..
    Symbol &ColonEqual();// ��ֵ���� :=

    // �ռ��� #
    Symbol &Sharp();//��������First���У�������Follow����Predict����

    // �ռ��� �ؼ���
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

    // ���ռ���
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

    // �Ƶ� �Ľṹ�� ���� F->(E)
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

    // �����Ƶ�������
    extern list<Derivation> allDerivations;
    // First��
    extern map<Symbol, set<Symbol>> FirstSet;
    // Follow��
    extern map<Symbol, set<Symbol>> FollowSet;
    // Predict��
    extern map<Derivation, set<Symbol>> PredictSet;

    void buildInit();

    int firstSize();

    /**
     * ����First��
     */
    void buildFirst();

    /**
     * FollowSet��set<Symbol>������Ԫ�ظ���
     * @return FollowSet��set<Symbol>������Ԫ�ظ���
     */
    int followSize();

    /**
     * ����Follow��
     */
    void buildFollow();

    set<Symbol> getFirstBySymbol(const Symbol &s);

    /**
     * ����Predict��
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

    // parse֮ǰ��initһ�¾Ϳ����ˣ���������������getError()�鿴����ԭ��
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
        vector<Derivation> vTree;// ��Vector��ʾ���﷨��
        unsigned int treeIndex;// ָʾvTree���±�
        Node *root;// ����������ʾ���﷨��
        int global_id;//DOT Language��ÿ���ڵ��id
        unsigned int global_token_index;//���ڽ�IDת��Ϊ���������ߺ�������
        void dfsBuildTree(Node *&parent);

        /**
         * �﷨����
         */
        int grammarAnalysis();

        void pushWord(const char *s, int l, int type);

        /**
         * ��ʶ��
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
         */
        int takeIdentifier(const char *s);

        /**
         * ������
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
         */
        int takeNum(const char *s);

        /**
         * ����.����ĵ��ַ��ֽ��
         * @param s �������ַ���
         * @return 1
         */
        int takeOneChar(const char *s);

        /**
         * ˫�ַ��ֽ�� :=
         * @param s �������ַ���
         * @return ����ɹ����ַ������� 2 �� ʧ��ԭ��
         */
        int takeTwoChar(const char *s);

        /**
         * ע�� {comment}
         * @param s �������ַ���
         * @return ����ɹ����ַ�������(���ڵ���2) �� ʧ��ԭ��
         */
        int takeComment(const char *s);

        /**
         * һ���� . ���������� ..
         * @param s �������ַ���
         * @return ����ɹ����ַ������� 1 �� 2
         */
        int takeDot(const char *s);

        /**
         * �հ׷�
         * @param s �������ַ���
         * @return ����ɹ����ַ������� ���ڵ���1
         */
        int takeWhiteSpace(const char *s);

        int analysisOneWord(const char *s);

        /**
         * ���ʷ���������������Word����ת��ΪSymbol����
         * ȥ��ע�ͺͿհ�
         */
        void convertWordsToSymbols();
    };
}

using snl::SnlGrammarAnalyser;

#endif //SNL_SNLGRAMMARANALYSER_H
