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

    // �ռ��� �մ�
    Symbol& Epsilon();

    // �ռ��� ���ַ��ֽ��
    Symbol& Plus();//�Ӻ� +
    Symbol& Sub();//���� -
    Symbol& Mul();//�˺� *
    Symbol& Div();//���� /
    Symbol& LessThan();//С�ں� <
    Symbol& Equal();// ���� =
    Symbol& LeftParenthesis(); // ��С���� (
    Symbol& RightParenthesis(); // ��С���� )
    Symbol& LeftBracket();//�������� [
    Symbol& RightBracket();//�������� ]
    Symbol& Dot();//���ַ��ֽ�� . һ���� ���ڽṹ��
    Symbol& Semicolon();// �ֺ� ;
    Symbol& Comma();// ���� ,

    // �ռ��� ˫�ַ��ֽ��
    Symbol& TwoDots();//�����±�ָ��� ������ ..
    Symbol& ColonEqual();// ��ֵ���� :=

    // �ռ��� #
    Symbol& Sharp();//��������First���У�������Follow����Predict����

    // �ռ��� �ؼ���
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

    // ���ռ���
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

    // parse֮ǰ��initһ�¾Ϳ����ˣ���������������getError()�鿴����ԭ��
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
            // ���֮ǰ��
            words.clear();
            tokenSymbols.clear();
            vTree.clear();
            errMsg = "";
            // �Ƚ��дʷ�����
            global_line_number = 1;
            while (*code != '\0') {
                int r = analysisOneWord(code);
                if (r <= 0) {
                    errMsg = "[Lexical error] line " + to_string(global_line_number) + " Current parsing \'" + *code +
                             "\'";
                    // ����
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

            // Ȼ������﷨����
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

            ////DEBUG ���������֪˳����Ƶ�ʽ
            //cout << "DEBUG �����Ƶ�ʽ" << endl;
            //for (const Derivation& d : vTree) {
            //	cout << d << endl;
            //}
            //cout << "DEBUG �����Ƶ�ʽ" << endl;

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

            stringstream ss;//��DOT Language��ʾ���﷨�����ַ����ŵ������

            ss.clear();
            ss << "digraph GrammarTree {" << endl;
            queue<Node *> q;
            q.push(root);
            while (!q.empty()) {
                Node *c = q.front();
                q.pop();

                // �ýڵ����ʽ������
                if (c->curr.terminate) {
                    if (c->curr == Epsilon()) {
                        ss << "\"" << c->id << "\" [shape=square; style=filled; fillcolor=cornsilk; label=\""
                           << "��" << "\"];" << endl;
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
                    // Ҷ�ӽ��
                    continue;
                }

                // �������ڵ�Ĺ�ϵ
                string children = "";
                for (unsigned int i = 0; i < c->children.size(); i++) {
                    children += "\"" + to_string(c->children[i]->id) + "\"; "; // "id; "
                }
                ss << "\"" << c->id << "\" -> {" << children << "}" << endl;
                ss << "{rank=same; " << children << "}" << endl;

                // ��ջ
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
        vector<Derivation> vTree;// ��Vector��ʾ���﷨��
        unsigned int treeIndex;// ָʾvTree���±�
        Node *root;// ����������ʾ���﷨��
        int global_id;//DOT Language��ÿ���ڵ��id
        unsigned int global_token_index;//���ڽ�IDת��Ϊ���������ߺ�������
        void dfsBuildTree(Node *&parent) {
            // �������Ĳ���parentһ���Ƿ��ռ���
            if (parent->curr.terminate) {
                cerr << "[error] dfsBuildTree invalid param" << endl;
                exit(-1);
            }
            if (treeIndex >= vTree.size()) {
                cerr << "[error] dfsBuildTree treeIndex" << endl;
                exit(-1);
            }
            // �����ӽڵ�
            for (unsigned int i = 0; i < vTree[treeIndex].symbolVector.size(); i++) {
                parent->children.emplace_back(new Node(vTree[treeIndex].symbolVector[i], parent, global_id++));
            }
            // �������Ƶ�ʽ�±�ͼӼ�
            treeIndex++;
            for (unsigned int i = 0; i < parent->children.size(); i++) {
                if (parent->children[i]->curr.terminate) {
                    // ��IDת��Ϊʵ��ֵ
                    if (parent->children[i]->curr == Epsilon()) {}
                    else {
                        //cout << tokenSymbols[global_token_index] <<"\t\ttokenlist"<< endl;
                        //cout << parent->children[i]->curr << endl;
                        //cout << endl;
                        parent->children[i]->curr.v = tokenSymbols[global_token_index].s.v;
                        global_token_index++;
                    }
                    continue;// �ռ�����Ҷ�ڵ�
                }
                dfsBuildTree(parent->children[i]);
            }
        }

        /**
         * �﷨����
         */
        int grammarAnalysis() {
            // �� ����ջ #E   ������ i+i*i#
            list<Symbol> s;//��Ϊջ���ܱ������������list�������
            s.emplace_back(Sharp()); // #
            s.emplace_back(allDerivations.front().head); // �ķ���ʼ��
            tokenSymbols.emplace_back(TokenSymbol(Sharp(), numeric_limits<int>::max()));
            int i = 0;//����������
            while (!s.empty()) {
                Symbol t = s.back();//ջ��Ԫ��
                Symbol r = tokenSymbols[static_cast<unsigned int>(i)].s;//��������ǰɨ�����
                if (!t.terminate) {
                    //���ռ��� Ѱ�� t->ɶ ��Predict�� ������������ǰ��������
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
                        // �ҵ��� t->a.second��Predict������ r

                        // �����﷨������
                        vTree.emplace_back(a.first);

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
                        errMsg = "[Grammatical error] line " +
                                 to_string(tokenSymbols[static_cast<unsigned int>(i)].line) + " Current parsing \'" +
                                 r.v + "\'";
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
                            errMsg = "[Grammatical error] line " +
                                     to_string(tokenSymbols[static_cast<unsigned int>(i)].line) +
                                     " Current parsing \'" + r.v + "\'";
                            return E_GRAMMAR;
                            // ����
                            //cerr << "[error] " << __LINE__ << " Top of stack is " << t << " but head of queue is " << r
                            //	<< endl;
                            //cerr << "at line " << tokenSymbols[i].line << endl;
                            //exit(-1);
                        }
                    } else if (t == Epsilon()) {
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
                            errMsg = "[Grammatical error] line " +
                                     to_string(tokenSymbols[static_cast<unsigned int>(i)].line) +
                                     " Current parsing \'" + r.v + "\'";
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
                            if (t == Sharp()) {
                                // �����!!!
                                break;
                            }
                            s.pop_back();
                            i++;
                            continue;
                        } else {
                            // ����
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

using snl::SnlGrammarAnalyser;

#endif //SNL_SNLGRAMMARANALYSER_H
