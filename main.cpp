#include <iostream>
#include <fstream>
#include "SnlGrammarAnalyser.h"

using namespace std;

/**
 * ������������Ϊһ��string
 * @param in ������
 * @return �������������������
 */
string convertStreamToString(ifstream &in) {
    string r, x;
    while (getline(in, x)) {
        r += x;
        r += '\n';
    }
    return r;
}

int main() {
    SnlGrammarAnalyser::init();
    SnlGrammarAnalyser analyser;

    for (int i = 1; i <= 8; i++) {
        ifstream in;
        in.open("../examples/C" + to_string(i) + ".TXT", ios::in);
        string x = convertStreamToString(in);
        in.close();

        if (analyser.parse(x.c_str()) == 0) {
            cout << i << " �ʷ����ͨ��! �﷨���ͨ��!" << endl << endl;
            cout << "�﷨����DOT Language��������:" << endl << endl;
            cout << analyser.getTree() << endl << endl;
        } else {
            cerr << i << " " << analyser.getError() << endl << endl;
        }
    }

    return 0;
}