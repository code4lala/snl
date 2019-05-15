#include <iostream>
#include <fstream>
#include "SnlGrammarAnalyser.h"

using namespace std;

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

int main() {
    SnlGrammarAnalyser::init();
    SnlGrammarAnalyser analyser;

    for (int i = 1; i <= 8; i++) {
        ifstream in;
        in.open("../examples/C" + to_string(i) + ".TXT", ios::in);
        string x = convertStreamToString(in);
        in.close();

        if (analyser.parse(x.c_str()) == 0) {
            cout << i << " 词法检查通过! 语法检查通过!" << endl << endl;
            cout << "语法树的DOT Language描述如下:" << endl << endl;
            cout << analyser.getTree() << endl << endl;
        } else {
            cerr << i << " " << analyser.getError() << endl << endl;
        }
    }

    return 0;
}