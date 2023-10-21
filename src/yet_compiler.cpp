#include<iostream>
#include<fstream>
#include<sstream>
#include<string.h>
#include<unordered_map>
#include<cstdlib>
#include<vector>
#include<list>
#include<algorithm>
#include<iterator>
#include<iomanip>
#include<math.h>
#include<unistd.h>
#include"opecodes.h"
#define nan NAN
using namespace std;

template <typename T>
void remove(std::vector<T>& v, size_t index) {
    v.erase(v.begin() + index);
}
string formatnum(int len, int value){
	stringstream str;
	if(value >= 10000) str << "more";
	else if(value >= 1000) str << "" << to_string(value);
	else if(value >= 100) str << " " << to_string(value);
	else if(value >= 10) str << "  " << to_string(value);
	else str << "   " << to_string(value);
	return str.str();
}
template <typename K, typename V>
void print_map(unordered_map<K, V> const &m){
	cout << "map ";
	for(auto const &pair: m){
		cout << "{" << pair.first << ":" << pair.second << "}";
	}
	cout << endl;
}

class Token{
protected:
    int linenum;
    double value = 0;
    string text = "";
    int type;
	bool isop = false;
public:
    Token(int line){
        linenum = line;
        type = -1;
    }
    int getlineno(){return linenum;}
    int gettype(){return type;}
    double getvalue(){return value;}
    string gettext(){return text;}
	bool is_op(){return isop;}
};
class IdToken : public Token{
private:
public:
    IdToken(int line, string id, bool isOp = false) : Token(line){
        text = id;
        type = 0;
		isop = isOp;
    }
};
class NumToken : public Token{
private:
public:
    NumToken(int line, double val) : Token(line){
        value = val;
        type = 1;
    }
};
void yetexception(string message, string filename = ""){
    cerr << message << "\tfile:" << filename << endl;
    exit(1);
}
void yetexception(string message, Token t, string filename = ""){
    cerr << message << " in line " << t.getlineno() << "\tfile:" << filename << endl;
    exit(1);
}
class Lexer{
private:
    ifstream file;
    bool hasMore;
	int lineno = 0;
    vector<Token> queue;
	list<string> operators;
    bool fillqueue(int i);
    void readline();
public:
    Lexer(){
        hasMore = true;
    }
	void setoperators(vector<string> o){
		operators = list<string>(o.begin(), o.end());
	}
	void addoperators(string o){
		operators.push_back(o);
	}
	void openfile(string filename){
		file = ifstream(filename);
		if(!file){
			yetexception("file \"" + filename + "\" does not exists");
		}
	}
	void closefile(){
		file.close();
	}
    Token read();
    Token peek(int i);
	Token new_operator();
};
bool Lexer::fillqueue(int i){
    while(i >= queue.size())
        if(hasMore){
			readline();
		}
        else
            return false;
    return true;
}
void Lexer::readline(){
    string line;
    if(!getline(file, line)){
        hasMore = false;
        return;
    }
    int endpos = line.length();
    Token token(lineno);
    int i = 0;
    string str = "";
    while(i < endpos){
        token = Token(lineno);
        str = "";
        if('0'<=line[i] && line[i]<='9' || ('-'==line[i] && ('0'<=line[i + 1] && line[i + 1]<='9'))){
            str += line[i++];
            while('0'<=line[i] && line[i]<='9'){
                str += line[i++];
            }
            if(line[i]=='.'){
                str += line[i++];
            }
            while('0'<=line[i] && line[i]<='9'){
                str += line[i++];
            }
            token = NumToken(lineno, atof(str.c_str()));
        }else if('a'<=line[i] && line[i]<='z' || 'A'<=line[i] && line[i]<='Z' || line[i] == '_'){
            str += line[i++];
            while('a'<=line[i] && line[i]<='z' || 'A'<=line[i] && line[i]<='Z' || line[i] == '_'){
                str += line[i++];
            }
            while('0'<=line[i] && line[i]<='9'){
                str += line[i++];
            }
			if(str == "NaN") token = NumToken(lineno, NAN);
			else if(str == "Infinity") token = NumToken(lineno, INFINITY);
            else token = IdToken(lineno, str);
        }else if(' '==line[i] || '\t'==line[i]){
            i++;
            continue;
        }else if('\n' == line[i]){
			i++;
			break;
		}else if('\'' == line[i]){
			i++;
			while('\'' != line[i]){
				i++;
			}
			i++;
		}else{
			int a = i;
			string s = str;
			for(int n = 0; n < 10; n++){
				if(*find(operators.begin(), operators.end(), s) == s) a = i + n;
				s += line[i + n];
				//cout << "s: " << s << ", *find(operators.begin(), operators.end(), s) == s : " << (*find(operators.begin(), operators.end(), s) == s) << endl;
			}
			//cout << "a:" << a << endl;
			while(!('0'<=line[i] && line[i]<='9') && !('a'<=line[i] && line[i]<='z' || 'A'<=line[i] && line[i]<='Z' || line[i] == '_') && !(' '==line[i] || '\n'==line[i] || '\t'==line[i]) && i < a/*(*find(operators.begin(), operators.end(), str) != str || *find(operators.begin(), operators.end(), str + line[i]) == str + line[i] || *find(operators.begin(), operators.end(), str + line[i] + line[i + 1]) == str + line[i] + line[i + 1])*/){
                str += line[i++];
            }
            token = IdToken(lineno, str, true);
		}
        queue.push_back(token);
    }
    i--;
    lineno++;
}
Token Lexer::read(){
    if(fillqueue(0)){
		if(queue.size() > 0){
        	Token r = queue.at(0);
        	remove(queue, 0);
        	return r;
		}else
			return Token(lineno);
    }else
        return Token(lineno);
}
Token Lexer::peek(int i){
    if(fillqueue(i)){
		if(queue.size() > 0)
        	return queue.at(i);
		else
			return Token(lineno);
    }else
        return Token(lineno);
}

class Astleaf{
protected:
    int type = -1;
    double value = nan;
    string op = "";
    string name = "";
public:
    Astleaf *right;
    Astleaf *left;
    vector<Astleaf> block;
    Astleaf(Astleaf *l, Astleaf *r){
        right = r;
        left = l;
        type = -1;
    }
    //~Astleaf(){delete right; delete left;}
    int gettype(){return type;}
    double getvalue(){return value;}
    string getop(){return op;}
    string getname(){return name;}
    Astleaf getright(){return *right;}
	bool isright(){return right != nullptr;}
    Astleaf getleft(){return *left;}
	bool isleft(){return left != nullptr;}
    Astleaf getblock(int i){return block.at(i);}
    int getblocksize(){return block.size();}
	void printleaf(){
		switch(type){
			case -1:
				break;
			case 0:
				cout << value;
				break;
			case 1:
				cout << name;
				if(isleft())
					left->printleaf();
				break;
			case 2:
				cout << op << "(";
				left->printleaf();
				cout << " ";
				right->printleaf();
				cout << ")";
				break;
			case 3:
				if(block.size() != 0){
					cout << "(";
					for(int i = 0; i < block.size() - 1; i++){
						block[i].printleaf();
						cout << ",";
					} block[block.size() - 1].printleaf();
					cout << ")";
				}
				break;
			case 4:
				if(block.size() != 0){
					cout << "[";
					for(int i = 0; i < block.size() - 1; i++){
						block[i].printleaf();
						cout << ",";
					} block[block.size() - 1].printleaf();
					cout << "]";
				}
				break;
		}
	}
};
class Number : public Astleaf{
public:
    Number(Astleaf *l, Astleaf *r, Token t) : Astleaf(l, r){
        value = t.getvalue();
        type = 0;
    }
    //~Number(){delete right; delete left;}
};
class Name : public Astleaf{
public:
    Name(Astleaf *l, Astleaf *r, Token t) : Astleaf(l, r){
        name = t.gettext();
        type = 1;
    }
    //~Name(){delete right; delete left;}
};
class Expr : public Astleaf{
public:
    Expr(Astleaf *l, Astleaf *r, Token t) : Astleaf(l, r){
        op = t.gettext();
        type = 2;
    }
    //~Expr(){delete right; delete left;}
};
class Block : public Astleaf{
public:
    Block(vector<Astleaf> b, bool iscode) : Astleaf(nullptr, nullptr){
        copy(b.begin(), b.end(), back_inserter(block));
		if(!iscode) type = 3;
		else type = 4;
    }
    //~Block(){delete right; delete left;}
};
class Array : public Astleaf{
public:
	Array(vector<Astleaf> e) : Astleaf(nullptr, nullptr){
		copy(e.begin(), e.end(), back_inserter(block));
		type = 5;
	}
	Array(Astleaf *exp) : Astleaf(exp, nullptr){
		type = 5;
	}
};

class Interpreter{
private:
	unordered_map<string, double> variables;
	enum class Type
	{
		number,
		name,
		expr,
		block,
		code,
		array
	};
public:
	double expression(Astleaf exp);
	Interpreter(){}
	void addvariable(string name, double value){
		variables[name] = value;
	}
	double getvariable(string name){
		return variables[name];
	}
	void erasevariable(string name){
		variables.erase(name);
	}
};
double Interpreter::expression(Astleaf exp){
	double result;
	if(exp.gettype() == (int)Type::number){
		result = exp.getvalue();
	}else if(exp.gettype() == (int)Type::name){
		result = variables.find(exp.getname()) != variables.end()? variables[exp.getname()] : nan;
	}else if(exp.gettype() == (int)Type::expr){
		if(exp.getop() == "="){
			variables[exp.getleft().getname()] = expression(exp.getright());
			result = expression(exp.getright());
			if(isnan(expression(exp.getleft()))) result = nan;
		}else if(exp.getop() == "?"){
			result = nan;
		}else if(exp.getop() == ":"){
			result = nan;
		}else if(exp.getop() == "=="){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) == expression(exp.getright()) : nan;
		}else if(exp.getop() == "!="){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) != expression(exp.getright()) : nan;
		}else if(exp.getop() == "<"){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) < expression(exp.getright()) : nan;
		}else if(exp.getop() == ">"){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) > expression(exp.getright()) : nan;
		}else if(exp.getop() == "<="){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) <= expression(exp.getright()) : nan;
		}else if(exp.getop() == ">="){
			result = !(isnan(expression(exp.getleft())) || isnan(expression(exp.getright())))? expression(exp.getleft()) >= expression(exp.getright()) : nan;
		}else if(exp.getop() == "+"){
			result = expression(exp.getleft()) + expression(exp.getright());
		}else if(exp.getop() == "-"){
			result = expression(exp.getleft()) - expression(exp.getright());
		}else if(exp.getop() == "*"){
			result = expression(exp.getleft()) * expression(exp.getright());
		}else if(exp.getop() == "/"){
			result = expression(exp.getleft()) / expression(exp.getright());
		}else if(exp.getop() == "%"){
			result = fmod(expression(exp.getleft()), expression(exp.getright()));
		}else if(exp.getop() == "^"){
			result = pow(expression(exp.getleft()), expression(exp.getright()));
		}else if(exp.getop() == "@"){
			result = nan;
		}else if(exp.getop() == "$"){
			result = nan;
		}else if(exp.getop() == "|"){
			result = nan;
		}
		if(result == nan) return nan;
	}else if(exp.gettype() == (int)Type::block){
		result = nan;
	}else if(exp.gettype() == (int)Type::array){
		result = nan;
	}
	return result;
}

typedef struct{
    int value;
    bool leftassoc;
}precedence;
class Parser{
private:
	string filename;
    precedence nullprec = {-1, false};
    unordered_map<string, precedence> operators;
	unordered_map<string, string> userop;
    Lexer lex = Lexer();
	Interpreter interpreter = Interpreter();
	enum class Type{
        number,
        name,
        expr,
        block,
		code,
		array
    };
    void token(string op){
        Token t = lex.read();
        if(t.gettype() != 0 && t.gettext() != op)
            yetexception("bad token \"" + t.gettext() + "\", correctly \"" + op + "\"", t, filename);
    }
    bool istoken(string op){
        Token t = lex.peek(0);
        return t.gettype() == 0 && t.gettext() == op;
    }
	bool isend(){
		return lex.peek(0).gettype() == -1;
	}
    bool rightisexpr(int prec, precedence nextprec){
        if(nextprec.leftassoc)
            return prec < nextprec.value;
        else
            return prec <= nextprec.value;
    }
    bool equprec(precedence prec1, precedence prec2){
        return prec1.value == prec2.value && prec1.leftassoc == prec2.leftassoc;
    }
	void addfunction(Astleaf left, Astleaf right){
		vector<string> parameter;
		for(int i = 0; i < left.getleft().getblocksize(); i++){
			parameter.push_back(left.getleft().getblock(i).getname());
		}
		functions[left.getname()] = {right, parameter};
	}
    Astleaf factor();
    precedence nextop();
    void doshift(Astleaf left, int prec, Astleaf *l, Astleaf *r, Token *o);
public:
	unordered_map<string, double> macro;
	struct function{
		Astleaf expression = Astleaf(nullptr, nullptr);
		vector<string> parameter;
	};
	unordered_map<string, function> functions;
    Parser(string filename){
		this->filename = filename;
        operators["="] = {1, false};
		operators["?"] = {2, true};
		operators[":"] = {3, true};
        operators["=="] = {3, true};
        operators["!="] = {3, true};
        operators["<"] = {3, true};
        operators[">"] = {3, true};
        operators["<="] = {3, true};
        operators[">="] = {3, true};
        operators["+"] = {4, true};
        operators["-"] = {4, true};
        operators["*"] = {5, true};
        operators["/"] = {5, true};
        operators["%"] = {5, true};
		operators["^"] = {6, false};
		operators["@"] = {7, true};
		operators["$"] = {7, true};
		operators["|"] = {7, true};
        operators[")"] = nullprec;
		operators["]"] = nullprec;
		operators["}"] = nullprec;
        operators[";"] = nullprec;
		operators[","] = nullprec;
		vector<string> o;
		for(auto op = operators.begin(); op != operators.end(); op++)
			o.push_back(op->first);
		o.push_back("(");
		o.push_back("[");
		o.push_back("{");
		o.push_back("#");
		o.push_back("'");
        lex.openfile(filename);
		lex.setoperators(o);
    }
	void closefile(){
		lex.closefile();
	}
    Astleaf expression();
    Astleaf block();
	Astleaf code();
};
Astleaf Parser::factor(){
    if(istoken("(")){
        token("(");
        Astleaf e = expression();
        token(")");
        return e;
    }else if(istoken("{")){
		token("{");
		Astleaf b = block();
		token("}");
		return b;
	}else if(istoken("[")){
		vector<Astleaf> elements;
		token("[");
		if(!istoken("]")){
			while(1){
				elements.push_back(expression());
				if(istoken(",")) token(",");
				else break;
			}
		}
		token("]");
		Astleaf *e = new Array(elements);
		return *e;
	}else{
        Token t = lex.read();
		if(t.gettype() == 0){
			if(lex.peek(0).gettext() == "("){
				token("(");
				vector<Astleaf> argments;
				if(!istoken(")")){
					while(1){
						argments.push_back(expression());
						if(istoken(",")) token(",");
						else break;
					}
				}
				token(")");
				Astleaf *a = new Block(argments, false);
				Astleaf f = Name(a, nullptr, t);
				if(functions.find(t.gettext()) != functions.end()){
					for(int i = 0; i < functions[t.gettext()].parameter.size(); i++){
						if(!isnan(argments[i].getvalue())){
							interpreter.addvariable(functions[t.gettext()].parameter[i], argments[i].getvalue());
						}else
							return f;
					}
					if(!isnan(interpreter.expression(functions[t.gettext()].expression)))
						f = Number(nullptr, nullptr, NumToken(0, interpreter.expression(functions[t.gettext()].expression)));
					for(int i = 0; i < functions[t.gettext()].parameter.size(); i++)
						interpreter.erasevariable(functions[t.gettext()].parameter[i]);
				}
				return f;
			}else{
				Astleaf v = Astleaf(nullptr, nullptr);
				if(macro.find(t.gettext()) != macro.end()) v = Number(nullptr, nullptr, NumToken(t.getlineno(), macro[t.gettext()]));
				else v = Name(nullptr, nullptr, t);
				v = isnan(interpreter.expression(v))? v : Number(nullptr, nullptr, NumToken(t.getlineno(), interpreter.expression(v)));
				return v;
			}
        }else if(t.gettype() == 1){
            Astleaf n = Number(nullptr, nullptr, t);
            return n;	
		}else{
			cout << t.gettype() << endl;
            yetexception("bad token type", t, filename);
        }
    }
    return Astleaf(nullptr, nullptr);
}
precedence Parser::nextop(){
    Token t = lex.peek(0);
    if(t.gettype() == 0)
        return operators[t.gettext()];
    return nullprec;
}
void Parser::doshift(Astleaf left, int prec, Astleaf *l, Astleaf *r, Token *o){
    Token op = lex.read();
    *o = op;
    Astleaf right = factor();
    precedence p;
    while(!equprec((p = nextop()), nullprec) && rightisexpr(prec, p)){
        Astleaf *rightleft = new Astleaf(nullptr, nullptr);
        Astleaf *rightright = new Astleaf(nullptr, nullptr);
        Token *op = new Token(-1);
        doshift(right, p.value, rightleft, rightright, op);
        right = Expr(rightleft, rightright, *op);
		if(userop.find(op->gettext()) != userop.end()){
			Astleaf *argments = new Block(vector<Astleaf>{*rightleft, *rightright}, false);
			right = Name(argments, nullptr, IdToken(0, userop[op->gettext()]));
		}
    }
    *r = right;
    *l = left;
}
Astleaf Parser::expression(){
    Astleaf right = factor();
    precedence next;
    while(!equprec((next = nextop()), nullprec)){
        Astleaf *rightleft = new Astleaf(nullptr, nullptr);
        Astleaf *rightright = new Astleaf(nullptr, nullptr);
        Token *op = new Token(-1);
        doshift(right, next.value, rightleft, rightright, op);
        right = Expr(rightleft, rightright, *op);
		if(userop.find(op->gettext()) != userop.end()){
			Astleaf *argments = new Block(vector<Astleaf>{*rightleft, *rightright}, false);
			right = Name(argments, nullptr, IdToken(0, userop[op->gettext()]));
		}
		
		if(!isnan(interpreter.expression(right))){
			right = Number(nullptr, nullptr, NumToken(0, interpreter.expression(right)));
			if(op->gettext() == "=") right = Astleaf(nullptr, nullptr);
		}else{
			if(op->gettext() == "=" && rightleft->isleft() && rightright->gettype() == 2){
				addfunction(*rightleft, *rightright);
			}
		}
		
    }
    return right;
}
Astleaf Parser::block(){
    vector<Astleaf> exps;
    while(!istoken("}")){
        Astleaf exp = expression();
        exps.push_back(exp);
        token(";");
    }
    Astleaf code = Block(exps, false);
    return code;
}
Astleaf Parser::code(){
    vector<Astleaf> exps;
    while(!isend()){
		if(istoken("#")){
			token("#");
			if(istoken("include")){
				token("include");
				string filename = lex.read().gettext() + ".yet";
				Parser incparser = Parser(filename);
				Astleaf inccode = incparser.code();
				for(int i = 0; i < inccode.getblocksize(); i++)
					exps.push_back(inccode.getblock(i));
				//macro.merge(incparser.macro);
				for(auto itr = incparser.macro.begin(); itr != incparser.macro.end(); itr++)
					macro[itr->first] = itr->second;
				for(auto itr = incparser.userop.begin(); itr != incparser.userop.end(); itr++)
					userop[itr->first] = itr->second;
				incparser.closefile();
			}else if(istoken("define")){
				token("define");
				string macroname = lex.read().gettext();
				double macronum = 0;
				try{
					macronum = lex.read().getvalue();
				}catch(const std::invalid_argument& e){
					yetexception("not a number", lex.peek(0), filename);
				}
				macro[macroname] = macronum;
			}else if(istoken("defoperator")){
				token("defoperator");
				string opname;
				Token t(0);
				while((t = lex.read()).is_op()) opname += t.gettext();
				string funcname = t.gettext();
				int order = (int)lex.read().getvalue();
				bool leftassoc = (bool)lex.read().getvalue();
				lex.addoperators(opname);
				operators[opname] = {order, leftassoc};
				userop[opname] = funcname;
				//cout << opname << ":" << funcname << "," << order << "," << leftassoc << endl;
			}
			continue;
		}
        Astleaf exp = expression();
    	exps.push_back(exp);
        token(";");
		//exp.printleaf(); cout << endl;
    }
    Astleaf code = Block(exps, true);
    return code;
}
class Compiler{
protected:
	string filename;
    enum class Type{
        number,
        name,
        expr,
        block,
		code,
		array
    };
	bool isnative(string funcname){
		return find(nativefuncs.begin(), nativefuncs.end(), funcname) != nativefuncs.end();
	}
	vector<string> nativefuncs;
	unordered_map<string, int> var;
	void pushglobal(string name){
		var[name] = gp;
		gp++;
	}
private:
	typedef struct{
		unordered_map<string, int> var;
		int value;
	}env;
    uint8_t *code = new uint8_t[655365];
	vector<string> asm_code;
    int pc = 0;
	int sp = 327682;
	int bp = 0;
	int bp_e = 0;
	int hp = 0;
	int gp = 0;
    uint8_t reg_num = 0;
	static const uint8_t RSP = 31, RBP = 30, RHP = 29, RET = 28;
	vector<string> argments;
	bool ispipe = false;
	Astleaf pipevalue = Astleaf(nullptr, nullptr);
	vector<env> envs;
	void push(unordered_map<string, int> env){
		envs.push_back({env, sp - 8});
		//cout << "env.val=" << sp - 8 << endl;
	}
	void addenv(int n, unordered_map<string, int> varenv){
		envs[n].var.insert(varenv.begin(), varenv.end());
	}
	void pop(){
		envs.pop_back();
	}
	int lookat(string name, bool *isglobal){
		*isglobal = false;
		if(envs.size() > 0){
			int i = envs.size() - 1;
			while(envs.at(i).var.find(name) == envs.at(i).var.end()){
				if(i > 0){
					i--;
				}else{
				 	*isglobal = true;
					break;
				}
			}
			if(!(*isglobal)) return bp - envs.at(i).value + envs.at(i).var[name] * 8;
		}
		*isglobal = true;
		if(var.find(name) != var.end()){
			return var[name];
		}else{
			pushglobal(name);
			return var[name];
		}
		yetexception("\"" + name + "\"" + " undeclared", filename);
		return 0;
	}
public:
    Compiler(string fn){
		filename = fn;
		nativefuncs.push_back("write");
		nativefuncs.push_back("read");
		nativefuncs.push_back("return");
		nativefuncs.push_back("if");
		nativefuncs.push_back("makearray");
		nativefuncs.push_back("isNaN");
    }
    void write(uint8_t b){
        code[pc] = b;
        pc++;
    }
	void writenum(double n){
		memcpy(code + pc, &n, 8);
		pc += 8;
	}
	void writenum(long n){
		memcpy(code + pc, &n, 8);
		pc += 8;
	}
	void writenum(int n){
		long m = (long)n;
		memcpy(code + pc, &m, 8);
		pc += 8;
	}
    void compile(Astleaf e, bool isassgnment);
	uint8_t *getcode(int *size, bool isprint = false);
};
void Compiler::compile(Astleaf e, bool isassignment){
    Astleaf exp = e;
    if(exp.gettype() == (int)Type::number){
        double val = exp.getvalue();
		asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(val));
        write((uint8_t)Opcode::MOVN);
        write(reg_num);
		writenum(val);
        reg_num++;
    }else if(exp.gettype() == (int)Type::name){
		if(isassignment){
			bool isglobal;
			int n = lookat(exp.getname(), &isglobal);
			if(!isglobal){
				asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num) + "," + "r" + to_string(RBP));
				write((uint8_t)Opcode::MOVR);
				write(reg_num);
				write(RBP);
				if(n < 0){
					asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(-n));
					write((uint8_t)Opcode::MOVN);
					write(reg_num + 1);
					writenum(-n);
					asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
					write((uint8_t)Opcode::ADD);
				}else{
					asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(n));
					write((uint8_t)Opcode::MOVN);
					write(reg_num + 1);
					writenum(n);
					asm_code.push_back(formatnum(4, pc) + " SUB " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
					write((uint8_t)Opcode::SUB);
				}
				write(reg_num);
				write(reg_num + 1);
				argments.clear();
				if(exp.isleft()){	
					for(int i = 0; i < exp.getleft().getblocksize(); i++)
						argments.push_back(exp.getleft().getblock(i).getname());
				}
				reg_num++;
			}else{
				asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(n * 8));
				write((uint8_t)Opcode::MOVN);
				write(reg_num);
				writenum(n * 8);
				reg_num++;
				argments.clear();
				if(exp.isleft()){
					for(int i = 0; i < exp.getleft().getblocksize(); i++)
						argments.push_back(exp.getleft().getblock(i).getname());
				}
			}
		}else{
			bool isglobal;
			if(isnative(exp.getname())){
				if(exp.isleft()){
					if(exp.getname() == "write"){
						compile(exp.getleft().getblock(0), exp.getleft().getblock(0).getop() == "=");
						reg_num--;
						asm_code.push_back(formatnum(4, pc) + " OUT " + "r" + to_string(reg_num));
						write((uint8_t)Opcode::OUT);
						write(reg_num);
					}else if(exp.getname() == "read"){
						asm_code.push_back(formatnum(4, pc) + " IN " + "r" + to_string(reg_num));
						write((uint8_t)Opcode::IN);
						write(reg_num);
						reg_num++;
					}else if(exp.getname() == "return"){
						compile(exp.getleft().getblock(0), exp.getleft().getblock(0).getop() == "=");
						reg_num--;
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string((int)RET) + "," + "r" + to_string(reg_num));
						write((uint8_t)Opcode::MOVR);
						write(RET);
						write(reg_num);
					}else if(exp.getname() == "if"){
						compile(exp.getleft().getblock(0), exp.getleft().getblock(0).getop() == "=");
						reg_num--;
						asm_code.push_back(formatnum(4, pc) + " IFZERO " + "r" + to_string(reg_num) + ",");
						write((uint8_t)Opcode::IFZERO);
						int code_num = asm_code.size() - 1;
						write(reg_num);
						int ifzero_position = pc;
						writenum(0);
						compile(exp.getleft().getblock(1), false);
						long where_to_jump = pc;
						memcpy(code + ifzero_position, &where_to_jump, 8);
						asm_code[code_num] += to_string(pc);
						if(exp.getleft().getblocksize() >= 3){
							where_to_jump = pc + 9;
							memcpy(code + ifzero_position, &where_to_jump, 8);
							asm_code[code_num] = formatnum(4, ifzero_position - 2) + " IFZERO " + "r" + to_string(reg_num - 1) + "," + to_string(pc + 9);
							asm_code.push_back(formatnum(4, pc) + " JUMPN ");
							write((uint8_t)Opcode::JUMPN);
							int code_num = asm_code.size() - 1;
							int jumpn_position = pc;
							writenum(0);
							compile(exp.getleft().getblock(2), false);
							where_to_jump = pc;
							memcpy(code + jumpn_position, &where_to_jump, 8);
							asm_code[code_num] += to_string(pc);
						}
					}else if(exp.getname() == "makearray"){
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(hp));
						write((uint8_t)Opcode::MOVN);
						write(reg_num);
						writenum(hp);
						reg_num++;
						hp += (int)exp.getleft().getblock(0).getvalue() * 8;
					}else if(exp.getname() == "isNaN"){

					}
				}
			}else{
				int n = lookat(exp.getname(), &isglobal);
				if(!isglobal){
					asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num) + "," + "r" + to_string(RBP));
					write((uint8_t)Opcode::MOVR);
					write(reg_num);
					write(RBP);
					if(n < 0){
						long m = -n;
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
						write((uint8_t)Opcode::MOVN);
						write(reg_num + 1);
						writenum(m);
						asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
						write((uint8_t)Opcode::ADD);
					}else{
						long m = n;
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
						write((uint8_t)Opcode::MOVN);
						write(reg_num + 1);
						writenum(m);
						asm_code.push_back(formatnum(4, pc) + " SUB " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
						write((uint8_t)Opcode::SUB);
					}
					write(reg_num);
					write(reg_num + 1);
					if(!exp.isleft()){
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
						write((uint8_t)Opcode::MOVR);
						write(reg_num + 1);
						write(reg_num);
						asm_code.push_back(formatnum(4, pc) + " MOVS " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::MOVS);
						write(reg_num);
						write(reg_num + 1);
						reg_num++;
					}else{
						asm_code.push_back(formatnum(4, pc) + " MOVS " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
						write((uint8_t)Opcode::MOVS);
						write(reg_num + 1);
						write(reg_num);
						reg_num += 2;
						for(int i = 0; i < reg_num; i++){
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(i)) ;
							write((uint8_t)Opcode::PUSHR);
							write((uint8_t)i);
							sp -= 8;
						}
						int r = reg_num;
						for(int i = exp.getleft().getblocksize() - 1; i >= 0; i--){
							compile(exp.getleft().getblock(i), exp.getleft().getblock(i).getop() == "=");
							reg_num--;
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(reg_num));
							write((uint8_t)Opcode::PUSHR);
							write(reg_num);
							sp -= 8;
							reg_num = r;
						}
						if(ispipe){
							cout << "Helllllllllllllllllllo" << endl;
							compile(pipevalue, false);
							/*	
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(reg_num - 3));
							write((uint8_t)Opcode::PUSHR);
							write(reg_num - 3);
							sp -= 8;
							*/
						}
						long l = pc + 33;
						asm_code.push_back(formatnum(4, pc) + " PUSHN " + to_string(l));
						write((uint8_t)Opcode::PUSHN);
						writenum(l);
						sp -= 8;
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num) + "," + "r" + to_string(RBP));
						write((uint8_t)Opcode::MOVR);
						write(reg_num);
						write(RBP);
						if(n < 0){
							long m = -n;
							asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
							write((uint8_t)Opcode::MOVN);
							write(reg_num + 1);
							writenum(m);
							asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
							write((uint8_t)Opcode::ADD);
						}else{
							long m = n;
							asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
							write((uint8_t)Opcode::MOVN);
							write(reg_num + 1);
							writenum(m);
							asm_code.push_back(formatnum(4, pc) + " SUB " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));	
							write((uint8_t)Opcode::SUB);
						}
						write(reg_num);
						write(reg_num + 1);
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
						write((uint8_t)Opcode::MOVR);
						write(reg_num + 1);
						write(reg_num);
						asm_code.push_back(formatnum(4, pc) + " MOVS " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::MOVS);
						write(reg_num);
						write(reg_num + 1);
						reg_num--;

						asm_code.push_back(formatnum(4, pc) + " JUMPR " + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::JUMPR);
						write(reg_num + 1);
						long argmentsize = exp.getleft().getblocksize() * 8;
						argmentsize += ispipe? 8 : 0;
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(argmentsize));
						write((uint8_t)Opcode::MOVN);
						write(reg_num + 1);
						writenum(argmentsize);
						asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string((int)RSP) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::ADD);
						write(RSP);
						write(reg_num + 1);
						sp += argmentsize;
						for(int i = r - 1; i >= 0; i--){
							asm_code.push_back(formatnum(4, pc) + " POP " + "r" + to_string(i));
							write((uint8_t)Opcode::POP);
							write((uint8_t)i);
							sp += 8;
						}
						reg_num--;
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num) + "," + "r" + to_string((int)RET));
						write((uint8_t)Opcode::MOVR);
						write(reg_num);
						write(RET);
						reg_num++;
					}	
				}else{
					long m = n * 8;
					if(!exp.isleft()){
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(m));
						write((uint8_t)Opcode::MOVN);
						write(reg_num);
						writenum(m);
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
						write((uint8_t)Opcode::MOVR);
						write(reg_num + 1);
						write(reg_num);
						asm_code.push_back(formatnum(4, pc) + " MOVG " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::MOVG);
						write(reg_num);
						write(reg_num + 1);
						reg_num++;
					}else{
						for(int i = 0; i < reg_num; i++){
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(i)) ;
							write((uint8_t)Opcode::PUSHR);
							write((uint8_t)i);
							sp -= 8;
						}
						int r = reg_num;
						for(int i = exp.getleft().getblocksize() - 1; i >= 0; i--){
							compile(exp.getleft().getblock(i), exp.getleft().getblock(i).getop() == "=");
							reg_num--;
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(reg_num));
							write((uint8_t)Opcode::PUSHR);
							write(reg_num);
							sp -= 8;
							reg_num = r;
						}
						if(ispipe){
							//cout << "Hellllllllllo " << endl;
							compile(pipevalue, false);
								
							asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string(reg_num - 3));
							write((uint8_t)Opcode::PUSHR);
							write(reg_num - 3);
							sp -= 8;
							
						}
						long l = pc + 24;
						asm_code.push_back(formatnum(4, pc) + " PUSHN " + to_string(l));
						write((uint8_t)Opcode::PUSHN);
						writenum(l);
						string filename;
						sp -= 8;
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
						write((uint8_t)Opcode::MOVN);
						write(reg_num + 1);
						writenum(m);
						asm_code.push_back(formatnum(4, pc) + " MOVG " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::MOVG);
						write(reg_num);
						write(reg_num + 1);
						asm_code.push_back(formatnum(4, pc) + " JUMPR " + "r" + to_string(reg_num));
						write((uint8_t)Opcode::JUMPR);
						write(reg_num);
						long argmentsize = exp.getleft().getblocksize() * 8;
						argmentsize += ispipe? 8 : 0;
						asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(argmentsize));
						write((uint8_t)Opcode::MOVN);
						write(reg_num + 1);
						writenum(argmentsize);
						asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string((int)RSP) + "," + "r" + to_string(reg_num + 1));
						write((uint8_t)Opcode::ADD);
						write(RSP);
						write(reg_num + 1);
						sp += argmentsize;
						for(int i = r - 1; i >= 0; i--){
							asm_code.push_back(formatnum(4, pc) + " POP " + "r" + to_string(i));
							write((uint8_t)Opcode::POP);
							write((uint8_t)i);
							sp += 8;
						}
						asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num) + "," + "r" + to_string((int)RET));
						write((uint8_t)Opcode::MOVR);
						write(reg_num);
						write(RET);
						reg_num++;
					}
				}
			}
		}
	}else if(exp.gettype() == (int)Type::expr){
		ispipe = false;
        if(!(exp.getop() == ":")) compile(exp.getleft(), exp.getop() == "=");
		if(exp.getop() == "="){
			if(exp.getleft().isleft()/* && exp.getright().gettype() != (int)Type::block*/){
				unordered_map<string, int> env;
				for(int i = 0; i < argments.size(); i++){
					env[argments.at(i)] = -1 - i;
					//cout << "argment" << i << ":" << argments.at(i) << endl;
				}
				push(env);
			}
			if((exp.getleft().isleft() | exp.getright().gettype() == (int)Type::block) && exp.getleft().getop() != "$"){
				sp -= 8;
				bool isglobal;
				lookat(exp.getleft().getname(), &isglobal);
				if(!isglobal){
					long n = pc + 22;
					asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(n));
					write((uint8_t)Opcode::MOVN);
					write(reg_num);
					writenum(n);
					asm_code.push_back(formatnum(4, pc) + " MOVSR " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));
					write((uint8_t)Opcode::MOVSR);
					write(reg_num - 1);
					write(reg_num);
					reg_num++;
				}else{
					long n = pc + 22;
					asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(n));
					write((uint8_t)Opcode::MOVN);
					write(reg_num);
					writenum(n);
					asm_code.push_back(formatnum(4, pc) + " MOVGR " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));
					write((uint8_t)Opcode::MOVGR);
					write(reg_num - 1);
					write(reg_num);
					reg_num++;
				}
				asm_code.push_back(formatnum(4, pc) + " JUMPN ");
				int code_num = asm_code.size() - 1;
				write((uint8_t)Opcode::JUMPN);
				int jump_position = pc;
				writenum(0);
				reg_num = 0;
				asm_code.push_back(formatnum(4, pc) + " PUSHR " + "r" + to_string((int)RBP));
				write((uint8_t)Opcode::PUSHR);
				write(RBP);
				bp_e = bp;
				sp -= 8;
				asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string((int)RBP) + "," + "r" + to_string((int)RSP));
				write((uint8_t)Opcode::MOVR);
				write(RBP);
				write(RSP);
				bp = sp;

				compile(exp.getright(), false);
				
				asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string((int)RSP) + "," + "r" + to_string((int)RBP));
				write((uint8_t)Opcode::MOVR);
				write(RSP);
				write(RBP);
				sp = bp;
				asm_code.push_back(formatnum(4, pc) + " POP " + "r" + to_string((int)RBP));
				write((uint8_t)Opcode::POP);
				write(RBP);
				bp = bp_e;
				sp += 8;
				long n = exp.getright().gettype() != (int)Type::block? pc + 7 : pc + 4;
				memcpy(code + jump_position, &n, 8);
				asm_code[code_num] += to_string(n);

				if(exp.getright().gettype() != (int)Type::block){
					asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string((int)RET) + "," + "r" + to_string(reg_num - 1));
					write((uint8_t)Opcode::MOVR);
					write(RET);
					write(reg_num - 1);
				}
				asm_code.push_back(formatnum(4, pc) + " POP " + "r" + to_string(reg_num));
				write((uint8_t)Opcode::POP);
				write(reg_num);
				sp += 8;
				asm_code.push_back(formatnum(4, pc) + " JUMPR " + "r" + to_string(reg_num));
				write((uint8_t)Opcode::JUMPR);
				write(reg_num);
			}else if(exp.getleft().getop() == "$"){
				compile(exp.getright(), false);
				asm_code.push_back(formatnum(4, pc) + " MOVHR " + "r" + to_string(reg_num - 2) + "," + "r" + to_string(reg_num - 1));
				write((uint8_t)Opcode::MOVHR);
				write(reg_num - 2);
				write(reg_num - 1);
			}else if(exp.getleft().gettype() == (int)Type::name){
				bool isglobal;
				lookat(exp.getleft().getname(), &isglobal);
				compile(exp.getright(), false);
				if(!isglobal){
					asm_code.push_back(formatnum(4, pc) + " MOVSR " + "r" + to_string(reg_num - 2) + "," + "r" + to_string(reg_num - 1));
					write((uint8_t)Opcode::MOVSR);
					write(reg_num - 2);
					write(reg_num - 1);
					reg_num++;
				}else{
					asm_code.push_back(formatnum(4, pc) + " MOVGR " + "r" + to_string(reg_num - 2) + "," + "r" + to_string(reg_num - 1));
					write((uint8_t)Opcode::MOVGR);
					write(reg_num - 2);
					write(reg_num - 1);
					reg_num++;
				}
			}
			if(exp.getleft().isleft() && exp.getright().gettype() != (int)Type::block){
				argments.clear();
				pop();
			}
		}else if(!(exp.getop() == ":")){
			compile(exp.getright(), false);
        }
		reg_num--;
        if(exp.getop() == "?"){
			reg_num += 3;
			int jump_position;
			long where_to_jump;
			int code_num;
			asm_code.push_back(formatnum(4, pc) + " IFZERO " + "r" + to_string(reg_num - 2) + ",");
			code_num = asm_code.size() - 1;
			write((uint8_t)Opcode::IFZERO);
			write(reg_num - 2);
			jump_position = pc;
			writenum(0);
			reg_num--;
			compile(exp.getright().getleft(), false);
			reg_num++;
			asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num - 3) + "," + "r" + to_string(reg_num - 2));
			write((uint8_t)Opcode::MOVR);
			write(reg_num - 3);
			write(reg_num - 2);
			where_to_jump = pc + 9;
			memcpy(code + jump_position, &where_to_jump, 8);
			asm_code[code_num] += to_string(where_to_jump);
			asm_code.push_back(formatnum(4, pc) + " JUMPN ");
			code_num = asm_code.size() - 1;
			write((uint8_t)Opcode::JUMPN);
			jump_position = pc;
			writenum(0);
			reg_num -= 2;
			compile(exp.getright().getright(), false);
			reg_num++;
			asm_code.push_back(formatnum(4, pc) + " MOVR " + "r" + to_string(reg_num - 3) + "," + "r" + to_string(reg_num - 2));
			write((uint8_t)Opcode::MOVR);
			write(reg_num - 3);
			write(reg_num - 2);
			where_to_jump = pc;
			memcpy(code + jump_position, &where_to_jump, 8);
			asm_code[code_num] += to_string(where_to_jump);
			reg_num -= 2;
        }else if(exp.getop() == ":"){
			//reg_num++;
		}else if(exp.getop() == "=="){
			asm_code.push_back(formatnum(4, pc) + " EQU " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::EQU);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "!="){
			asm_code.push_back(formatnum(4, pc) + " NEQU " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::NEQU);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "<"){
			asm_code.push_back(formatnum(4, pc) + " LESS " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::LESS);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == ">"){
			asm_code.push_back(formatnum(4, pc) + " MORE " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::MORE);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "<="){
			asm_code.push_back(formatnum(4, pc) + " LESS " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::LESS);
            write(reg_num);
            write(reg_num - 1);
			asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num - 1) + "," + to_string(0));
            write((uint8_t)Opcode::MOVN);
            write(reg_num - 1);
			writenum(0);
			asm_code.push_back(formatnum(4, pc) + " EQU " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::EQU);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == ">="){
			asm_code.push_back(formatnum(4, pc) + " MORE " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::MORE);
            write(reg_num);
            write(reg_num - 1);
			asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num - 1) + "," + to_string(0));
            write((uint8_t)Opcode::MOVN);
            write(reg_num - 1);
			writenum(0);
			asm_code.push_back(formatnum(4, pc) + " EQU " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::EQU);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "+"){
			asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::ADD);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "-"){
			asm_code.push_back(formatnum(4, pc) + " SUB " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::SUB);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "*"){
			asm_code.push_back(formatnum(4, pc) + " MUL " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::MUL);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "/"){
			asm_code.push_back(formatnum(4, pc) + " DIV " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::DIV);
            write(reg_num - 1);
            write(reg_num);
        }else if(exp.getop() == "%"){
			asm_code.push_back(formatnum(4, pc) + " MOD " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::MOD);
            write(reg_num - 1);
            write(reg_num);
		}else if(exp.getop() == "^"){
			asm_code.push_back(formatnum(4, pc) + " EXP " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));	
            write((uint8_t)Opcode::EXP);
            write(reg_num - 1);
            write(reg_num);
		}else if(exp.getop() == "@"){
			asm_code.push_back(formatnum(4, pc) + " COMV " + "r" + to_string(reg_num));
			write((uint8_t)Opcode::COMV);
			write(reg_num);
			asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num - 1));
			write((uint8_t)Opcode::ADD);
			write(reg_num);
			write(reg_num - 1);
			asm_code.push_back(formatnum(4, pc) + " MOVH " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));
			write((uint8_t)Opcode::MOVH);
			write(reg_num - 1);
			write(reg_num);
		}else if(exp.getop() == "$"){
			asm_code.push_back(formatnum(4, pc) + " COMV " + "r" + to_string(reg_num));
			write((uint8_t)Opcode::COMV);
			write(reg_num);
			asm_code.push_back(formatnum(4, pc) + " ADD " + "r" + to_string(reg_num - 1) + "," + "r" + to_string(reg_num));
			write((uint8_t)Opcode::ADD);
			write(reg_num - 1);
			write(reg_num);
		}else if(exp.getop() == "|"){
			cout << "HELLLLLLLO" << endl;
			ispipe = true;
			pipevalue = exp.getleft();
		}
    }else if(exp.gettype() == (int)Type::block){
		reg_num = 0;
		unordered_map<string, int> argenv;
		for(int i = 0; i < argments.size(); i++){
			argenv[argments.at(i)] = -1 - i;
		}

		unordered_map<string, int> locenv;
		int i = 0;
		if(exp.getblocksize() != 0 && exp.getblock(0).getname() == "declare"){
			while(i < exp.getblock(0).getleft().getblocksize()){
				locenv[exp.getblock(0).getleft().getblock(i).getname()] = i;
				i++;
			}
		}
		push(locenv);
		sp+=16;
		push(argenv);
		sp-=16;
		//sp-=16;
		//sp+=16;
		argments.clear();
		long varnum = i * 8;
		sp -= varnum;
		asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(varnum));
		write((uint8_t)Opcode::MOVN);
		write(reg_num);
		writenum(varnum);
		asm_code.push_back(formatnum(4, pc) + " SUB " + "r" + to_string((int)RSP) + "," + "r" + to_string(reg_num));	
		write((uint8_t)Opcode::SUB);
		write(RSP);
		write(reg_num);
		//sp -= varnum;
        for(int j = (i == 0? 0 : 1); j < exp.getblocksize(); j++){
            reg_num = 0;
            compile(exp.getblock(j), false);
        }
		pop();
		pop();
		reg_num++;
    }else if(exp.gettype() == (int)Type::code){
        for(int i = 0; i < exp.getblocksize(); i++){
            reg_num = 0;
            compile(exp.getblock(i), false);
        }
		//cout << "sp:" << sp << " pc:" << pc << endl;
		
		if(var.find("main") != var.end()){
			reg_num = 0;
			long pc2 = pc + 24;
			asm_code.push_back(formatnum(4, pc) + " PUSHN "  + to_string(pc2));
			write((uint8_t)Opcode::PUSHN);
			writenum(pc2);
			bool isglobal;
			int n = lookat("main", &isglobal);
			long m = n * 8;
			asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(m));
			write((uint8_t)Opcode::MOVN);
			write(reg_num + 1);
			writenum(m);
			asm_code.push_back(formatnum(4, pc) + " MOVG " + "r" + to_string(reg_num) + "," + "r" + to_string(reg_num + 1));
			write((uint8_t)Opcode::MOVG);
			write(reg_num);
			write(reg_num + 1);
			asm_code.push_back(formatnum(4, pc) + " JUMPR " + "r" + to_string(reg_num));
			write((uint8_t)Opcode::JUMPR);
			write(reg_num);
		}
		asm_code.push_back(formatnum(4, pc) + " HLT ");
		write((uint8_t)Opcode::HLT);
	}else if(exp.gettype() == (int)Type::array){
		long start = hp;
		int r = reg_num;
		long n;
		for(int i = 0; i < exp.getblocksize(); i++){
			reg_num = r;
			compile(exp.getblock(i), false);
			reg_num--;
			n = hp;
			asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(n));
			write((uint8_t)Opcode::MOVN);
			write(reg_num + 1);
			writenum(n);
			asm_code.push_back(formatnum(4, pc) + " MOVHR " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
			write((uint8_t)Opcode::MOVHR);
			write(reg_num + 1);
			write(reg_num);
			hp += 8;
		}
		asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num) + "," + to_string(nan));
		write((uint8_t)Opcode::MOVN);
		write(reg_num);
		writenum(nan);
		n = hp;
		asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(n));
		write((uint8_t)Opcode::MOVN);
		write(reg_num + 1);
		writenum(n);
		asm_code.push_back(formatnum(4, pc) + " MOVHR " + "r" + to_string(reg_num + 1) + "," + "r" + to_string(reg_num));
		write((uint8_t)Opcode::MOVHR);
		write(reg_num + 1);
		write(reg_num);
		reg_num--;
		asm_code.push_back(formatnum(4, pc) + " MOVN " + "r" + to_string(reg_num + 1) + "," + to_string(start));
		write((uint8_t)Opcode::MOVN);
		write(reg_num + 1);
		writenum(start);
		reg_num += 2;
	}
}
uint8_t *Compiler::getcode(int *size, bool isprint){
	if(isprint){
		for(string s : asm_code){
			cout << s << endl;
		}
	}
    const int len = pc;
    uint8_t *subcode = new uint8_t[len];
    for(int i = 0; i < len; i++){
        subcode[i] = code[i];
    }
    *size = len;
    return subcode;
}

class TransCompiler : public Compiler{
private:
	string lisp_code;
	vector<string> variables;
	bool isvariableexist(string name){
		if(find(variables.begin(), variables.end(), name) == variables.end()){
			writestring("(defparameter " + name + " 0) ");
			variables.push_back(name);
			return false;
		}
		return true;
	}
public:
	TransCompiler(string fn) : Compiler(fn){

	}
	string destination = "LISP";
	void writestring(string s){
		lisp_code += s;
	}
	void popcode(){
		lisp_code.pop_back();
	}
	void compile(Astleaf e, bool isassgnment = false);
	uint8_t *getcode(int *size, bool isprint = false);
};
void TransCompiler::compile(Astleaf e, bool isassignment){
	Astleaf exp = e;
	if(exp.gettype() == (int)Type::number){
		double val = exp.getvalue();
		if(floor(val) == val){
			writestring(to_string((int)val));
		}else{
			writestring(to_string(val) + "d0");
		}
	}else if(exp.gettype() == (int)Type::name){
		if(exp.isleft() && !isnative(exp.getname())){
			writestring("(" + exp.getname());
			for(int i = 0; i < exp.getleft().getblocksize(); i++){writestring(" "); compile(exp.getleft().getblock(i));}
			writestring(")");
		}else if(exp.isleft() && isnative(exp.getname())){
			if(exp.getname() == "write"){
				writestring("(format t \"~d~%\" ");
				compile(exp.getleft().getblock(0));
				writestring(")");
			}else if(exp.getname() == "read"){
				writestring("(read)");
			}else if(exp.getname() == "return"){
				compile(exp.getleft().getblock(0));
			}else if(exp.getname() == "if"){
				if(exp.getleft().getblocksize() == 2){
					writestring("(when ");
					compile(exp.getleft().getblock(0));
					writestring(" ");
					compile(exp.getleft().getblock(1));
					writestring(")");
				}else if(exp.getleft().getblocksize() >= 3){
					writestring("(cond (");
					compile(exp.getleft().getblock(0));
					writestring(" ");
					compile(exp.getleft().getblock(1));
					writestring(") (t ");
					compile(exp.getleft().getblock(2));
					writestring("))");
				}
			}else if(exp.getname() == "makearray"){
				writestring("(make-array ");
				compile(exp.getleft().getblock(0));
				writestring(")");
			}
		}else{
			writestring(exp.getname());
		}
	}else if(exp.gettype() == (int)Type::expr){
		if(exp.getop() == "="){
			if(exp.getleft().getop() == "$"){
				writestring("(setf (aref " + exp.getleft().getleft().getname() + " ");
				compile(exp.getleft().getright());
				writestring(") ");
				compile(exp.getright());
				writestring(")");
			}else if(exp.getright().gettype() == (int)Type::number || exp.getright().gettype() == (int)Type::name  || exp.getright().gettype() == (int)Type::array || !exp.getleft().isleft() && exp.getright().gettype() == (int)Type::expr){
				isvariableexist(exp.getleft().getname());
				writestring("(setq " + exp.getleft().getname() + " ");
				compile(exp.getright());
				writestring(")");
			}else if(exp.getright().gettype() == (int)Type::block || exp.getleft().isleft() && exp.getright().gettype() == (int)Type::expr){
				writestring("(defun " + exp.getleft().getname() + " (");
				for(int i = 0; exp.getleft().isleft() && i < exp.getleft().getleft().getblocksize(); i++){
					writestring(exp.getleft().getleft().getblock(i).getname() + " ");
				}
				writestring(") ");
				compile(exp.getright());
				writestring(")");
			}
		}else if(exp.getop() == "?"){
			writestring("(if ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright().getleft());
			writestring(" ");
			compile(exp.getright().getright());
			writestring(")");
		}else if(exp.getop() == "=="){
			writestring("(= ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "!="){
			writestring("(/= ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "<"){
			writestring("(< ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == ">"){
			writestring("(> ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "+"){
			writestring("(+ ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "-"){
			writestring("(- ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "*"){
			writestring("(* ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "/"){
			writestring("(/ ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "%"){
			writestring("(mod ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "^"){
			writestring("(expt ");
			compile(exp.getleft());
			writestring(" ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "@"){
			writestring("(aref " + exp.getleft().getname() + " ");
			compile(exp.getright());
			writestring(")");
		}else if(exp.getop() == "$"){
			//writestring("(aref " + exp.getleft().getname() + " ");
			//compile(exp.getright());
			//writestring(")");
		}else{
			yetexception("ecasdfwacdsfwg;;; " + exp.getop(), filename);
		}
	}else if(exp.gettype() == (int)Type::array){
		writestring("#(");
		for(int i = 0; i < exp.getblocksize(); i++){
			compile(exp.getblock(i));
			writestring(" ");
		}
		writestring(")");
	}else if(exp.gettype() == (int)Type::block){
		bool isdeclare = exp.getblock(0).getname() == "declare";
		if(isdeclare){
			writestring(" (let (");
			for(int i = 0; i < exp.getblock(0).getleft().getblocksize(); i++){
				writestring(exp.getblock(0).getleft().getblock(i).getname() + " ");
				variables.push_back(exp.getblock(0).getleft().getblock(i).getname());
			}
			writestring(")");
		}
		for(int i = isdeclare ? 1 : 0; i < exp.getblocksize(); i++){
			writestring(" ");
			compile(exp.getblock(i));
		}
		if(isdeclare){
			writestring(")");
			for(int i = 0; i < exp.getblock(0).getleft().getblocksize(); i++)
				variables.pop_back();
		}
	}else if(exp.gettype() == (int)Type::code){
		for(int i = 0; i < exp.getblocksize(); i++){
			compile(exp.getblock(i));
			writestring("\n");
		}
	}
}
uint8_t *TransCompiler::getcode(int *size, bool isprint){
	if(isprint){
		cout << lisp_code << endl;
	}
	int len = lisp_code.length();
	uint8_t *subcode = new uint8_t[len];
	for(int i = 0; i < len; i++){
		subcode[i] = lisp_code.at(i);
	}
	*size = len;
	return subcode;
}

int main(int argc, char* argv[]){
	string in_file_name;
	string out_file_name = "a.ye";
	int compile_type = 0;
	bool is_print = false;
	int opt;
	while((opt = getopt(argc, argv, "pLo:h")) != -1){
		switch(opt){
			case 'p':
				is_print = true;
				break;
			case 'L':
				out_file_name = out_file_name == "a.ye"? "a.lisp" : out_file_name;
				compile_type = 1;
				break;
			case 'o':
				out_file_name = optarg;
				break;
			case 'h':
				cout << "Usage: " << argv[0] << " [-h] [-L] [-p] [-o filename]" << endl;
				return 0;
		}
	}
	in_file_name = argc <= 1? "error" : argv[argc - 1];

	cout.fill('0');
	Parser parser = Parser(in_file_name);
	int size;
	uint8_t *code;
	if(compile_type == 1){
		TransCompiler *compiler = new TransCompiler(in_file_name);
		compiler->compile(parser.code(), false);
		code = compiler->getcode(&size, is_print);
	}else{
		Compiler *compiler = new Compiler(in_file_name);
		compiler->compile(parser.code(), false);
		code = compiler->getcode(&size, is_print);
	}
	parser.closefile();
	ofstream out_file;
	out_file.open(out_file_name, ios::out|ios::binary|ios::trunc);
	for(int i = 0; i < size; i++){
		out_file.write((char *)&code[i], sizeof(code[0]));
	}
	out_file.close();
	return 0;
}
