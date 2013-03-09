// Starter code for CS241 assignments 9-11
//
// C++ translation by Simon Parent (Winter 2011),
// based on Java code by Ondrej Lhotak,
// which was based on Scheme code by Gord Cormack.
// Modified July 3, 2012 by Gareth Davies
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
using namespace std;

// The set of terminal symbols in the WLPP grammar.
const char *terminals[] = {
  "BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", "GE", "GT", "ID",
  "IF", "INT", "LBRACE", "LE", "LPAREN", "LT", "MINUS", "NE", "NUM",
  "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI",
  "SLASH", "STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW",
  "DELETE", "NULL"
};

int isTerminal(const string &sym) {
  int idx;
  for(idx=0; idx<sizeof(terminals)/sizeof(char*); idx++)
    if(terminals[idx] == sym) return 1;
  return 0;
}

// Data structure for storing the parse tree.
class tree {
public:
  string rule;
  vector<string> tokens;
  vector<tree*> children;
  ~tree() { for(int i=0; i<children.size(); i++) delete children[i]; }
};

// Call this to display an error message and exit the program.
void bail(const string &msg) {
  // You can also simply throw a string instead of using this function.
  throw string(msg);
}

// Read and return wlppi parse tree.
tree *readParse(const string &lhs) {
  // Read a line from standard input.
  string line;
  getline(cin, line);
  if(cin.fail())
    bail("ERROR: Unexpected end of file.");
  tree *ret = new tree();
  // Tokenize the line.
  stringstream ss;
  ss << line;
  while(!ss.eof()) {
    string token;
    ss >> token;
    if(token == "") continue;
    ret->tokens.push_back(token);
  }
  // Ensure that the rule is separated by single spaces.
  for(int idx=0; idx<ret->tokens.size(); idx++) {
    if(idx>0) ret->rule += " ";
    ret->rule += ret->tokens[idx];
  }
  // Recurse if lhs is a nonterminal.
  if(!isTerminal(lhs)) {
    for(int idx=1/*skip the lhs*/; idx<ret->tokens.size(); idx++) {
      ret->children.push_back(readParse(ret->tokens[idx]));
    }
  }
  return ret;
}

tree *parseTree;

string exprCode(tree*, map<string, string>&);
string statementsCode(tree*, map<string, string>& symbols);
string factorCode(tree*, map<string, string>& symbols);
string compute_type(tree*, map<string, string>&); 

string labelMaker(){
  static int labelCounter = 0;
  string label;
  stringstream ss;
  ss << labelCounter;
  ss >> label;
  labelCounter++;
  return label;
}

string loadAddress(string name, string reg){
  return "lis $" + reg + "\n" + ".word V" + name + "\n";
}

string loadWord(string to, string from, string offset){
  return "lw $" + to + ", " + offset + "($" + from + ")\n";
}

string storeWord(string to, string from, string offset){
  return "sw $" + from + ", " + offset + "($" + to + ")\n";
}

string push(string reg){
  return storeWord("30", reg, "-4") + "lis $" + reg + "\n" + ".word 4\n" + "sub $30, $30, $" + reg + "\n";
}

string pop(string reg){
  return "lis $" + reg + "\n" + ".word 4\n" + "add $30, $30, $" + reg + "\n" + loadWord(reg, "30", "-4");
}

string addressCode(tree* t, map<string, string>& symbols){
  if(t->rule == "lvalue ID"){
    string name = t->children[0]->tokens[1];
    return loadAddress(name, "3");
  }
  if(t->rule == "lvalue LPAREN lvalue RPAREN"){
    return addressCode(t->children[1], symbols);
  }
  if(t->rule == "lvalue STAR factor"){
    return factorCode(t->children[1], symbols);
  }
  return "";
}

string factorCode(tree* t, map<string, string>& symbols){
  if(t->rule == "factor ID"){
    string name = t->children[0]->tokens[1];
    return loadAddress(name, "4") + loadWord("3", "4", "0");
  }
  if(t->rule == "factor LPAREN expr RPAREN"){
    return exprCode(t->children[1], symbols);
  }
  if(t->rule == "factor NUM"){
    string num = t->children[0]->tokens[1];
    return "lis $3\n.word " + num + "\n";
  }
  if(t->rule == "factor NULL"){
    return "lis $3\n.word 1\n";
  }
  if(t->rule == "factor AMP lvalue"){
    return addressCode(t->children[1], symbols);
  }
  if(t->rule == "factor STAR factor"){
    return factorCode(t->children[1], symbols) + loadWord("3", "3", "0");
  }
  if(t->rule == "factor NEW INT LBRACK expr RBRACK"){
    return exprCode(t->children[3], symbols) + "add $1, $3, $0\n" + "lis $3\n" + ".word new\n" + "jalr $3\n";
  }
  return "";
}

string termCode(tree* t, map<string, string>& symbols){
  if(t->rule == "term factor"){
    return factorCode(t->children[0], symbols);
  }
  if(t->rule == "term term STAR factor"){
    return termCode(t->children[0], symbols) + push("3") + factorCode(t->children[2], symbols) + pop("1") + "mult $3, $1\n" + "mflo $3\n";
  }
  if(t->rule == "term term SLASH factor"){
    return termCode(t->children[0], symbols) + push("3") + factorCode(t->children[2], symbols) + pop("1") + "div $1, $3\n" + "mflo $3\n";
  }
  if(t->rule == "term term PCT factor"){
    return termCode(t->children[0], symbols) + push("3") + factorCode(t->children[2], symbols) + pop("1") + "div $1, $3\n" + "mfhi $3\n";
  }
  return "";
}

string exprCode(tree* t, map<string, string>& symbols){
  if(t->rule == "expr term"){
    return termCode(t->children[0], symbols);
  }
  if(t->rule == "expr expr PLUS term"){
    string type1 = compute_type(t->children[0], symbols);
    string type2 = compute_type(t->children[2], symbols);
    if(type1 == "int" && type2 == "int"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "add $3, $1, $3\n";
    }
    if(type1 == "int*" && type2 == "int"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "lis $4\n" + ".word 4\n" + "mult $3, $4\n" + "mflo $3\n" + "add $3, $1, $3\n";
    }
    if(type1 == "int" && type2 == "int*"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "lis $4\n" +".word 4\n" + "mult $1, $4\n" +"mflo $1\n" + "add $3, $1, $3\n";
    }
    bail("ERROR");
  }
  if(t->rule == "expr expr MINUS term"){
    string type1 = compute_type(t->children[0], symbols);
    string type2 = compute_type(t->children[2], symbols);
    if(type1 == "int" && type2 == "int"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "sub $3, $1, $3\n";
    }
    
    if(type1 == "int*" && type2 == "int"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "lis $4\n" + ".word 4\n" + "mult $3, $4\n" + "mflo $3\n" + "sub $3, $1, $3\n";
    }
    if(type1 == "int*" && type2 == "int*"){
      return exprCode(t->children[0], symbols) + push("3") + termCode(t->children[2], symbols) + pop("1") + "sub $3, $1, $3\n";
    }
    bail("ERROR");
  }
  return "";
}

string testCode(tree* t, string label, map<string, string>& symbols){
  string rule = t->rule;
  string type1 = compute_type(t->children[0], symbols);
  string type2 = compute_type(t->children[2], symbols);
  if(rule == "test expr EQ expr"){
    return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sub $1, $1, $3\n" + "bne $1, $0, L" + label + "\n";
  }
  if(rule == "test expr NE expr"){
    return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sub $1, $1, $3\n" + "beq $1, $0, L" +label +"\n";
  }
  if(type1 == "int" && type2 == "int"){
    if(rule == "test expr LT expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "slt $1, $1, $3\n" + "beq $1, $0, L" + label + "\n";
    }
    if(rule == "test expr GE expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "slt $1, $1, $3\n" + "bne $1, $0, L" + label + "\n";
    }
    if(rule == "test expr LE expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "slt $1, $3, $1\n" + "bne $1, $0, L" + label + "\n";
    }
    if(rule == "test expr GT expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "slt $1, $3, $1\n" + "beq $1, $0, L" + label + "\n";
    }
  }
  if(type1 == "int*" && type2 == "int*"){
    if(rule == "test expr LT expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sltu $1, $1, $3\n" + "beq $1, $0, L" + label + "\n";
    }
    if(rule == "test expr GE expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sltu $1, $1, $3\n" + "bne $1, $0, L" + label + "\n";
    }
    if(rule == "test expr LE expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sltu $1, $3, $1\n" + "bne $1, $0, L" + label + "\n";
    }
    if(rule == "test expr GT expr"){
      return exprCode(t->children[0], symbols) + push("3") + exprCode(t->children[2], symbols) + pop("1") + "sltu $1, $3, $1\n" + "beq $1, $0, L" + label + "\n";
    }
  }
  bail("ERROR");
  return "";
}

string statementCode(tree* t, map<string, string>& symbols){
  if(t->rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
    return exprCode(t->children[2], symbols) + "add $1, $3, $0\n" + "lis $29\n" + ".word print\n" + "jalr $29\n";
  }
  if(t->rule == "statement lvalue BECOMES expr SEMI"){
    return exprCode(t->children[2], symbols) + push("3") + addressCode(t->children[0], symbols) + pop("1") + storeWord("3", "1", "0");
  }
  if(t->rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
    string begin = labelMaker();
    string end = labelMaker();
    return "L" + begin + ":\n" + testCode(t->children[2], end, symbols) + statementsCode(t->children[5], symbols) + "beq $0, $0, L" + begin + "\n" + "L" + end + ":\n";
  }
  if(t->rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){
    string end = labelMaker();
    string ELSE = labelMaker();
    return testCode(t->children[2], ELSE, symbols) + statementsCode(t->children[5], symbols) + "beq $0, $0, L" + end + "\n" + "L" + ELSE + ":\n" + statementsCode(t->children[9], symbols) + "L" + end +":\n";
  }
  if(t->rule == "statement DELETE LBRACK RBRACK expr SEMI"){
    return exprCode(t->children[3], symbols) + "add $1, $3, $0\n" + "lis $3\n" + ".word delete\n" + "jalr $3\n";
  }
  return "";
}

string statementsCode(tree* t, map<string, string>& symbols){
  if(t->rule == "statements statements statement"){
    return statementsCode(t->children[0], symbols) + statementCode(t->children[1], symbols);
  }
  return "";
}

string declarationCode(tree* t, string num, map<string, string>& symbols){
  if(t->rule == "dcl type ID"){
    string name = t->children[1]->tokens[1];
    return push("4") + push("5") + loadAddress(name, "4") + "lis $5\n" + ".word " + num + "\n" + storeWord("4", "5", "0") + pop("5") + pop("4");
  }
  return "";
}

string declarationsCode(tree* t, map<string, string>& symbols){
  if(t->rule == "dcls dcls dcl BECOMES NUM SEMI"){
    string num = t->children[3]->tokens[1];
    return declarationsCode(t->children[0], symbols) + declarationCode(t->children[1], num, symbols);
  }
  if(t->rule == "dcls dcls dcl BECOMES NULL SEMI"){
    return declarationsCode(t->children[0], symbols) + declarationCode(t->children[1], "1", symbols);
  }
  return "";
}

string compute_type(tree* t, map<string, string>& symbol_table){
  string rule = t->rule;

  //dcls rules
  if(rule == "dcls dcls dcl BECOMES NUM SEMI"){
    tree* ID = t->children[1]->children[1];
    if(symbol_table[ID->tokens[1]] != "int"){
      bail("ERROR");
    }
    return "int";
  }

  if(rule == "dcls dcls dcl BECOMES NULL SEMI"){
    tree* ID = t->children[1]->children[1];
    if(symbol_table[ID->tokens[1]] != "int*"){
      bail("ERROR");
    }
    return "int*";
  }

  //factor rules
  if(rule == "factor ID"){
    return symbol_table[t->children[0]->tokens[1]];
  }
  if(rule == "factor NUM") return "int";
  if(rule == "factor NULL") return "int*";
  if(rule == "factor LPAREN expr RPAREN"){
    return compute_type(t->children[1], symbol_table);
  }
  if(rule == "factor AMP lvalue"){
    if(compute_type(t->children[1], symbol_table) != "int"){
      bail("ERROR");// lvalue must be of type int in &lvalue");
    }
    return "int*";
  }
  if(rule == "factor STAR factor"){
    if(compute_type(t->children[1], symbol_table) != "int*"){
      bail("ERROR");//: factor must be of type int* in *factor");
    }
    return "int";
  }
  if(rule == "factor NEW INT LBRACK expr RBRACK"){
    if(compute_type(t->children[3], symbol_table) != "int"){
      bail("ERROR");//: expr must be of type int in new int expr");
    }
    return "int*";
  }
  
  //expr rules
  if(rule == "expr term"){
    return compute_type(t->children[0], symbol_table);
  }
  if(rule == "expr expr PLUS term"){
    string type_left = compute_type(t->children[0], symbol_table);
    string type_right = compute_type(t->children[2], symbol_table);
    if(type_left == "int*" && type_right == "int*"){
      bail("ERROR");//: mismatched types in expr + term");
    }//other combinations of int and int* are valid
    if(type_left == "int" && type_right == "int") return "int";
    return "int*";
  }
  if(rule == "expr expr MINUS term"){
    string type_left = compute_type(t->children[0], symbol_table);
    string type_right = compute_type(t->children[2], symbol_table);
    if(type_left == "int" && type_right == "int*"){
      bail("ERROR");//: mismatched types in expr - term");
    }//other combinations of int and int* are valid
    if(type_left == "int*" && type_right == "int") return "int*";
    return "int";
  }

  //lvalue rules
  if(rule == "lvalue ID"){
    return symbol_table[t->children[0]->tokens[1]];
  }
  if(rule == "lvalue STAR factor"){
    if(compute_type(t->children[1], symbol_table) != "int*"){
      bail("ERROR");//: factor must be of type int* in *factor");
    }
    return "int";
  }
  if(rule == "lvalue LPAREN lvalue RPAREN"){
    return compute_type(t->children[1], symbol_table);
  }

  //term rules
  if(rule == "term factor"){
    return compute_type(t->children[0], symbol_table);
  }
  if(rule == "term term STAR factor" || rule == "term term SLASH factor" || rule == "term term PCT factor"){
    string type_left = compute_type(t->children[0], symbol_table);
    string type_right = compute_type(t->children[2], symbol_table);
    if(type_left != "int" || type_right != "int"){
      bail("ERROR");//: both term and factor must be of type int in term op factor");
    }
    return "int";
  }

  //statement rules
  if(rule == "statement lvalue BECOMES expr SEMI"){
    string type_left = compute_type(t->children[0], symbol_table);
    string type_right = compute_type(t->children[2], symbol_table);
    if(type_left != type_right){
      bail("ERROR");//: equal operator requires elements of same type");
    }
    return type_left;
  }
  if(rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
    if(compute_type(t->children[2], symbol_table) != "int"){
      bail("ERROR");//: print requires int argument");
    }
    return "int";
  }
  if(rule == "statement DELETE LBRACK RBRACK expr SEMI"){
    if(compute_type(t->children[3], symbol_table) != "int*"){
      bail("ERROR");//: delete requires pointer argument");
    }
    return "int*";
  }
  
  //test rules
  if(t->tokens[0] == "test"){
    string type_left = compute_type(t->children[0], symbol_table);
    string type_right = compute_type(t->children[2], symbol_table);
    if(type_left != type_right){
      bail("ERROR");//: comparisons require arguments of same type
    }
    return type_left;
  }

  //for all other nodes, recurse
  for(int i = 0; i < t->children.size(); i++){
    compute_type(t->children[i], symbol_table);
  }
  return "";
}

void well_typed(tree* t, map<string, string>& symbol_table){
  for(int i = 0; i < t->children.size(); i++){
    if(i == t->children.size() - 3) break;//grab return expr and finish
    string s = compute_type(t->children[i], symbol_table);
  }
  if(compute_type(t->children[t->children.size() - 3], symbol_table) != "int"){
    bail("ERROR");
  }
  return;
}

void build_symbol_table(tree* parse_tree, map<string, string>& symbol_table){
  if(parse_tree->rule == "dcl type ID"){//node is dcl
    tree* type_data = parse_tree->children[0];//find type of ID
    stringstream ss(parse_tree->children[1]->rule);
    string name;
    string ID;
    ss >> ID >> name;//find name of ID
    if(symbol_table.count(name)){
      bail("ERROR");//duplicate declarations for name
    }
    if(type_data->rule == "type INT"){
      symbol_table[name] = "int"; return;
    }
    symbol_table[name] = "int*"; return;
  }
  if(parse_tree->tokens[0] == "dcls"){//node is dcls
    if(parse_tree->rule == "dcls") return;//dcls empty, done.
    for(int i = 0; i < parse_tree->children.size(); i++){//otherwise, recurse
      build_symbol_table(parse_tree->children[i], symbol_table);
    }
    return;
  }
  if(parse_tree->rule == "factor ID" || parse_tree->rule == "lvalue ID"){
    stringstream ss(parse_tree->children[0]->rule);
    string ID;
    string name;
    ss >> ID >> name;
    if(!symbol_table.count(name)){
      bail("ERROR");//variable name not defined
    }
    return;
  }
  for(int i = 0; i < parse_tree->children.size(); i++){
    build_symbol_table(parse_tree->children[i], symbol_table);
  }
  return;
}

void print_symbols(map<string, string>& symbol_table){
  map<string, string>::iterator p = symbol_table.begin();
  for(p; p != symbol_table.end(); p++){
    cerr << p->first  << " " << p->second << endl;
  }
  return;
}

string procedureCode(tree* t, map<string, string>& symbols){
  tree* dcl1 = t->children[3];
  tree* dcl2 = t->children[5];
  tree* dcls = t->children[8];
  tree* ret = t->children[11];
  tree* states = t->children[9];
  string name1 = dcl1->children[1]->tokens[1];
  string name2 = dcl2->children[1]->tokens[1];
  string output("");
  output += push("31") + loadAddress(name1, "3") + storeWord("3", "1", "0") + loadAddress(name2, "3") + storeWord("3", "2", "0");
  if(symbols[name1] == "int"){
    output += "add $2, $0, $0\n";
  }
  output += "lis $5\n.word init\njalr $5\n" + declarationsCode(dcls, symbols) + statementsCode(states, symbols) + exprCode(ret, symbols) + pop("31") + "jr $31\n";
  map<string, string>::iterator p = symbols.begin();
  for(p; p != symbols.end(); p++){
    output = output + "V" + p->first + ": .word 0\n";
  }
  int size = output.size();
  output = output.substr(0, size - 1);
  output += "\nprint:\nsw $1, -4($30)\nsw $2, -8($30)\nsw $3, -12($30)\nsw $4, -16($30)\nsw $5, -20($30)\nsw $6, -24($30)\nsw $7, -28($30)\nsw $8, -32($30)\nsw $9, -36($30)\nsw $10, -40($30)\nlis $3\n.word -40\nadd $30, $30, $3\nlis $3\n.word 0xffff000c\nlis $4\n.word 10\nlis $5\n.word 4\nadd $6, $1, $0\nslt $7, $1, $0\nbeq $7, $0, IfDone\nlis $8\n.word 0x0000002d\nsw $8, 0($3)\nsub $6, $0, $6\nIfDone:\nadd $9, $30, $0\nLoop:\ndivu $6, $4\nmfhi $10\nsw $10, -4($9)\nmflo $6\nsub $9, $9, $5\nslt $10, $0, $6\nbne $10, $0, Loop\nlis $7\n.word 48\nLoop2:\nlw $8, 0($9)\nadd $8, $8, $7\nsw $8, 0($3)\nadd $9, $9, $5\nbne $9, $30, Loop2\nsw $4, 0($3)\nlis $3\n.word 40\nadd $30, $30, $3\nlw $1, -4($30)\nlw $2, -8($30)\nlw $3, -12($30)\nlw $4, -16($30)\nlw $5, -20($30)\nlw $6, -24($30)\nlw $7, -28($30)\nlw $8, -32($30)\nlw $9, -36($30)\nlw $10, -40($30)\njr $31\ninit:\n   sw $1, -4($30)\n   sw $2, -8($30)\n   sw $3, -12($30)\n   sw $4, -16($30)\n   sw $5, -20($30)\n   sw $6, -24($30)\n   sw $7, -28($30)\n   sw $8, -32($30)\n   lis $4\n   .word 32\n   sub $30, $30, $4\n   lis $1\n   .word end\n   lis $3\n   .word 1024       ; space for free list (way more than necessary)\n   lis $6\n   .word 16         ; size of bookkeeping region at end of program\n   lis $7\n   .word 4096       ; size of heap\n   lis $8\n   .word 1\n   add $2, $2, $2   ; Convert array length to words (*4)\n   add $2, $2, $2\n   add $2, $2, $6   ; Size of OS added by loader\n   add $5, $1, $6   ; end of program + length of bookkeeping\n   add $5, $5, $2   ; + length of incoming array\n   add $5, $5, $3   ; + length of free list\n   sw $5, 0($1)     ; store address of heap at Mem[end]\n   add $5, $5, $7   ; store end of heap at Mem[end+4]\n   sw $5, 4($1)\n   sw $8, 8($1)     ; store initial size of free list (1) at Mem[end+8]\n   add $5, $1, $6\n   add $5, $5, $2\n   sw $5, 12($1)   ; store location of free list at Mem[end+12]\n   sw $8, 0($5)    ; store initial contents of free list (1) at Mem[end+12]\n   sw $0, 4($5)    ; zero-terminate the free list\n   add $30, $30, $4\n   lw $1, -4($30)\n   lw $2, -8($30)\n   lw $3, -12($30)\n   lw $4, -16($30)\n   lw $5, -20($30)\n   lw $6, -24($30)\n   lw $7, -28($30)\n   lw $8, -32($30)\n   jr $31\nnew:\n   sw $1, -4($30)\n   sw $2, -8($30)\n   sw $4, -12($30)\n   sw $5, -16($30)\n   sw $6, -20($30)\n   sw $7, -24($30)\n   sw $8, -28($30)\n   sw $9, -32($30)\n   sw $10, -36($30)\n   sw $11, -40($30)\n   sw $12, -44($30)\n   lis $10\n   .word 44\n   sub $30, $30, $10\n   ;; Make sure requested size > 0 ; if not, bail out.\n   slt $3, $0, $1\n   beq $3, $0, cleanupN\n   lis $11   ; $11 = 1\n   .word 1\n   add $1, $1, $11 ; One extra word to store deallocation info\n   add $1, $1, $1  ; Convert $1 from words to bytes\n   add $1, $1, $1\n   add $2, $11, $11  ; $2 = 2\n   add $4, $0, $0  ; $4 = counter, to accumulate ceil(log($1))\n   ;; Repeatedly dividing $1 by 2 and counting the divisions gives\n   ;; floor (log($1)).  To get ceil(log($1)), evaluate floor(log($1-1))+1\n   sub $1, $1, $11  ; So subtract 1 from $1\n topN:  ; Repeatedly divide $1 by 2, and count iterations\n   beq $1, $0, endloopN\n   div $1, $2      ; $1 /= 2\n   mflo $1\n   add $4, $4, $11  ; $4++\n   beq $0, $0, topN\n endloopN:\n   add $1, $1, $11  ; Now add 1 to $1 to restore its value after previous sub\n   add $4, $4, $11  ; And add 1 to $4 to complete ceil calculation (see above)\n   ;; An address' allocation code will consist of $14-$4 bits\n   lis $5     ; $5 = 14\n   .word 14  \n   sub $4, $5, $4  ; $4 <- 14 - $4  \n   ;; Cap the number of bits in an allocation code at 9 (so we don't allocate\n   ;; blocks smaller than 4 words at a time).\n   lis $5\n   .word 9\n   slt $6, $5, $4 \n   beq $6, $0, doNotFixN\n   add $4, $5, $0\n doNotFixN:\n   ; Make sure requested size is not too big, i.e., $4>0\n   slt $3, $0, $4\n   beq $3, $0, cleanupN\n   ; Now search for a word in the free list with that many bits or fewer\n   ; (Fewer bits = larger block size)\n   ; Compute largest possible $4-bit number, store in $7\n   add $6, $4, $0    ; countdown from $4 to 0\n   add $7, $11, $0   ; accumulates result by doubling $4 times\n top2N:\n   add $7, $7, $7    ; double $7\n   sub $6, $6, $11   ; $6--\n   bne $6, $0, top2N\n   sub $7, $7, $11  ; At the end of the loop, $7 = 2^$4 - 1\n   ; Find largest word in freelist <= $7\n   lis $8\n   .word findWord\n   sw $31, -4($30)\n   lis $31\n   .word 4\n   sub $30, $30, $31\n   jalr $8          ; call findWord\n   lis $31\n   .word 4\n   add $30, $30, $31\n   lw $31, -4($30)\n   ; If no match found, cleanup and abort\n   beq $3, $0, cleanupN  ; if allocation fails, clean up and return 0\n   \n     ; Compute minimum code for exact match  (($7+1)/2)\n   add $7, $7, $11\n   div $7, $2\n   mflo $7\n   ; If exact match found, remove it from the free list\n exactN:\n   slt $6, $3, $7\n   bne $6, $0, largerN\n   beq $0, $0, convertN\n   ; If larger match found, split into smaller buddies\n largerN:  ;; buddies are 2$3 and 2$3+1\n   add $3, $3, $3 ;; double $3\n   ; add 2$3+1 to free list; evaluate 2$3 as possible candidate\n   lis $6   ;; $6 = address of address of free list\n   .word free\n   lw $8, -4($6)  ;; $8 = length of free list\n   lw $6, 0($6)   ;; $6 = address of free list\n   add $8, $8, $8 ;; convert to words (*4)\n   add $8, $8, $8\n   add $6, $6, $8 ;; address of next spot in free list\n   add $8, $3, $11 ;; $8 = buddy\n   sw $8, 0($6)   ;; add to end of list\n   sw $0, 4($6)\n   ;; increment length of free list\n   lis $6\n   .word free\n   lw $8, -4($6)\n   add $8, $8, $11\n   sw $8, -4($6)\n   ; now go back to exact with new value of $3, and re-evaluate\n   beq $0, $0, exactN\n   ; Convert number to address\n convertN:\n   add $12, $3, $0  ; retain original freelist word\n   add $7, $0, $0 ;; offset into heap\n   lis $8\n   .word end\n   lw $9, 4($8)  ;; end of heap\n   lw $8, 0($8)  ;; beginning of heap\n   sub $9, $9, $8 ;; size of heap (bytes)\n top5N:\n   beq $3, $11, doneconvertN\n   div $3, $2\n   mflo $3    ;; $3/2\n   mfhi $10   ;; $3%2\n   beq $10, $0, evenN\n   add $7, $7, $9   ;; add size of heap to offset\n evenN:\n   div $7, $2       ;; divide offset by 2\n   mflo $7\n   beq $0, $0, top5N\n doneconvertN:\n   add $3, $8, $7  ;; add start of heap to offset to get address\n   lis $4\n   .word 4\n   add $3, $3, $4  ;; advance one byte for deallocation info\n   sw $12, -4($3)  ;; store deallocation info\n cleanupN:\n   lis $10\n   .word 44\n   add $30, $30, $10\n   lw $1, -4($30)\n   lw $2, -8($30)\n   lw $4, -12($30)\n   lw $5, -16($30)\n   lw $6, -20($30)\n   lw $7, -24($30)\n   lw $8, -28($30)\n   lw $9, -32($30)\n   lw $10, -36($30)\n   lw $11, -40($30)\n   lw $12, -44($30)\n   jr $31\ndelete:\n   sw $1, -4($30)\n   sw $2, -8($30)\n   sw $3, -12($30)\n   sw $4, -16($30)\n   sw $5, -20($30)\n   sw $6, -24($30)\n   sw $11, -28($30)\n   sw $12, -32($30)\n   sw $14, -36($30)\n   lis $6\n   .word 36\n   sub $30, $30, $6\n   lis $11\n   .word 1\n   lis $12\n   .word 2\n   lis $14\n   .word 4\n   lw $2, -4($1) ;; buddy code for the allocated block\n nextBuddyD:\n   beq $2, $11, notFoundD  ;; if there is no buddy (i.e. buddy code=1), bail out\n   ;; compute buddy's buddy code  (i.e, add 1 if code is even, sub 1 if odd)\n   add $3, $2, $0\n   div $3, $12   ; $4 = $3 % 2\n   mfhi $4\n   beq $4, $0, evenD\n   sub $3, $3, $11\n   beq $0, $0, doneParityD\n evenD:\n   add $3, $3, $11\n doneParityD:\n   ;; Now search free list for the buddy; if found, remove, and divide the\n   ;; buddy code by 2; if not found, add current buddy code to the free list.\n   lis $5\n   .word findAndRemove\n   sw $31, -4($30)\n   sub $30, $30, $14\n   add $1, $3, $0\n   jalr $5\n   add $30, $30, $14\n   lw $31, -4($30)\n   ;; If the procedure succeeded in finding the buddy, $3 will be 1; else it\n   ;; will be 0.\n   beq $3, $0, notFoundD\n   div $2, $12\n   mflo $2\n   beq $0, $0, nextBuddyD\n  notFoundD:\n   lis $4   ;; address of address of free list\n   .word free\n   lw $5, -4($4) ; length of the free list\n   lw $4, 0($4)  ;; address of the free list\n   add $5, $5, $5  ; convert to offset\n   add $5, $5, $5\n   add $5, $4, $5  ; address of next spot in free list\n   sw $2, 0($5)    ; put code back into free list\n   sw $0, 4($5)    ; keep free list 0-terminated\n   ; update size of free list\n   lis $4\n   .word free\n   lw $5, -4($4)\n   add $5, $5, $11\n   sw $5, -4($4)\n   lis $6\n   .word 36\n   add $30, $30, $6\n   lw $1, -4($30)\n   lw $2, -8($30)\n   lw $3, -12($30)\n   lw $4, -16($30)\n   lw $5, -20($30)\n   lw $6, -24($30)\n   lw $11, -28($30)\n   lw $12, -32($30)\n   lw $14, -36($30)\n   jr $31\nfindWord:\n    sw $1, -4($30)\n    sw $2, -8($30)\n    sw $4, -12($30)\n    sw $5, -16($30)\n    sw $6, -20($30)\n    sw $7, -24($30)\n    sw $8, -28($30)\n    sw $9, -32($30)\n    sw $10, -36($30)\n    lis $1\n    .word 36\n    sub $30, $30, $1\n    \n    ;; $1 = start of free list\n    ;; $2 = length of free list\n    lis $1  ;; address of address of the free list\n    .word free\n    lw $2, -4($1)\n    lw $1, 0($1) ;; address of the free list\n    lis $4   ; $4 = 4 (for looping increments over memory)\n    .word 4\n    lis $9   ; $9 = 1 (for loop decrements)\n    .word 1\n    add $3, $0, $0  ;; initialize output to 0 (not found)\n    add $10, $0, $0 ;; for address of max word\n    beq $2, $0, cleanupFW  ;; skip if no free memory\n    add $5, $2, $0  ;; loop countdown to 0\n topFW:\n    lw $6, 0($1)\n    slt $8, $7, $6  ;; limit < current item (i.e. item ineligible?)\n    bne $8, $0, ineligibleFW\n    slt $8, $3, $6  ;; max < current item?\n    beq $8, $0, ineligibleFW  ; if not, skip to ineligible\n    add $3, $6, $0  ;; replace max with current\n    add $10, $1, $0 ;; address of current\n ineligibleFW:\n    add $1, $1, $4  ;; increment address\n    sub $5, $5, $9  ;; decrement loop counter\n    bne $5, $0, topFW     ;; if items left, continue looping\n ;; if candidate not found, bail out (if not found, $3 will still be 0)\n    beq $3, $0, cleanupFW\n ;; now loop from $10 to end, moving up array elements\n top2FW:\n    lw $6, 4($10)  ;; grab next element in array\n    sw $6, 0($10)  ;; store in current position\n    add $10, $10, $4 ;; increment address\n    bne $6, $0, top2FW  ;; continue while elements nonzero\n ;; decrement length of free list\n    lis $2\n    .word end\n    lw $4, 8($2)\n    sub $4, $4, $9  ; $9 still 1\n    sw $4, 8($2)\n cleanupFW:\n    lis $1\n    .word 36\n    add $30, $30, $1\n    lw $1, -4($30)\n    lw $2, -8($30)\n    lw $4, -12($30)\n    lw $5, -16($30)\n    lw $6, -20($30)\n    lw $7, -24($30)\n    lw $8, -28($30)\n    lw $9, -32($30)\n    lw $10, -36($30)\n    jr $31\nfindAndRemove:\n   sw $1, -4($30)\n   sw $2, -8($30)\n   sw $4, -12($30)\n   sw $5, -16($30)\n   sw $6, -20($30)\n   sw $7, -24($30)\n   sw $8, -28($30)\n   sw $9, -32($30)\n   sw $11, -36($30)\n   sw $14, -40($30)\n   lis $9\n   .word 40\n   sub $30, $30, $9\n   lis $11\n   .word 1\n   lis $14\n   .word 4\n   lis $2     ;; address of address of the free list\n   .word free\n   lw $4, -4($2) ;; length of the free list\n   lw $2, 0($2)  ;; address of the free list\n\n   add $3, $0, $0 ; success code\n   add $6, $0, $0 ; address of found code\n   add $7, $0, $0 ; loop counter\n topFaR:  ; loop through free list, looking for the code\n   beq $4, $0, cleanupFaR\n   lw $5, 0($2) ; next code in list\n   bne $5, $1, notEqualFaR  ;; compare with input\n   add $6, $6, $2  ; if code found, save its address\n   beq $0, $0, removeFaR\n notEqualFaR:  ; current item not the one we're looking for; update counters\n   add $2, $2, $14\n   add $7, $7, $11\n   bne $7, $4, topFaR\n removeFaR:\n   beq $6, $0, cleanupFaR  ;; if code not found, bail out\n top2FaR:  ; now loop through the rest of the free list, moving each item one\n           ; slot up\n   lw $8, 4($2)\n   sw $8, 0($2)\n   add $2, $2, $14  ; add 4 to current address\n   add $7, $7, $11  ; add 1 to loop counter\n   bne $7, $4, top2FaR\n   add $3, $11, $0  ;; set success code\n   ;; decrement size\n   lis $2\n   .word free\n   lw $5, -4($2)\n   sub $5, $5, $11\n   sw $5, -4($2)\n cleanupFaR:\n   lis $9\n   .word 40\n   add $30, $30, $9\n   lw $1, -4($30)\n   lw $2, -8($30)\n   lw $4, -12($30)\n   lw $5, -16($30)\n   lw $6, -20($30)\n   lw $7, -24($30)\n   lw $8, -28($30)\n   lw $9, -32($30)\n   lw $11, -36($30)\n   lw $14, -40($30)\n   jr $31\nprintFreeList:\n   sw $1, -4($30)\n   sw $2, -8($30)\n   sw $3, -12($30)\n   sw $4, -16($30)\n   sw $5, -20($30)\n   sw $6, -24($30)\n   sw $7, -28($30)\n   sw $8, -32($30)\n   lis $6\n   .word 32\n   sub $30, $30, $6\n   lis $3   ; address of address of the start of the free list\n   .word free\n   lis $4\n   .word 4\n   lis $5   ; external print procedure\n   .word print\n   lis $6\n   .word 1\n   lw $2, -4($3) ; $2 = length of free list; countdown to 0 for looping\n   lw $3, 0($3) ; $3 = address of the start of the free list\n   ;; loop through the free list, and print each element\n topPFL:\n   beq $2, $0, endPFL  ;; skip if free list empty\n   lw $1, 0($3)     ; store in $1 the item to be printed\n   sw $31, -4($30)\n   sub $30, $30, $4\n   jalr $5          ; call external print procedure\n   add $30, $30, $4\n   lw $31, -4($30)\n   add $3, $3, $4   ; update current address and loop counter\n   sub $2, $2, $6\n   bne $2, $0, topPFL\n endPFL:\n   ;; add an extra newline at the end, so that if this procedure is called\n   ;; multiple times, we can distinguish where one call ends and the next\n   ;; begins\n   lis $6\n   .word 0xffff000c\n   lis $5\n   .word 10\n   sw $5, 0($6)\n   lis $6\n   .word 32\n   add $30, $30, $6\n   lw $1, -4($30)\n   lw $2, -8($30)\n   lw $3, -12($30)\n   lw $4, -16($30)\n   lw $5, -20($30)\n   lw $6, -24($30)\n   lw $7, -28($30)\n   lw $8, -32($30)\n   jr $31\nend:\n   .word 0 ;; beginnning of heap\n   .word 0 ;; end of heap\n   .word 0 ;; length of free list\nfree: .word 0 ;; beginning of free list\n";
  return output;
}

int main() {
  // Main program.
  try {
    map<string, string> symbol_table;
    parseTree = readParse("S");
    build_symbol_table(parseTree, symbol_table);
    parseTree = parseTree->children[1];
    string s = procedureCode(parseTree, symbol_table);
    cout << s << endl;
  } catch(string msg) {
    cerr << msg << endl;
  }
  if (parseTree) delete parseTree;
  return 0;
}
