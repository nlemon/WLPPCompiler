/*  CS241 Scanner

    Starter code for the CS 241 assembler (assignments 3 and 4).
    Code contained here may be included in submissions for CS241
    assignments at the University of Waterloo.

    ---------------------------------------------------------------

    To compile on a CSCF linux machine, use:

            g++ -g asm.cc -o asm

    To run:
            ./asm           < source.asm > program.mips
            valgrind ./asm  < source.asm > program.mips
*/

#include <string>
#include <vector>
#include <iostream>
#include <cstdio>
using namespace std;

//======================================================================
//========= Declarations for the scan() function =======================
//======================================================================

// Each token has one of the following kinds.
string kind_strings [] = {"RETURN", "IF", "ELSE", "WHILE", "PRINTLN", "WAIN", "NEW", "DELETE", "ID", "NUM", "COMMA", "SEMI", "INT", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "EQ", "NE", "LT", "GT", "LE", "GE", "PLUS", "MINUS", "STAR", "SLASH", "PCT", "LBRACK", "RBRACK", "AMP", "BECOMES", "WHITESPACE", "NULL"};

enum Kind {
  RETURN,
  IF,
  ELSE,
  WHILE,
  PRINTLN,
  WAIN,
  NEW,
  DELETE,
  ID,                 // Opcode or identifier (use of a label)
  NUM,                // Decimal integer
  COMMA,              // Comma
  SEMI,
  INT,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  EQ,
  NE,
  LT,
  GT,
  LE,
  GE,
  PLUS,
  MINUS,
  STAR,
  SLASH,
  PCT,
  LBRACK,
  RBRACK,
  AMP,
  BECOMES,
  WHITESPACE,         
  NUL                 
};

// kindString(k) returns string a representation of kind k
// that is useful for error and debugging messages.
//string kindString(Kind k);

// Each token is described by its kind and its lexeme.

struct Token {
  Kind      kind;
  string    lexeme;
  /* toInt() returns an integer representation of the token. For tokens
   * of kind INT (decimal integer constant) and HEXINT (hexadecimal integer
   * constant), returns the integer constant. For tokens of kind
   * REGISTER, returns the register number.
   */
  //int       toInt();
};

// scan() separates an input line into a vector of Tokens.
vector<Token> scan(string input);

// =====================================================================
// The implementation of scan() and associated type definitions.
// If you just want to use the scanner, skip to the next ==== separator.

// States for the finite-state automaton that comprises the scanner.

enum State {
  ST_NUL,
  ST_START,
  ST_ID,
  ST_NUM,
  ST_LPAREN,
  ST_RPAREN,
  ST_LBRACE,
  ST_RBRACE,
  ST_BECOMES,
  ST_EQ,
  ST_NE,
  ST_LT,
  ST_GT,
  ST_LE,
  ST_GE,
  ST_PLUS,
  ST_MINUS,
  ST_STAR,
  ST_SLASH,
  ST_PCT,
  ST_COMMA,
  ST_SEMI,
  ST_LBRACK,
  ST_RBRACK,
  ST_AMP,
  ST_NOT,
  ST_COMMENT,
  ST_WHITESPACE,
  ST_I,
  ST_IN,
  ST_INT
};

// The *kind* of token (see previous enum declaration)
// represented by each state; states that don't represent
// a token have stateKinds == NUL.

Kind stateKinds[] = {
  NUL,            // ST_NUL
  NUL,            // ST_START
  ID,             // ST_ID
  NUM,            // ST_NUM
  LPAREN,         // ST_LPAREN
  RPAREN,         // ST_RPAREN
  LBRACE,         // ST_LBRACE
  RBRACE,         // ST_RBRACE
  BECOMES,        // ST_BECOMES
  EQ,
  NE,
  LT,
  GT,
  LE,
  GE,
  PLUS,
  MINUS,
  STAR,
  SLASH,
  PCT,
  COMMA,
  SEMI,
  LBRACK,
  RBRACK,
  AMP,
  NUL,            // ST_NOT
  WHITESPACE,     // ST_COMMENT
  WHITESPACE,      // ST_WHITESPACE
  ID,
  ID,
  INT
};

State delta[ST_INT+1][256];

#define whitespace "\t\n\r "
#define letters    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
#define noi "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghjklmnopqrstuvwxyz"
#define digits     "0123456789"
#define oneToNine  "123456789"
#define everything "(){}[]=!<>+-*/%,;&"
#define not_n "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmopqrstuvwxyz"
#define not_t "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrsuvwxyz"

void setT(State from, string chars, State to) {
  for(int i = 0; i < chars.length(); i++ ) delta[from][chars[i]] = to;
}

void initT(){
  int i, j;

  // The default transition is ST_NUL (i.e., no transition
  // defined for this char).
  for ( i=0; i<=ST_INT; i++ ) {
    for ( j=0; j<256; j++ ) {
      delta[i][j] = ST_NUL;
    }
  }
  // Non-null transitions of the finite state 
  // NB: in the third line below, letters digits are macros
  // that are replaced by string literals, which the compiler
  // will concatenate into a single string literal.
  setT( ST_START,      whitespace,     ST_WHITESPACE );
  setT( ST_WHITESPACE, whitespace,     ST_WHITESPACE );

  setT( ST_START,      "i",            ST_I          );
  setT( ST_I,          "n",            ST_IN         );
  setT( ST_I,          not_n,          ST_ID        );
  setT( ST_IN,         "t",            ST_INT         );
  setT( ST_IN,         not_t,          ST_ID      );
  setT( ST_INT,        "*",            ST_STAR       );
  setT( ST_INT,        letters digits, ST_ID      );
  
  setT( ST_START,      noi,        ST_ID         );
  setT( ST_ID,         letters digits, ST_ID         );
  setT( ST_START,      digits,         ST_NUM        );
  setT( ST_NUM,        digits,         ST_NUM        );
  setT( ST_START,      "(",            ST_LPAREN     );
  setT( ST_START,      ")",            ST_RPAREN     );
  setT( ST_START,      ",",            ST_COMMA      );
  setT( ST_START,      "{",            ST_LBRACE     );
  setT( ST_START,      "}",            ST_RBRACE     );
  setT( ST_START,      "=",            ST_BECOMES    );
  setT( ST_BECOMES,    "=",            ST_EQ         );
  setT( ST_START,      "!",            ST_NOT        );
  setT( ST_NOT,        "=",            ST_NE         );
  setT( ST_START,      "<",            ST_LT         );
  setT( ST_START,      ">",            ST_GT         );
  setT( ST_LT,         "=",            ST_LE         );
  setT( ST_GT,         "=",            ST_GE         );
  setT( ST_START,      "+",            ST_PLUS       );
  setT( ST_START,      "-",            ST_MINUS      );
  setT( ST_START,      "*",            ST_STAR       );
  setT( ST_START,      "/",            ST_SLASH      );
  setT( ST_SLASH,      "/",            ST_COMMENT    );
  setT(ST_COMMENT, letters digits everything whitespace, ST_COMMENT);
  setT( ST_START,      "%",            ST_PCT        );
  setT( ST_START,      ";",            ST_SEMI       );
  setT( ST_START,      "[",            ST_LBRACK     );
  setT( ST_START,      "]",            ST_RBRACK     );
  setT( ST_START,      "&",            ST_AMP        );

  for ( j=0; j<256; j++ ){ delta[ST_COMMENT][j] = ST_COMMENT;}
}

static int initT_done = 0;
bool flag = false;
//vector<string> tokens; 
 
vector<Token> scan(string input){ 
  // Initialize the transition table when called for the first time.
  if(!initT_done) {
    initT();
    initT_done = 1;
  }
  vector<Token> ret;

  int i = 0;
  int startIndex = 0;
  State state = ST_START;

  if(input.length() > 0) {
    while(true) {
      State nextState = ST_NUL;
      if(i < input.length())
	nextState = delta[state][(unsigned char) input[i]];
      if(nextState == ST_NUL) {
	// no more transitions possible
	if(stateKinds[state] == NUL) {
	  cerr << "ERROR" << endl;
	  flag = true; return ret;
	  //throw("ERROR in lexing after reading " + input.substr(0, i));
	}
	if(stateKinds[state] != WHITESPACE) {
	  Token token;
	  token.kind = stateKinds[state];
	  token.lexeme = input.substr(startIndex, i-startIndex);
	  if(token.kind == NUM){
	    if((token.lexeme.size() > 1) && (token.lexeme[0] == '0')){
	      cerr << "ERROR" << endl;
	      flag = true; return ret;
	    }
	  }
	  if(token.kind == ID){
	    if(ret.size() > 0 && ret[ret.size() - 1].kind == NUM){
	      cerr << "ERROR" << endl;
	      flag = true; return ret;
	    }
	  }
	  if(token.kind == STAR){
	    if(token.lexeme == "int*"){
	      Token k;
	      k.kind = INT;
	      k.lexeme = token.lexeme.substr(0, 3);
	      ret.push_back(k);
	      token.lexeme = token.lexeme.substr(3);
	    }
	  }
	  if(token.lexeme == "NULL"){
	    token.kind = NUL;
	  }
	  if(token.lexeme == "wain"){
	    token.kind = WAIN;
	  }
	  if(token.lexeme == "if"){
            token.kind = IF;
          }
          if(token.lexeme == "else"){
            token.kind = ELSE;
          }
	  if(token.lexeme == "while"){
            token.kind = WHILE;
          }
          if(token.lexeme == "println"){
            token.kind = PRINTLN;
          }
	  if(token.lexeme == "return"){
            token.kind = RETURN;
          }
	  if(token.lexeme == "new"){
            token.kind = NEW;
          }
          if(token.lexeme == "delete"){
            token.kind = DELETE;
          }
	  ret.push_back(token);
	}
	startIndex = i;
	state = ST_START;
	if(i >= input.length()) break;
      } else {
	state = nextState;
	i++;
      }
    }
  }

  return ret;
}

int main(){
  vector<Token> srcLine;
  vector< vector<Token> > token_lines;
  // Read the entire input file, storing each line as a
  // single string in the array srcLines.
  while(true) {
    string line;
    getline(cin, line);
    if(cin.fail()) break;
    srcLine = scan(line);
    if(flag) return 0;
    token_lines.push_back(srcLine);
  }
  for(int i = 0; i < token_lines.size(); i++){
    for(int j = 0; j < token_lines[i].size(); j++){
      string token_kind = kind_strings[token_lines[i][j].kind];
      cout << token_kind << " " << token_lines[i][j].lexeme << endl;
    }
  }
  return 0;
}
