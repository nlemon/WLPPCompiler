#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <map>
#include <list>
#include <sstream>
#include <iostream>

std::string rules[] = {
  "S BOF procedure EOF",
  "procedure INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE", 
  "type INT",
  "type INT STAR",
  "dcls",
  "dcls dcls dcl BECOMES NUM SEMI",
  "dcls dcls dcl BECOMES NULL SEMI",
  "dcl type ID",
  "statements",
  "statements statements statement",  
  "statement lvalue BECOMES expr SEMI",
  "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE", 
  "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE", 
  "statement PRINTLN LPAREN expr RPAREN SEMI",
  "statement DELETE LBRACK RBRACK expr SEMI",
  "test expr EQ expr",  
  "test expr NE expr",  
  "test expr LT expr", 
  "test expr LE expr",  
  "test expr GE expr",  
  "test expr GT expr", 
  "expr term", 
  "expr expr PLUS term", 
  "expr expr MINUS term", 
  "term factor", 
  "term term STAR factor", 
  "term term SLASH factor", 
  "term term PCT factor", 
  "factor ID",  
  "factor NUM",  
  "factor NULL",  
  "factor LPAREN expr RPAREN",  
  "factor AMP lvalue",
  "factor STAR factor",
  "factor NEW INT LBRACK expr RBRACK",
  "lvalue ID",  
  "lvalue STAR factor",
  "lvalue LPAREN lvalue RPAREN"
};

std::string transitions = 
"6 NE reduce 33 90 NE reduce 32 46 NUM shift 1 54 NUM shift 1 32 NUM shift 1 36 NE reduce 31 80 NE reduce 34 80 MINUS reduce 34 30 STAR shift 2 103 RETURN reduce 8 59 EQ reduce 36 12 NE reduce 28 1 NE reduce 29 11 NE reduce 30 53 EQ reduce 35 47 NEW shift 3 48 NEW shift 3 12 MINUS reduce 28 1 MINUS reduce 29 11 MINUS reduce 30 6 MINUS reduce 33 90 MINUS reduce 32 45 EQ reduce 37 36 MINUS reduce 31 101 RETURN reduce 8 100 RETURN reduce 8 34 SEMI shift 4 51 SEMI shift 5 4 WHILE reduce 6 5 WHILE reduce 5 85 STAR reduce 4 67 PRINTLN reduce 8 14 factor shift 6 6 RBRACK reduce 33 90 RBRACK reduce 32 23 GT shift 7 71 RBRACE shift 8 72 RBRACE shift 9 66 RETURN reduce 9 36 RBRACK reduce 31 80 RBRACK reduce 34 73 RBRACE shift 10 12 RBRACK reduce 28 1 RBRACK reduce 29 11 RBRACK reduce 30 4 INT reduce 6 5 INT reduce 5 83 NULL shift 11 13 NULL shift 11 7 ID shift 12 16 ID shift 12 24 ID shift 12 21 ID shift 12 37 ID shift 12 17 ID shift 12 81 BECOMES reduce 7 83 LPAREN shift 13 46 STAR shift 14 54 STAR shift 14 32 STAR shift 14 83 factor shift 15 45 GT reduce 37 13 factor shift 15 59 GT reduce 36 13 LPAREN shift 13 53 GT reduce 35 23 GE shift 16 53 GE reduce 35 59 GE reduce 36 85 PRINTLN reduce 4 45 GE reduce 37 23 EQ shift 17 68 BECOMES shift 18 19 RBRACK reduce 21 78 RBRACK reduce 22 79 RBRACK reduce 23 49 NUM shift 1 18 NUM shift 1 2 LPAREN shift 13 53 PCT reduce 35 27 NUM shift 1 2 STAR shift 14 84 term shift 19 59 PCT reduce 36 45 PCT reduce 37 56 RPAREN shift 20 23 LT shift 21 95 PLUS shift 22 94 PLUS shift 22 93 PLUS shift 22 92 PLUS shift 22 91 PLUS shift 22 96 PLUS shift 22 83 NUM shift 1 47 expr shift 23 48 expr shift 23 59 BECOMES reduce 36 53 BECOMES reduce 35 45 BECOMES reduce 37 84 ID shift 12 47 NULL shift 11 48 NULL shift 11 14 LPAREN shift 13 80 SEMI reduce 34 23 LE shift 24 89 MINUS shift 25 36 SEMI reduce 31 88 MINUS shift 25 13 STAR shift 14 84 STAR shift 14 13 NUM shift 1 83 STAR shift 14 27 term shift 19 49 term shift 19 18 term shift 19 25 AMP shift 26 22 AMP shift 26 67 RETURN reduce 8 10 DELETE reduce 11 105 RBRACE reduce 10 7 STAR shift 14 16 STAR shift 14 24 STAR shift 14 21 STAR shift 14 37 STAR shift 14 17 STAR shift 14 74 RBRACE reduce 13 106 RBRACE reduce 14 66 WHILE reduce 9 9 DELETE reduce 12 9 RBRACE reduce 12 104 RBRACK shift 27 6 SEMI reduce 33 6 STAR reduce 33 90 SEMI reduce 32 90 STAR reduce 32 10 RBRACE reduce 11 12 SEMI reduce 28 12 STAR reduce 28 1 SEMI reduce 29 1 STAR reduce 29 11 SEMI reduce 30 11 STAR reduce 30 53 LE reduce 35 36 STAR reduce 31 80 STAR reduce 34 64 SEMI shift 28 74 PRINTLN reduce 13 106 PRINTLN reduce 14 105 PRINTLN reduce 10 40 PLUS reduce 25 41 PLUS reduce 26 42 PLUS reduce 27 9 PRINTLN reduce 12 2 NUM shift 1 15 PLUS reduce 24 10 PRINTLN reduce 11 27 LPAREN shift 13 88 PLUS shift 22 7 NULL shift 11 16 NULL shift 11 24 NULL shift 11 21 NULL shift 11 37 NULL shift 11 17 NULL shift 11 89 PLUS shift 22 7 NUM shift 1 16 NUM shift 1 24 NUM shift 1 21 NUM shift 1 37 NUM shift 1 17 NUM shift 1 49 LPAREN shift 13 18 LPAREN shift 13 101 STAR reduce 8 100 STAR reduce 8 67 STAR reduce 8 15 SEMI reduce 24 103 STAR reduce 8 39 STAR shift 29 40 SEMI reduce 25 41 SEMI reduce 26 42 SEMI reduce 27 80 RPAREN reduce 34 67 WHILE reduce 8 105 DELETE reduce 10 74 DELETE reduce 13 106 DELETE reduce 14 12 RPAREN reduce 28 1 RPAREN reduce 29 11 RPAREN reduce 30 6 RPAREN reduce 33 90 RPAREN reduce 32 47 STAR shift 14 48 STAR shift 14 36 RPAREN reduce 31 19 LT reduce 21 45 PLUS reduce 37 97 LPAREN shift 30 78 LT reduce 22 79 LT reduce 23 53 PLUS reduce 35 59 PLUS reduce 36 15 NE reduce 24 64 PLUS shift 22 40 NE reduce 25 41 NE reduce 26 42 NE reduce 27 28 RBRACE shift 31 39 ID reduce 2 14 NEW shift 3 30 LPAREN shift 30 80 EQ reduce 34 12 GT reduce 28 1 GT reduce 29 11 GT reduce 30 36 GT reduce 31 6 GT reduce 33 90 GT reduce 32 78 RPAREN reduce 22 79 RPAREN reduce 23 19 RPAREN reduce 21 36 GE reduce 31 80 GE reduce 34 67 ID reduce 8 10 WHILE reduce 11 67 IF reduce 8 66 RBRACE reduce 9 12 GE reduce 28 1 GE reduce 29 11 GE reduce 30 6 GE reduce 33 90 GE reduce 32 78 NE reduce 22 79 NE reduce 23 19 NE reduce 21 4 STAR reduce 6 5 STAR reduce 5 9 WHILE reduce 12 25 NEW shift 3 22 NEW shift 3 74 WHILE reduce 13 106 WHILE reduce 14 105 WHILE reduce 10 101 IF reduce 8 100 IF reduce 8 103 ID reduce 8 73 STAR shift 2 103 IF reduce 8 79 PCT shift 32 78 PCT shift 32 53 RBRACK reduce 35 59 RBRACK reduce 36 45 RBRACK reduce 37 101 ID reduce 8 100 ID reduce 8 3 INT shift 33 71 STAR shift 2 72 STAR shift 2 85 LPAREN reduce 4 46 ID shift 12 54 ID shift 12 32 ID shift 12 66 LPAREN reduce 9 80 GT reduce 34 85 IF reduce 4 43 NULL shift 34 85 ID reduce 4 8 ELSE shift 35 66 DELETE reduce 9 89 RPAREN shift 36 23 NE shift 37 97 WHILE shift 38 19 LE reduce 21 14 AMP shift 26 98 INT shift 39 70 INT shift 39 71 LPAREN shift 30 72 LPAREN shift 30 46 factor shift 40 54 factor shift 41 32 factor shift 42 81 RPAREN reduce 7 13 term shift 19 78 LE reduce 22 79 LE reduce 23 83 term shift 19 73 LPAREN shift 30 58 BECOMES shift 43 80 PCT reduce 34 12 PCT reduce 28 1 PCT reduce 29 11 PCT reduce 30 6 PCT reduce 33 90 PCT reduce 32 36 PCT reduce 31 40 MINUS reduce 25 41 MINUS reduce 26 42 MINUS reduce 27 15 MINUS reduce 24 99 INT shift 44 2 AMP shift 26 15 STAR reduce 24 45 RPAREN reduce 37 7 factor shift 15 16 factor shift 15 24 factor shift 15 21 factor shift 15 37 factor shift 15 17 factor shift 15 9 LPAREN reduce 12 105 LPAREN reduce 10 74 LPAREN reduce 13 106 LPAREN reduce 14 53 RPAREN reduce 35 69 RPAREN shift 45 59 RPAREN reduce 36 84 NUM shift 1 10 LPAREN reduce 11 19 STAR shift 46 79 STAR shift 46 78 STAR shift 46 47 LPAREN shift 13 48 LPAREN shift 13 52 LPAREN shift 47 38 LPAREN shift 48 82 LPAREN shift 49 19 MINUS reduce 21 99 procedure shift 50 78 MINUS reduce 22 79 MINUS reduce 23 36 LT reduce 31 12 LT reduce 28 1 LT reduce 29 11 LT reduce 30 67 INT shift 39 6 LT reduce 33 90 LT reduce 32 80 LT reduce 34 78 GE reduce 22 79 GE reduce 23 43 NUM shift 51 97 IF shift 52 19 GE reduce 21 15 RBRACK reduce 24 36 LE reduce 31 6 LE reduce 33 90 LE reduce 32 23 PLUS shift 22 40 RBRACK reduce 25 41 RBRACK reduce 26 42 RBRACK reduce 27 12 LE reduce 28 1 LE reduce 29 11 LE reduce 30 80 LE reduce 34 97 ID shift 53 47 ID shift 12 48 ID shift 12 78 GT reduce 22 79 GT reduce 23 19 SLASH shift 54 19 GT reduce 21 79 SLASH shift 54 78 SLASH shift 54 84 NULL shift 11 13 AMP shift 26 31 EOF reduce 1 83 AMP shift 26 40 STAR reduce 25 41 STAR reduce 26 42 STAR reduce 27 67 RBRACE reduce 8 15 EQ reduce 24 40 EQ reduce 25 41 EQ reduce 26 42 EQ reduce 27 27 expr shift 55 49 expr shift 56 18 expr shift 57 103 RBRACE reduce 8 67 dcl shift 58 13 ID shift 12 46 AMP shift 26 54 AMP shift 26 32 AMP shift 26 83 ID shift 12 2 factor shift 59 4 PRINTLN reduce 6 5 PRINTLN reduce 5 98 dcl shift 60 97 STAR shift 2 71 DELETE shift 61 72 DELETE shift 61 80 SLASH reduce 34 70 dcl shift 62 27 ID shift 12 49 ID shift 12 18 ID shift 12 67 type shift 63 36 PLUS reduce 31 6 SLASH reduce 33 90 SLASH reduce 32 36 SLASH reduce 31 80 PLUS reduce 34 67 LPAREN reduce 8 12 SLASH reduce 28 1 SLASH reduce 29 11 SLASH reduce 30 12 PLUS reduce 28 1 PLUS reduce 29 11 PLUS reduce 30 6 PLUS reduce 33 90 PLUS reduce 32 15 GT reduce 24 40 GT reduce 25 41 GT reduce 26 42 GT reduce 27 26 LPAREN shift 30 70 type shift 63 98 type shift 63 23 MINUS shift 25 15 GE reduce 24 84 expr shift 64 40 GE reduce 25 41 GE reduce 26 42 GE reduce 27 45 STAR reduce 37 59 STAR reduce 36 53 STAR reduce 35 73 DELETE shift 61 103 WHILE reduce 8 85 DELETE reduce 4 60 RPAREN shift 65 101 RBRACE reduce 8 100 RBRACE reduce 8 71 statement shift 66 72 statement shift 66 49 factor shift 15 18 factor shift 15 7 AMP shift 26 16 AMP shift 26 24 AMP shift 26 21 AMP shift 26 37 AMP shift 26 17 AMP shift 26 27 factor shift 15 73 statement shift 66 101 WHILE reduce 8 100 WHILE reduce 8 85 dcls shift 67 97 lvalue shift 68 85 WHILE reduce 4 84 NEW shift 3 85 RETURN reduce 4 66 ID reduce 9 66 IF reduce 9 2 NEW shift 3 30 lvalue shift 69 75 LPAREN shift 70 47 factor shift 15 48 factor shift 15 13 NEW shift 3 101 statements shift 71 100 statements shift 72 83 NEW shift 3 103 statements shift 73 27 NEW shift 3 49 NEW shift 3 18 NEW shift 3 25 ID shift 12 22 ID shift 12 26 STAR shift 2 2 ID shift 12 66 STAR reduce 9 103 PRINTLN reduce 8 47 AMP shift 26 48 AMP shift 26 14 ID shift 12 101 PRINTLN reduce 8 100 PRINTLN reduce 8 19 PCT shift 32 4 LPAREN reduce 6 5 LPAREN reduce 5 71 lvalue shift 68 72 lvalue shift 68 73 lvalue shift 68 36 EQ reduce 31 20 SEMI shift 74 12 EQ reduce 28 1 EQ reduce 29 11 EQ reduce 30 49 NULL shift 11 18 NULL shift 11 6 EQ reduce 33 90 EQ reduce 32 27 NULL shift 11 40 LT reduce 25 41 LT reduce 26 42 LT reduce 27 15 LT reduce 24 81 COMMA reduce 7 10 RETURN reduce 11 15 LE reduce 24 97 statement shift 66 44 WAIN shift 75 59 MINUS reduce 36 53 MINUS reduce 35 15 RPAREN reduce 24 45 MINUS reduce 37 40 LE reduce 25 41 LE reduce 26 42 LE reduce 27 105 RETURN reduce 10 40 RPAREN reduce 25 41 RPAREN reduce 26 42 RPAREN reduce 27 7 term shift 19 16 term shift 19 24 term shift 19 21 term shift 19 37 term shift 19 17 term shift 19 74 RETURN reduce 13 106 RETURN reduce 14 7 NEW shift 3 16 NEW shift 3 24 NEW shift 3 21 NEW shift 3 37 NEW shift 3 17 NEW shift 3 9 RETURN reduce 12 66 PRINTLN reduce 9 53 LT reduce 35 59 LT reduce 36 15 PCT reduce 24 47 test shift 76 48 test shift 77 59 LE reduce 36 45 LE reduce 37 9 STAR reduce 12 22 term shift 78 25 term shift 79 40 PCT reduce 25 41 PCT reduce 26 42 PCT reduce 27 10 STAR reduce 11 101 LPAREN reduce 8 100 LPAREN reduce 8 88 RBRACK shift 80 74 STAR reduce 13 106 STAR reduce 14 103 LPAREN reduce 8 45 LT reduce 37 105 STAR reduce 10 97 DELETE shift 61 103 DELETE reduce 8 63 ID shift 81 64 MINUS shift 25 101 DELETE reduce 8 100 DELETE reduce 8 47 term shift 19 48 term shift 19 97 PRINTLN shift 82 71 ID shift 53 72 ID shift 53 45 NE reduce 37 25 NUM shift 1 22 NUM shift 1 71 IF shift 52 72 IF shift 52 59 NE reduce 36 53 NE reduce 35 73 ID shift 53 73 IF shift 52 30 ID shift 53 7 LPAREN shift 13 16 LPAREN shift 13 24 LPAREN shift 13 21 LPAREN shift 13 37 LPAREN shift 13 17 LPAREN shift 13 25 STAR shift 14 22 STAR shift 14 57 PLUS shift 22 56 PLUS shift 22 55 PLUS shift 22 40 SLASH reduce 25 41 SLASH reduce 26 42 SLASH reduce 27 15 SLASH reduce 24 33 LBRACK shift 83 78 PLUS reduce 22 79 PLUS reduce 23 97 RETURN shift 84 55 MINUS shift 25 57 MINUS shift 25 56 MINUS shift 25 19 PLUS reduce 21 67 DELETE reduce 8 46 LPAREN shift 13 54 LPAREN shift 13 32 LPAREN shift 13 45 SEMI reduce 37 59 SEMI reduce 36 53 SEMI reduce 35 49 AMP shift 26 18 AMP shift 26 27 AMP shift 26 95 MINUS shift 25 94 MINUS shift 25 93 MINUS shift 25 92 MINUS shift 25 91 MINUS shift 25 96 MINUS shift 25 65 LBRACE shift 85 71 WHILE shift 38 72 WHILE shift 38 4 DELETE reduce 6 5 DELETE reduce 5 73 WHILE shift 38 22 factor shift 15 25 factor shift 15 47 NUM shift 1 48 NUM shift 1 76 RPAREN shift 86 77 RPAREN shift 87 2 NULL shift 11 26 ID shift 53 46 NEW shift 3 54 NEW shift 3 32 NEW shift 3 19 SEMI reduce 21 83 expr shift 88 13 expr shift 89 4 IF reduce 6 5 IF reduce 5 78 SEMI reduce 22 79 SEMI reduce 23 14 STAR shift 14 26 lvalue shift 90 4 ID reduce 6 5 ID reduce 5 36 BECOMES reduce 31 25 LPAREN shift 13 49 STAR shift 14 18 STAR shift 14 22 LPAREN shift 13 6 BECOMES reduce 33 90 BECOMES reduce 32 12 BECOMES reduce 28 1 BECOMES reduce 29 11 BECOMES reduce 30 27 STAR shift 14 29 ID reduce 3 10 IF reduce 11 10 ID reduce 11 9 IF reduce 12 105 IF reduce 10 74 IF reduce 13 106 IF reduce 14 9 ID reduce 12 14 NULL shift 11 80 BECOMES reduce 34 95 RPAREN reduce 16 94 RPAREN reduce 17 93 RPAREN reduce 18 92 RPAREN reduce 19 91 RPAREN reduce 20 96 RPAREN reduce 15 46 NULL shift 11 54 NULL shift 11 32 NULL shift 11 14 NUM shift 1 7 expr shift 91 16 expr shift 92 24 expr shift 93 21 expr shift 94 37 expr shift 95 17 expr shift 96 4 RETURN reduce 6 5 RETURN reduce 5 67 statements shift 97 74 ID reduce 13 106 ID reduce 14 105 ID reduce 10 62 COMMA shift 98 0 BOF shift 99 19 EQ reduce 21 78 EQ reduce 22 79 EQ reduce 23 45 SLASH reduce 37 87 LBRACE shift 100 86 LBRACE shift 101 50 EOF shift 102 59 SLASH reduce 36 84 LPAREN shift 13 35 LBRACE shift 103 53 SLASH reduce 35 61 LBRACK shift 104 73 PRINTLN shift 82 57 SEMI shift 105 85 INT reduce 4 55 SEMI shift 106 84 AMP shift 26 71 PRINTLN shift 82 72 PRINTLN shift 82 25 NULL shift 11 84 factor shift 15 22 NULL shift 11";

struct Tree {
  std::string rule;
  //bool step;
  std::list<Tree*> children;

  ~Tree() {
    for(std::list<Tree*>::iterator it=children.begin(); it != children.end(); it++) {  // delete all subtrees
      delete (*it);
    }
  }
};

void print_derivation(Tree* t){
  if(!t->children.size()){
    std::cout << t->rule << std::endl; return;
  }
  std::cout << t->rule << std::endl;
  std::list<Tree*> child = t->children;
  std::list<Tree*>::iterator p = child.begin();
  for(p; p != child.end(); p++){
    print_derivation(*p);
  }
}

std::string make_input(){
  std::string input = "";
  std::string kind;
  std::string lexeme;
  std::string space(" ");
  while(std::cin >> kind){
    std::cin >> lexeme;
    kind.append(space);
    kind.append(lexeme);
    input.append(kind);
    input.append(space);
  }
  return input;
}

int main(int argc, char** argv) {
  int states = 107;
  int number_of_transitions = 710;
  std::istringstream ss(transitions);
 
  //read and sort transitions
  std::map<std::string, std::vector<int> > reduction_table;
  std::map<std::string, std::vector<int> > shift_table;
  int current_state;
  std::string current_element;
  std::string operation;
  int result;
  for(int i = 0; i < number_of_transitions; i++){
    ss >> current_state >> current_element >> operation >> result;
    if(operation == "reduce"){
      if(!reduction_table.count(current_element)){
	std::vector<int> v(states, -1);
        reduction_table[current_element] = v; 
      }
      reduction_table[current_element][current_state] = result;
      continue;
    }
    if(!shift_table.count(current_element)){
      std::vector<int> v(states, -1);
      shift_table[current_element] = v;
    }
    shift_table[current_element][current_state] = result;
  }
  //execute bottom up parse
  std::list<Tree*> node_stack;
  std::list<int> state_stack;
  std::string BOF("BOF");
  Tree* begin = new Tree;
  begin->rule = "BOF BOF";
  node_stack.push_front(begin);
  int initial_transition = shift_table[BOF][0];
  state_stack.push_front(initial_transition);

  std::string element;
  std::string value;
  int current_token = 0;
  std::string input = make_input();
  input = input.append(" EOF EOF");
  std::istringstream is(input);
  while(is >> element){
    is >> value;
    current_token++;
    if(reduction_table.count(element) > 0){
      while(reduction_table[element][state_stack.front()] > 0){
        int rule_number = reduction_table[element][state_stack.front()];
        std::string rule = rules[rule_number];
        std::istringstream s(rule);
        std::string lhs;
        s >> lhs;
	std::string rhs;
	if(rule.size() == lhs.size()){
	  rhs = "";
	}
	if(rule.size() > lhs.size()){
          rhs = rule.substr(lhs.size() + 1);
	}
	std::istringstream ss(rhs);
	std::string dummy;
	int rhs_size = 0;
	while(ss >> dummy){
	  rhs_size++;
	}
	Tree* new_root = new Tree;
        new_root->rule = rule;
	std::list<Tree*> v;
	new_root->children = v;
        for(int i = 0; i < rhs_size; i++){
	  new_root->children.push_front(node_stack.front());
          node_stack.pop_front();
          state_stack.pop_front();
        }
	node_stack.push_front(new_root);
        int current_state = state_stack.front();
        state_stack.push_front(shift_table[lhs][current_state]);
      }
    }
    Tree* new_leaf = new Tree;
    std::string kind_lex(element);
    kind_lex.append(" ");
    kind_lex = kind_lex.append(value);
    new_leaf->rule = kind_lex;
    node_stack.push_front(new_leaf);
    int next_state = shift_table[element][state_stack.front()];
    if(next_state > 0){
      state_stack.push_front(next_state); continue;
    }
    std::cerr << "ERROR at " << current_token << std::endl; return 0;
  }
  node_stack.pop_front();
  Tree* root = node_stack.front();
  std::string first_rule = root->rule;
  std::istringstream ss1(first_rule);
  std::string first_step;
  ss1 >> first_step;
  std::cout << "S BOF " << first_step << " EOF" << std::endl;
  std::cout << "BOF BOF" << std::endl;
  print_derivation(root);
  std::cout << "EOF EOF" << std::endl;
  return 0;
}
