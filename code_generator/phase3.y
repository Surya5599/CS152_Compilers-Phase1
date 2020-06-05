%{
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <string>
#include <map>
#include <vector>
#include <list>
#include <iterator>
#include <iostream>
#include <stdbool.h>

using namespace std;

extern FILE * yyin;
extern int currLine;
void yyerror(const char *msg);
char* returnVal(string);
int yylex();
bool no_error = true;
void addMap(string,int);
bool checkMap(string,int);
void printMap();
string makeTemps();
int num_temps = 0;
string makeLabel();
int num_label = 0;

std::map<string, int> symbol_table; // 0 is integer, 1 as array //2 = function name

%}



%union{
	char* cVal;
	int iVal;
	char* str;
	struct {
		char* code;
		int numVar;
	}ids;
	struct {
		char* code;
		char* variables;
		int numVar;
	}forTemp;

}

//declaration

%error-verbose
%token FUNCTION
%token BEGIN_PARAMS END_PARAMS
%token BEGIN_LOCALS END_LOCALS
%token BEGIN_BODY END_BODY
%token INTEGER ARRAY OF
%token IF THEN ENDIF ELSE WHILE DO FOR
%token BEGINLOOP ENDLOOP CONTINUE READ WRITE
%token TRUE FALSE RETURN
%token COMMA COLON SEMICOLON

%right ASSIGN
%left OR
%left AND
%right NOT
%left GTE LTE GT LT NEQ EQ
%left SUB ADD
%left MULT DIV MOD
%left R_SQUARE_BRACKET L_SQUARE_BRACKET
%left R_PAREN L_PAREN

%token <cVal> IDENT
%token <iVal> NUMBER

%start prog_start
%type <str> prog_start function functions declarations  var vars
%type <str> declaration statements  ident statement comp
%type <ids> identifier
%type <forTemp> term expressions expression multi_expr relation_expr relation_and_exp bool_exp

%%
prog_start: functions
       	   {

						 printf("%s",$1);}
	;

functions: /*epsilon*/
       	   {string temp = "";
	 					$$ = returnVal(temp);}
	 |function functions
       	   {
						 string temp = $1;
						 temp += "\n";
						 temp += $2;
						 $$ = returnVal(temp);
					 }
	;
function: FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
       	  { string temp = "func ";
					temp += $2;
					addMap(string($2), 2);
					temp += "\n";
					temp += $5;
					temp += $8;
					temp += $11;
					temp += "endfunc";
 				 	$$ = returnVal(temp);

					//printf(c);
					}
	;

ident:  IDENT
		{$$ = strdup($1);
		}

  ;

declarations: /*epsilon*/
				{string temp = "";
					$$ = returnVal(temp);
				}
	      | declaration SEMICOLON declarations
	     	{ string temp = $1;
					temp += $3;
					$$ = returnVal(temp);
				}
	      | error {}
	;

declaration: identifier COLON INTEGER  //i,j,d,c : integer
	     {
				char* str = $1.code;
				char *token = strtok(str, " ");
				string temp;
			    while (token != NULL)
			    {
							temp += ". ";
							temp += token;
							addMap(token,0);
							temp += "\n";
				      token = strtok(NULL, " ");
			    }
					$$ = returnVal(temp);
		 		}
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {
				 char* str = $1.code;
			 	char *token = strtok(str, " ");
			 	string temp;
				 while (token != NULL)
				 {
						 temp += ". [] ";
						 temp += token;
						 addMap(token,0);
						 temp += ", ";
						 temp += to_string($5);
						 temp += "\n";
						 token = strtok(NULL, " ");
				 }
				$$ = returnVal(temp);
		 }
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {
				char* str = $1.code;
			 char *token = strtok(str, " ");
			 string temp;
				while (token != NULL)
				{
						temp += ". [] ";
						temp += token;
						addMap(token,0);
						temp += ", ";
						temp += to_string($5);
						temp += ", ";
						temp += to_string($8);
						temp += "\n";
						token = strtok(NULL, " ");
				}
			 $$ = returnVal(temp);
				}
	;




	identifier: ident COMMA identifier
				{string temp = $1;
				$$.numVar = 1;
				temp += " ";
				temp +=  $3.code;
				$$.numVar += $3.numVar;
				$$.code = returnVal(temp);
				}
		    | ident
		    {$$.code = strdup($1);
					$$.numVar = 1;
				}
		;

statements: /*epsilon*/
		  {string temp = "";
				$$ = returnVal(temp);
			}
	   |statement SEMICOLON statements
	    { string temp = $1;
			temp += "\n";
			temp +=  $3;
			$$ = returnVal(temp);
			}
	   | error {}
	  ;

statement: var ASSIGN expression
	   {string temp =  $3.code;
		 temp += "= ";
		 temp+= $1;
		 temp += ", ";
		 temp += $3.variables;
		 $$ = returnVal(temp);
	 }
		 | IF bool_exp THEN statements ENDIF
		 {}
	   | IF bool_exp THEN statements ELSE  statements ENDIF
	   {}
	   | WHILE bool_exp BEGINLOOP statements ENDLOOP
	   { string whileLoop = makeLabel();
			 string beginloop: makeLabel();
			 string endloop: makeLabel();

			 string temp = $2.code;
			 temp += ":= " + whileLoop;
			 $$ = returnVal(temp);
		 }
	   | DO BEGINLOOP statements ENDLOOP WHILE bool_exp
	   {}
	   | FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGINLOOP statements ENDLOOP
	   {}
	   | READ vars
		 {  //check if var is int or arr based on that print [] .
		 string temp = ".< ";
		 temp +=  $2;
		 $$ = returnVal(temp);
		 }
	   | WRITE vars
	   {
		 string temp = ".> ";
		 temp +=  $2;
		 $$ = returnVal(temp);}
	   | CONTINUE
	   {}
	   |
	   RETURN expression
	   {}
	;

vars: var COMMA vars
	{}
	|var
	{ //cout << $1 << endl;
		$$ = strdup($1);
	}
	;

var: ident
	{
		$$ = strdup($1);
	}
	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
	{ }
	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
	{}
;

expressions: expression COMMA expressions
	     {}
	     | expression
	     {$$.code = strdup($1.code);
 			 	$$.variables = ($1.variables);
			}
	;

expression: multi_expr
	    {$$.code = strdup($1.code);
		   $$.variables = ($1.variables);
				}
	    | multi_expr ADD  expression
      { string tempVar = makeTemps();
 			 string temp = ($1.code);
 			 temp += ($3.code);
 			 temp += "+ " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
 			 temp += "\n";
 			 $$.code = returnVal(temp);
 			 $$.variables = returnVal(tempVar);
		 }
	    | multi_expr SUB  expression
	    { string tempVar = makeTemps();
 			 string temp = ($1.code);
 			 temp += ($3.code);
 			 temp += "- " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
 			 temp += "\n";
 			 $$.code = returnVal(temp);
 			 $$.variables = returnVal(tempVar);
		 }
	 ;

multi_expr: term
	    {$$.code = strdup($1.code);
			 $$.variables = ($1.variables);
			}
	   | term MULT multi_expr
	    { string tempVar = makeTemps();
 			 string temp = ($1.code);
 			 temp += ($3.code);
 			 temp += "* " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
 			 temp += "\n";
 			 $$.code = returnVal(temp);
 			 $$.variables = returnVal(tempVar);
		 }
	   | term DIV multi_expr
	   { string tempVar = makeTemps();
			 string temp = ($1.code);
			 temp += ($3.code);
			 temp += "/ " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
			 temp += "\n";
			 $$.code = returnVal(temp);
			 $$.variables = returnVal(tempVar);
		 }
	   | term MOD multi_expr
	   { string tempVar = makeTemps();
			 string temp = ($1.code);
			 temp += ($3.code);
			 temp += "% " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
			 temp += "\n";
			 $$.code = returnVal(temp);
			 $$.variables = returnVal(tempVar);
		 }
	;

term: 	SUB var {}
	| SUB NUMBER
	{}
	| SUB L_PAREN expression R_PAREN
	{}
	| var
	{ string tempVar = makeTemps();
		string temp = ". ";
		temp += tempVar;
		temp += "\n";
	 	temp += "= " + tempVar + ", ";
		temp += $1;
		temp += "\n";
		$$.code = returnVal(temp);
	  $$.variables = returnVal(tempVar);
	}
	| NUMBER
	{ string tempVar = makeTemps();
		string temp = ". ";
		temp += tempVar;
		temp += "\n";
	 	temp += "= " + tempVar + ", ";
		temp += to_string($1);
		temp += "\n";
		$$.variables = returnVal(tempVar);
		$$.code = returnVal(temp);
}
	| L_PAREN expression R_PAREN
	{}
	| ident L_PAREN expressions R_PAREN
	{ string temp = $3.code;
		temp += "param ";
	  temp += $3.variables;
		temp += "\n";
		string tempVar = makeTemps();
		temp += ". " + tempVar + "\n";
		temp += "call ";
		temp += $1;
		temp += ", " + tempVar + "\n";
		$$.code = returnVal(temp);
		$$.variables = returnVal(tempVar);

	}
	;

bool_exp: relation_and_exp
	  {$$.code = strdup($1.code);
		 $$.variables = ($1.variables);}
	  | relation_and_exp OR bool_exp
	  {}
	;

relation_and_exp: relation_expr
		  {$$.code = strdup($1.code);
			 $$.variables = ($1.variables);}
		 | relation_expr AND relation_and_exp
		  {}
	;

relation_expr: NOT expression comp expression
		{}
		| NOT TRUE
		{}
		| NOT FALSE
		{}
		| NOT L_PAREN bool_exp R_PAREN
		{}
		| expression comp expression
		{ string tempVar = makeTemps();
			string temp = $1.code;
		 	temp += $3.code;
			temp += $2;
			temp += " " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
			$$.code = returnVal(temp);
			$$.variables = returnVal(tempVar);
		}
		| TRUE
		{}
		| FALSE
		{}
		| L_PAREN bool_exp R_PAREN
		{}
		;

comp: EQ
	{$$ = returnVal("==");}
	| NEQ
	{$$ = returnVal("!=");}
	| LT
	{$$ = returnVal("<");}
	| GT
	{$$ = returnVal(">");}
	| LTE
	{$$ = returnVal("<=");}
	| GTE
	{$$ = returnVal(">=");}
	;

%%

//user code

int main(int argc, char ** argv)
{
   if(argc >= 2)
   {
      yyin = fopen(argv[1], "r");
      if(yyin == NULL)
      {
         yyin = stdin;
      }
   }
   else
   {
      yyin = stdin;
   }

   yyparse();
   return 1;
}

bool checkMap(string name){
	if(symbol_table.find(name) != symbol_table.end()){
			return true;
	}
	else{
		return false;
	}
}

void addMap(string name, int num){
	if(symbol_table.find(name) != symbol_table.end()){
		cout << "Exists already" << endl;
	}
	else{
		symbol_table.insert(pair<string, int>(name, num));
	}

}

char * returnVal(string temp){
	char* c = const_cast<char*>(temp.c_str());
	return strdup(c);
}


void printMap(){
	for(auto elem : symbol_table)
	 {
			std::cout << elem.first << " " << elem.second << "\n";
	 }
 }

 string makeTemps(){
	 string ret = "__temp__" + to_string(num_temps);
	 num_temps++;
	 return ret;
 }

 string makeLabel(){
	string ret = "__label__" + to_string(num_temps);
	num_label++;
	return ret;
 }


void yyerror (const char * msg){
	printf("Error in line %d %s\n", currLine, msg);
}
