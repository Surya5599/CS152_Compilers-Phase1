%{
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <string>
#include <map>
#include <list>
#include <iostream>
#include <stdbool.h>

using namespace std;

extern FILE * yyin;
extern int currLine;
void yyerror(const char *msg);
char* returnVal(string);
int yylex();
bool no_error = true;
map<string, int> symbol_table; // 0 is integer, 1 as array //2 = function name

%}


%union{
	char* cVal;
	int iVal;
	char* str;
	struct{
		char* code;
	} ids;

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
%type <str> prog_start function functions declarations multi_expr term var vars
%type <str> declaration statements  ident statement expressions expression
%type <ids> identifier

%%
prog_start: functions
       	   {printf("%s",$1);}
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
		{$$ = strdup($1);}
  ;

declarations: /*epsilon*/
				{string temp = "";
					$$ = returnVal(temp);
				}
	      | declaration SEMICOLON declarations
	     	{ string temp = $1;
					temp += "\n";
					temp += $3;
					$$ = returnVal(temp);
				}
	      | error {}
	;

declaration: identifier COLON INTEGER
	     { string temp = ". ";
				 temp += $1.code;
				 $$ = returnVal(temp);
		 		}
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {string temp = ". [] " + string($1.code) + ", " + to_string($5);
				$$ = returnVal(temp);
		 }
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {string temp = ". [][] " + string($1.code) + ", " + to_string($5) + ", " + to_string($8);
			 	$$ = returnVal(temp);
				}
	;



identifier: ident COMMA identifier
			{string temp = $1;
			temp += "\n";
			temp += ". ";
			temp +=  $3.code;
			$$.code = returnVal(temp);
		//	$$.id.push_back(string($1))
		//	$$.id.push_back(string($3))
			}
	    | ident
	    {$$.code = strdup($1);}
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
	   {string temp = $1;
		 temp += "\n";
		 temp +=  $3;
		 $$ = returnVal(temp);
	 }
		 | IF bool_exp THEN statements ENDIF
		 {}
	   | IF bool_exp THEN statements ELSE  statements ENDIF
	   {}
	   | WHILE bool_exp BEGINLOOP statements ENDLOOP
	   {}
	   | DO BEGINLOOP statements ENDLOOP WHILE bool_exp
	   {}
	   | FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGINLOOP statements ENDLOOP
	   {}
	   | READ vars
	   {string temp = ".< ";
		 temp +=  $2;
		 $$ = returnVal(temp);
		}
	   | WRITE vars
	   {string temp = ".> ";
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
	{}
	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
	{}
;

expressions: expression COMMA expressions
	     {}
	     | expression
	     {$$ = strdup($1);}
	;

expression: multi_expr
	    {$$ = strdup($1);}
	    | multi_expr ADD  expression
      {}
	    | multi_expr SUB  expression
	    {}
	 ;

multi_expr: term
	    {$$ = strdup($1);}
	   | term MULT multi_expr
	    {}
	   | term DIV multi_expr
	   {}
	   | term MOD multi_expr
	   {}
	;

term: 	SUB var {}
	| SUB NUMBER
	{}
	| SUB L_PAREN expression R_PAREN
	{}
	| var
	{string temp = ". _temp_13\n";
	 	temp += "= _temp_13, ";
		temp += $1;
		temp += "\n";
		$$ = returnVal(temp);
	}
	| NUMBER
	{}
	| L_PAREN expression R_PAREN
	{}
	| ident L_PAREN expressions R_PAREN
	{ string temp = $3;
		temp += "param _temp13";
		temp += "\n";
		temp += ". _param _temp14\n";
		temp += "call ";
		temp += $1;
		temp += ", _param _temp14";
		$$ = returnVal(temp);

	}
	;

bool_exp: relation_and_exp
	  {}
	  | relation_and_exp OR bool_exp
	  {}
	;

relation_and_exp: relation_expr
		  {}
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
		{}
		| TRUE
		{}
		| FALSE
		{}
		| L_PAREN bool_exp R_PAREN
		{}
		;

comp: EQ
	{}
	| NEQ
	{}
	| LT
	{}
	| GT
	{}
	| LTE
	{}
	| GTE
	{}
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

void yyerror (const char * msg){
	printf("Error in line %d %s\n", currLine, msg);
}
