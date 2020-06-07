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
#include <algorithm>
#include <stdbool.h>
#include <sstream>

using namespace std;

extern FILE * yyin;
extern int currLine;
void yyerror(const char *msg);
char* returnVal(string);
int yylex();
bool no_error = true;
void addMap(string,int);
bool checkMap(string);
void printMap();
string makeTemps();
int num_temps = 0;
string makeLabel();
string calledFunctions = "";
int num_label = 0;
bool contFound = false;
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
		int varType;
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
%type <str> prog_start function functions declarations
%type <str> declaration statements  ident statement comp
%type <ids> identifier
%type <forTemp>  var vars term expressions expression multi_expr relation_expr relation_and_exp bool_exp

%%
prog_start: functions
       	   {
						char* str = returnVal(calledFunctions);
		 				char *token = strtok(str, " ");
		 			   while (token != NULL)
		 			    {
		 							if (checkMap(string(token)) == false){
										string temp =  " Called function \"" + string(token) + "\" doesnt exists \n";
						 				yyerror(returnVal(temp));
									}
		 				      token = strtok(NULL, " ");
		 			    }
						 if(checkMap("main") == false){
							string temp =  " No main function exists \n";
			 				yyerror(returnVal(temp));
						 }
						 if(no_error == true){
							printf("%s",$1);
						 }
						 //printf("%s",$1);
					 }
	;


functions: /*epsilon*/
       	   {string temp = "";
	 					$$ = returnVal(temp);}
	 |function functions
       	   {
						 string temp = $1;
						 temp += "\n\n";
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
					char* parameters = $5;
					int param = 0;
					char *token = strtok(parameters, "\n");
				  while (token != NULL)
				  {
							string s = token;
							replace( s.begin(), s.end(), '.', '=');
							temp += s;
							temp += ", $" + to_string(param);
							param++;
							temp += "\n";
				      token = strtok(NULL, "\n");
				    }
					temp += $8;
					temp += $11;
					string state = $11;
					if (state.find("continue") != string::npos) {
						string temp =  " Continue is used outside a loop \n";
						yyerror(returnVal(temp));
					}
					temp += "endfunc";
 				 	$$ = returnVal(temp);
				}
	;



ident:  IDENT
		{$$ = strdup($1);
		}

  ;

declarations: /*epsilon*/
				{
					string temp = "";
					$$ = returnVal(temp);
				}
	      | declaration SEMICOLON declarations
	     	{
					string temp = $1;
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
				 if($5 <= 0){
					string temp =  " Cannot declare array of size \"" + to_string($5) +  "\"\n";
	 				yyerror(returnVal(temp));
				 }
				 char* str = $1.code;
			 	char *token = strtok(str, " ");
			 	string temp;
				 while (token != NULL)
				 {
						 temp += ".[] ";
						 temp += token;
						 addMap(token,1);
						 temp += ", ";
						 temp += to_string($5);
						 temp += "\n";
						 token = strtok(NULL, " ");
				 }
				$$ = returnVal(temp);
		 }
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {if($5 <= 0 || $8 <= 0){
				string temp =  " Cannot declare array of size \"" + to_string($5) +  "\"\n";
				yyerror(returnVal(temp));
			 }
				char* str = $1.code;
			 char *token = strtok(str, " ");
			 string temp;
				while (token != NULL)
				{
						temp += ".[] ";
						temp += token;
						addMap(token,1);
						temp += ", ";
						int num1 = $5;
						int num2 = $8;
						int num = num1 * num2;
						temp += to_string(num);
						temp += "\n";
						token = strtok(NULL, " ");
				}
			 			$$ = returnVal(temp);
				}
	;




	identifier: ident COMMA identifier
				{
					string temp = $1;
					$$.numVar = 1;
					temp += " ";
					temp +=  $3.code;
					$$.numVar += $3.numVar;
					$$.code = returnVal(temp);
				}
		    | ident
		    {
					$$.code = strdup($1);
					$$.numVar = 1;
				}
		;

statements: /*epsilon*/
		  {string temp = "";
				$$ = returnVal(temp);
			}
	   |statement SEMICOLON statements
	    { string temp = $1;
				string state = $1;
			temp +=  $3;
			$$ = returnVal(temp);
			}
	  ;

statement: var ASSIGN expression
		   {
				 string temp = $1.code;
				 temp += $3.code;
				 if($1.varType == 1){
					 temp += "[]= ";
				 }
				 else if($1.varType == 0){
					 temp += "= ";
				 }
				 temp += $1.variables;
				 temp += ", ";
				 temp += $3.variables;
				 temp += "\n";
				 $$ = returnVal(temp);
		 	 }
		 | IF bool_exp THEN statements ENDIF
		 {
			 string boolWork = $2.code;
			 string boolVar = $2.variables;
			 string state = $4;
			 string temp = boolWork;
			 string labelIF = makeLabel();
			 temp += "?:= " + labelIF + ", " + boolVar + "\n";
			 string labelEndIf = makeLabel();
			 temp += ":= " + labelEndIf + "\n";
			 temp += ": " + labelIF + "\n";
			 temp += state;
			 temp += ": " + labelEndIf + "\n";
			 $$ = returnVal(temp);
		 }
	   | IF bool_exp THEN statements ELSE  statements ENDIF
	   {
			 string boolWork = $2.code;
			 string boolVar = $2.variables;
			 string state1 = $4;
			 string state2 = $6;
			 string temp = boolWork;
			 string labelIF = makeLabel();
			 string labelElse = makeLabel();
			 temp += "?:= " + labelIF + ", " + boolVar + "\n";
			 string labelEndIf = makeLabel();
			 temp += ":= " + labelElse + "\n";
			 temp += ": " + labelIF + "\n";
			 temp += state1;
			 temp += ":= " + labelEndIf + "\n";
			 temp += ": " + labelElse + "\n";
			 temp += state2;
			 temp += ": " + labelEndIf + "\n";
			 $$ = returnVal(temp);
		 }
	   | WHILE bool_exp BEGINLOOP statements ENDLOOP
	   { string boolWork = $2.code;
			 string boolVar = $2.variables;
			 string state = $4;
			 string labelWhile = makeLabel();
			 string endWhile = makeLabel();
			 string checkWhile = makeLabel();
			 string temp = ": " + checkWhile + "\n";
			 temp += boolWork;
			 temp += "?:= " + labelWhile + ", " + boolVar + "\n";
			 temp += ":= " + endWhile + "\n";
			 temp += ": " + labelWhile + "\n";
			 temp += state;
			 temp += ":= " + checkWhile + "\n";
			 temp += ": " + endWhile + "\n";
			 if(temp.find("continue") != string::npos){
				 temp.replace(temp.find("continue"), 8, checkWhile);
			 }
			 $$ = returnVal(temp);
		 }
	   | DO BEGINLOOP statements ENDLOOP WHILE bool_exp
	   { string boolWork = $6.code;
			 string boolVar = $6.variables;
			 string startLabel = makeLabel();
			 string temp = ": " + startLabel + "\n";
			 temp += $3;
			 string state = $3;
			 temp += boolWork;
			 temp += "?:= " + startLabel + ", " + boolVar + "\n";
			 if(temp.find("continue") != string::npos){
				 temp.replace(temp.find("continue"), 8, startLabel);
			 }
			 $$ = returnVal(temp);
		  }
	   | FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGINLOOP statements ENDLOOP
	   {	string varCode = makeTemps();
			 	string temp = ". " + varCode + "\n";
				temp += ":= " + varCode + ", " + to_string($4) + "\n";
				temp += ":= " +  string($2.variables) + ", " + varCode + "\n";
				string loopLabel = makeLabel();
				string stateLabel = makeLabel();
				temp += ": " + loopLabel + "\n";
				string boolWork = $6.code;
	 			string boolVar = $6.variables;
				temp += boolWork;
				temp += "?:= " + stateLabel + ", " + boolVar + "\n";
				string endLabel = makeLabel();
				temp += ":= " + endLabel + "\n";
				temp += ": " + stateLabel + "\n";
				temp += $12;
				string state = $12;
				temp += $10.code;
				temp += ":= " + string($8.variables) + ", " + string($10.variables) + "\n";
				temp += ":= " + loopLabel + "\n";
				temp += ": " + endLabel + "\n";
				if(temp.find("continue") != string::npos){
 				 temp.replace(temp.find("continue"), 8, loopLabel);
 			 	}
				$$ = returnVal(temp);
		 }
	   | READ vars
		 {
			 char* str = $2.variables;
			 char *token = strtok(str, "|");
			 string temp = $2.code;
			 while (token != NULL)
			 {
				 if($2.varType == 1){
					 temp += ".[]< ";
				 }
				 else{
					 temp += ".< ";
				 }
					 temp += token;
					 temp += "\n";
					 token = strtok(NULL, "|");
				 }
				 $$ = returnVal(temp);
	   }
	   | WRITE vars
	   {
			 char* str = $2.variables;
			 char *token = strtok(str, "|");
			 string temp = $2.code;
			 while (token != NULL)
			 {
				 if($2.varType == 1){
					 if(strstr (token, ",") ){
						 temp += ".[]> ";
					 }
					 else{
						 temp += ".> ";
					 }

				 }
				 else{
					 temp += ".> ";
				 }
					 temp += token;
					 temp += "\n";
					 token = strtok(NULL, "|");
				 }
				 $$ = returnVal(temp);
	   }
	   | CONTINUE
	   { string temp = ":= continue\n";
		 	$$ = returnVal(temp);
	  	}
	   |
	   RETURN expression
	   {
			 string tempVar = $2.variables;
			 string temp = $2.code;
			 temp += "ret " + tempVar + "\n";
			 $$ = returnVal(temp);
		 }
	;

vars: var COMMA vars
			{ string temp = $1.code;
				temp += $3.code;
				$$.code = returnVal(temp);
				temp = $1.variables;
				temp += "|";
				temp += $3.variables;
				$$.variables = returnVal(temp);
			}
			|var
			{
				$$.code = strdup($1.code);
				$$.variables = ($1.variables);
			}
	;

var: ident
		{
			if(checkMap(string($1)) == false){
				string temp =  " Used variable \"" + string($1) +  "\" was not previously declared.\n";
				yyerror(returnVal(temp));
			}
			else{
				int x = symbol_table.find(string($1))->second;
				if(x != 0){
					if(x == 1){
						string temp =  " Used array variable \"" + string($1) +  "\" is missing an index.\n";
						yyerror(returnVal(temp));
					}
					else if(x == 2){
						string temp =  " Variable name \"" + string($1) +  "\" defined as a function\n";
						yyerror(returnVal(temp));
					}
				}
			}
			string temp = $1;
			$$.variables = returnVal(temp);
			$$.code = returnVal("");
			$$.varType = 0;
		}
		| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
		{ if(checkMap(string($1)) == false){
			string temp =  " Used variable \"" + string($1) +  "\" was not previously declared.\n";
			yyerror(returnVal(temp));
		}
		else{
			int x = symbol_table.find(string($1))->second;
			if(x != 1){
				if(x == 0){
					string temp =  " Used variable \"" + string($1) +  "\" is an integer.\n";
					yyerror(returnVal(temp));
				}
				else if(x == 2){
					string temp =  " Variable name \"" + string($1) +  "\" defined as a function\n";
					yyerror(returnVal(temp));
				}
			}
		}

			string temp = $3.code;
			$$.code = returnVal(temp);
			temp = $1;
			temp += ", ";
			temp += $3.variables;
			$$.variables = returnVal(temp);
			$$.varType = 1;
		}
		| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
		{
			string temp = $3.code;
			temp += $6.code;
			string tempVar3 = makeTemps();
			temp += ". " + tempVar3 + "\n";
			temp += "* " + tempVar3 + ", ";
			temp += $3.variables;
			temp += ", ";
			temp += $6.variables;
			temp += "\n";
			$$.code = returnVal(temp);
			temp = $1;
			temp += ", ";
			temp += tempVar3;
			$$.variables = returnVal(temp);
			$$.varType = 1;
		 }
;

expressions: expression COMMA expressions
	     	{ string temp = $1.code;
					temp += $3.code;
					$$.code = returnVal(temp);
					temp = $1.variables;
					temp += " ";
					temp += $3.variables;
					$$.variables = returnVal(temp);

				}
	     | expression
		      {
					  $$.code = strdup($1.code);
		 			 	$$.variables = ($1.variables);
			  	}
	;

expression: multi_expr
		    {
				 $$.code = strdup($1.code);
			   $$.variables = ($1.variables);
				}
	    | multi_expr ADD  expression
	      {
				 string tempVar = makeTemps();
	 			 string temp = ($1.code);
	 			 temp += ($3.code);
				 temp += ". " + tempVar + "\n";
	 			 temp += "+ " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
	 			 temp += "\n";
	 			 $$.code = returnVal(temp);
	 			 $$.variables = returnVal(tempVar);
			 }
	    | multi_expr SUB  expression
		    {
				 string tempVar = makeTemps();
	 			 string temp = ($1.code);
	 			 temp += ($3.code);
				 temp += ". " + tempVar + "\n";
	 			 temp += "- " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
	 			 temp += "\n";
	 			 $$.code = returnVal(temp);
	 			 $$.variables = returnVal(tempVar);
			 }
	 ;

multi_expr: term
		    {
				 $$.code = strdup($1.code);
				 $$.variables = ($1.variables);
				}
	   | term MULT multi_expr
		    {
				 string tempVar = makeTemps();
	 			 string temp = ($1.code);
	 			 temp += ($3.code);
				 temp += ". " + tempVar + "\n";
	 			 temp += "* " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
	 			 temp += "\n";
	 			 $$.code = returnVal(temp);
	 			 $$.variables = returnVal(tempVar);
			 }
	   | term DIV multi_expr
		   {
				 string tempVar = makeTemps();
				 string temp = ($1.code);
				 temp += ($3.code);
				 temp += ". " + tempVar + "\n";
				 temp += "/ " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
				 temp += "\n";
				 $$.code = returnVal(temp);
				 $$.variables = returnVal(tempVar);
			 }
	   | term MOD multi_expr
		   { string tempVar = makeTemps();
				 string temp = ($1.code);
				 temp += ($3.code);
				 temp += ". " + tempVar + "\n";
				 temp += "% " + tempVar + ", " + string($1.variables) + ", " + string($3.variables);
				 temp += "\n";
				 $$.code = returnVal(temp);
				 $$.variables = returnVal(tempVar);
			 }
	;

term: 	SUB var
	{ string tempVar = makeTemps();
		string temp = $2.code;
		temp += ". ";
	temp += tempVar;
	temp += "\n";
	if($2.varType == 1){
		temp += "-[] ";
	}
	else if($2.varType == 0){
		temp += "- ";
	}
	temp += tempVar + ", 0 ,";
	temp += $2.variables;
	temp += "\n";
	$$.code = returnVal(temp);
	$$.variables = returnVal(tempVar); }
	| SUB NUMBER
	{ string tempVar = makeTemps();
		string temp = ". ";
		temp += tempVar;
		temp += "\n";
		temp += "- " + tempVar + ", 0 ,";
		temp += to_string($2);
		temp += "\n";
		$$.variables = returnVal(tempVar);
		$$.code = returnVal(temp);
	}
	| SUB L_PAREN expression R_PAREN
	{ string tempVar = makeTemps();
		string temp = $3.code;
		temp += ". ";
		temp += tempVar;
		temp += "\n";
		temp += "- " + tempVar + ", 0 ,";
		temp += $3.variables;
		temp += "\n";
		$$.variables = returnVal(tempVar);
		$$.code = returnVal(temp); }
	| var
		{
			string tempVar = makeTemps();
			string temp = $1.code;
			temp += ". ";
			temp += tempVar;
			temp += "\n";
			if($1.varType == 1){
				temp += "=[] ";
			}
			else if($1.varType == 2){
				temp += "=[][] ";
			}
			else if($1.varType == 0){
				temp += "= ";
			}
			temp += tempVar + ", ";
			temp += $1.variables;
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
		{$$.code = strdup($2.code);
		 $$.variables = ($2.variables);
	 }
	| ident L_PAREN expressions R_PAREN
		{

			string temp = $3.code;
			char* str = $3.variables;
			char *token = strtok(str, " ");
				while (token != NULL)
				{
					temp += "param ";
					temp += token;
					temp += "\n";
						token = strtok(NULL, " ");
				}
			string tempVar = makeTemps();
			temp += ". " + tempVar + "\n";
			temp += "call ";
			calledFunctions += " " + string($1);
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
	  { string tempVar = makeTemps();
			string temp =	$1.code;
			temp += $3.code;
			temp += ". " + tempVar + "\n";
			temp += "|| " + tempVar + ", " + $1.variables + ", " + $3.variables + "\n";
			$$.code = returnVal(temp);
			$$.variables = returnVal(tempVar);
		}
	;

relation_and_exp: relation_expr
		  {$$.code = strdup($1.code);
			 $$.variables = ($1.variables);
		 }
		 | relation_expr AND relation_and_exp
		  { string tempVar = makeTemps();
				string temp =	$1.code;
				temp += $3.code;
				temp += ". " + tempVar + "\n";
				temp += "&& " + tempVar + ", " + $1.variables + ", " + $3.variables + "\n";
			 	$$.code = returnVal(temp);
				$$.variables = returnVal(tempVar);
			}
	;

relation_expr: NOT expression comp expression
		{ 	string tempVar = makeTemps();
			string temp = $2.code;
			temp += $4.code;
			temp += ". " + tempVar + "\n";
			temp += $3;
			temp += " " + tempVar + ", " + string($2.variables) + ", " + string($4.variables) + "\n";
			string newTemp = makeTemps();
			temp += ". " + newTemp + "\n";
			temp += "! " + newTemp + ", " + tempVar + "\n";
			$$.code = returnVal(temp);
			$$.variables = returnVal(newTemp);
		}
		| NOT TRUE
		{ string temp = "0";
		$$.code = returnVal(temp);
		$$.variables = returnVal("");
	}
		| NOT FALSE
		{
			string temp = "1";
			$$.code = returnVal(temp);
			$$.variables = returnVal("");
		}
		| NOT L_PAREN bool_exp R_PAREN
		{
			string newTemp = makeTemps();
			string temp = $3.code;
			temp += ". " + newTemp + "\n";
			temp += "! " + newTemp + ", " + string($3.variables) + "\n";
			$$.code = returnVal(temp);
			$$.variables = returnVal(newTemp);
		}
		| expression comp expression
			{
				string tempVar = makeTemps();
				string temp = $1.code;
			 	temp += $3.code;
				temp += ". " + tempVar + "\n";
				temp += $2;
				temp += " " + tempVar + ", " + string($1.variables) + ", " + string($3.variables) + "\n";
				$$.code = returnVal(temp);
				$$.variables = returnVal(tempVar);
			}
		| TRUE
			{
				string temp = "1";
		  	$$.code = returnVal(temp);
				$$.variables = returnVal("");
			}
		| FALSE
			{
				string temp ="0";
				$$.code = returnVal(temp);
				$$.variables = returnVal("");
			}
		| L_PAREN bool_exp R_PAREN
			{
				$$.code = strdup($2.code);
				$$.variables = ($2.variables);
			}
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
		int x = symbol_table.find(name)->second;
		string temp =  " variable name \"" + name +  "\" is already defined as an ";

		if(x == 0){
			temp += "integer\n";
		}
		else if(x == 1){
			temp += "array\n";
		}
		else{
			temp += "function name\n";
		}
		yyerror(returnVal(temp));

	}
		symbol_table.insert(pair<string, int>(name, num));


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
	string ret = "__label__" + to_string(num_label);
	num_label++;
	return ret;
 }

void yyerror (const char * msg){
	printf("Error in line %d:%s\n", currLine, msg);
	no_error = false;
}
