%{
#include <stdio.h>
#include <stdlib.h>
extern FILE * yyin;
extern int currLine;
void yyerror(const char *msg);
%}

%union{
	char* cVal;
	int iVal;
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


%%
prog_start: functions
       	   {printf("program->functions\n");}
	;
functions: /*epsilon*/
       	   {printf("functions->epsilon\n");}
	 |function functions
       	   {printf("functions->function functions\n");}
	;
function: FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
       	  {printf("function->FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statement SEMICOLON END_BODY\n");}
	;

ident:  IDENT
	{printf("ident -> IDENT %s\n", $1);}
  	;
declarations: /*epsilon*/
	      {printf("declarations->epsilon\n");}
	      | declaration SEMICOLON declarations
	      {printf("declarations->declaration SEMICOLON declarations\n");}
	      | error SEMICOLON
	      | declaration error
	;

declaration: identifier COLON INTEGER
	     {printf("declaration -> identifier COLON INTEGER\n");}
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {printf("declaration -> identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
	     | identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
	     {printf("declaration -> identifier COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}	
;

identifier: ident COMMA identifier
	    {printf("identifier -> ident COMMA identifiers\n");}
	    | ident
	    {printf("identifier->ident\n");}
	;

statements: /*epsilon*/
		  {printf("statements->epsilon\n");}
	   |statement SEMICOLON statements
	    {printf("statements -> statement SEMICOLON statements\n");}
	   | error SEMICOLON
	   | statement error	
	  ;

statement: var ASSIGN expression
	   {printf("statement -> var ASSIGN expression\n");}
		 | IF bool_exp THEN statements ENDIF
		 {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
	   | IF bool_exp THEN statements ELSE  statements ENDIF
	   {printf("statement -> IF bool_exp THEN statements ELSE  statements ENDIF\n");}
	   | WHILE bool_exp BEGINLOOP statements ENDLOOP
	   {printf("statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP\n");}
	   | DO BEGINLOOP statements ENDLOOP WHILE bool_exp
	   {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
	   | FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGINLOOP statements ENDLOOP
	   {printf("statement -> FOR var ASSIGN NUMBER SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGINLOOP statements ENDLOOP\n");}
	   | READ vars
	   {printf("statement -> READ vars\n");}
	   | WRITE vars
	   {printf("statement -> WRITE vars\n");}
	   | CONTINUE
	   {printf("statement -> CONTINUE\n");}
	   |
	   RETURN expression
	   {printf("statement -> RETURN expression\n");}
	;

vars: var COMMA vars
	{printf("vars -> var COMMA vars\n");}
	|var
	{printf("vars -> var\n");}
	;

var: ident
	{printf("var -> ident\n");}
	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
	{printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
	{printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
;

expressions: expression COMMA expressions
	     {printf("expression -> expression COMMA expressions\n");}
	     | expression
	     {printf("expression -> expression\n");}
	;

expression: multi_expr
	    {printf("expression -> multi_expr \n");}
	    | multi_expr ADD  expression
            {printf("expression -> multi_expr ADD  expression\n");}
	    | multi_expr SUB  expression
	    {printf("expression -> multi_expr SUB  expression\n");}
	 ;

multi_expr: term
	    {printf("multi_expr -> term\n");}
	   | term MULT multi_expr
	    {printf("multi_expr -> term MULT multi_expr\n");}
	   | term DIV multi_expr
	   {printf("multi_expr -> term DIV multi_expr\n");}
	   | term MOD multi_expr
	   {printf("multi_expr -> term MOD multi_expr\n");}
	;

term: 	SUB var
	| SUB NUMBER
	{printf("term -> SUB NUMBER\n");}
	| SUB L_PAREN expression R_PAREN
	{printf("term -> SUB L_PAREN expression R_PAREN\n");}
	| var
	{printf("term -> var\n");}
	| NUMBER
	{printf("term -> NUMBER %d\n", $1);}
	| L_PAREN expression R_PAREN
	{printf("term -> L_PAREN expression R_PAREN\n");}
	| ident L_PAREN expressions R_PAREN
	{printf("term -> ident L_PAREN expressions R_PAREN\n");}
	;

bool_exp: relation_and_exp
	  {printf("bool_exp -> relation_and_exp\n");}
	  | relation_and_exp OR bool_exp
	  {printf("bool_exp -> relation_and_exp OR bool_exp\n");}
	;

relation_and_exp: relation_expr
		  {printf("relation_and_exp -> relation_expr\n");}
		 | relation_expr AND relation_and_exp
		  {printf("relation_and_exp -> relation_expr AND relation_and_exp\n");}
	;

relation_expr: NOT expression comp expression
		{printf("relation_expr -> NOT expression comp expression\n");}
		| NOT TRUE
		{printf("relation_expr -> NOT TRUE\n");}
		| NOT FALSE
		{printf("relation_expr -> NOT FALSE\n");}
		| NOT L_PAREN bool_exp R_PAREN
		{printf("relation_expr -> NOT L_PAREN bool_exp R_PAREN\n");}
		| expression comp expression
		{printf("relation_expr -> expression comp expression\n");}
		| TRUE
		{printf("relation_expr -> TRUE\n");}
		| FALSE
		{printf("relation_expr -> FALSE\n");}
		| L_PAREN bool_exp R_PAREN
		{printf("relation_expr -> L_PAREN bool_exp R_PAREN\n");}
		;

comp: EQ
	{printf("comp -> EQ\n");}
	| NEQ
	{printf("comp -> NEQ\n");}
	| LT
	{printf("comp -> LT\n");}
	| GT
	{printf("comp -> GT\n");}
	| LTE
	{printf("comp -> LTE\n");}
	| GTE
	{printf("comp -> GTE\n");}
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

void yyerror (const char * msg){
	printf("Error in line %d %s\n", currLine, msg);
}
