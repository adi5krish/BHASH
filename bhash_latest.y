%{
#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
FILE *yyin;
int intArr[100];

void yyerror(char*);
%}

%token ID
%token CONSTANT
%token FUNC
%token CHAR_CONSTANT
%token EQUALTO
%token INT
%token BLOCK
%token DOUBLE
%token DEFVAR
%token CHAR
%token STRING
%token LIST
%token BOOL
%token WHILE
%token IF
%token ELSE

%token ELIF
%token CASE

%token DEF
%token DO
%token FOR
%token BREAK
%token RETURN
%token CONTINUE
%token DEFAULT
%token BRIDGE
%token LABEL
%token TRUE_
%token FALSE_
%token INC
%token DEC
%token B_OP
%token RELOP
%token OR_B_RELOP
%token AND_B_RELOP
%token NOT_B_RELOP
%token MULOP
%token U_OP
%token BOOLEAN
%token UNARYOP	
%token END
%token SWITCH
%token NUM

%token VALUE
%token TYPEDEF
%token PRINT
%token PRINTLN
%token INPUT
%token SUMOP
%token COMMENT

%left '+' '-' '^'
%left '*' '/' '%'




%%

program : declarationlist {printf("parsed successfully\n");}
	;
declarationlist: declaration declarationlist 
		| declaration
		;

declaration : function_definition 
	    | variable_declaration
	
	 ;


function_definition : FUNC ID '(' params ')' statement; 
variable_declaration : variable_declaration datatype init_dec_list
		     | datatype  init_dec_list
;
init_dec_list     : init_dec ';'
			|init_dec ',' init_dec_list   ;
init_dec : ID EQUALTO initializer 		{intArr[$1] =$3;}
	 | ID					{$$ = intArr[$1];}
	 | ID '[' NUM ']'   {}	
	 | ID '[' NUM ']' EQUALTO initializer
	;
initializer : expression 		
		| CONSTANT 		{$$ = $1;}
		| list_init
		| NUM 			{$$ = $1;}
		|CHAR_CONSTANT		{$$ = $1;}
		    ;
list_init : '{' list_constant_list '}' ;
list_constant_list :  list_constant_list ',' CONSTANT 
			| CONSTANT
;
list_constant_list : list_constant_list ',' NUM
			| NUM
;
list_constant_list : list_constant_list ',' CHAR_CONSTANT
		|CHAR_CONSTANT
;
params : paramlist | ;
paramlist: datatype ID 
	 | paramlist ',' datatype ID
;
datatype : INT
	 | DOUBLE
	 | DEFVAR
	 | CHAR
	 | STRING
	 | BOOLEAN
	 | BLOCK
;	 

expression : ID EQUALTO simpleexpression       	  {intArr[$1] = $3;}
	   | immutable B_OP simpleexpression      {if($2 == 10)
							{$1 = $1 + $3;}
			
						   else if($2 == 11)
							{$1 = $1 - $3;}

						   else if($2 == 12)
							{$1 = $1 * $3;}

						   else if($2 == 13)
							{$1 = $1 / $3;}

						   else if($2 == 14)
							{$1 = $1 % $3;}
						   }
	   | immutable inc_dec  {
				if ($2 == 16) 
				{	
					$1=$1+1;
					intArr[$1] = $1;
					
				}
				else if($2 == 17)
					intArr[$1]=intArr[$1]-1;
				 }	
	   | simpleexpression   {$$ = $1;} ;
inc_dec : INC	 {$$=$1;}
	| DEC 	 {$$=$1;};			

simpleexpression : simpleexpression OR_B_RELOP andexpression   {if($1 || $3)
								 $$ = true;
								}
		| andexpression{$$=$1;} ;
andexpression : andexpression AND_B_RELOP unaryrelexpression   {if($1 && $3)
								 $$ = true;
									}
		| unaryrelexpression{$$=$1;} ; 
unaryrelexpression : NOT_B_RELOP unaryrelexpression	       {if(!$2)
								$$ = true;
								}
		| relexpression {$$=$1;}  ;
relexpression : relexpression RELOP sumexpression    {  if($2 == 4)
							   {
							   if($1 != $3)
								$$ = true;
						           }

							else if($2 == 5)
							   {
							   if($1 >= $3)
								$$ = true;
							   }

							 else if($2 == 6)
							   {
							   if($1 <= $3)
								$$ = true;
							  }

							 else if($2 == 7)
							   {
							   if($1 > $3)
								$$ = true;
							   }

							 else if($2 == 8)
							   {
							   if($1 < $3)
								$$ = true;
							   }
	
							 else if($2 == 9)
							   {
							   if($1 == $3)
								$$ = true;
							   }
							}
		| sumexpression {$$=$1;}  ;
sumexpression : sumexpression SUMOP term	{if($2 == 43)
							{$$ = $1 + $3;}

						    else if($2 == 45)
							{$$ = $1 - $3;}
						 }
		| term 				{$$=$1;}
		;
term : term MULOP unaryexpression		{if($2 == 42)
							{$$ = $1 * $3;}

						  else if($2 == 47)
							{$$ = $1 / $3;}

						  else if($2 == 37)
							{$$ = $1 % $3;}
					         }
	| unaryexpression			{$$=$1;}
; 
unaryexpression : UNARYOP unaryexpression  {$$=$2;}
		| factor 		   {$$=$1;}

;
factor : immutable      {$$=$1;}
	| mutable 	{$$=$1;}

;
immutable : ID 		{$$=intArr[$1];}


;
mutable : '(' expression ')'    {$$=$2;}
	| CONSTANT		{$$=$1;}
	| call 			
	| NUM			{$$=$1;}
	| CHAR_CONSTANT		{$$=$1;}
	| TRUE_			{$$ = $1;}
	| FALSE_		{$$ =$1;}
	;		
call : ID '(' args ')'  {} ; 
args : arglist 		
	|
	;
arglist : arglist ',' expression 
	| expression 
	;

statement : expressionstmt //{$$ = $1;
				//printf("e--%d\n",$$);}
		| iterationstmt  //{$$ = $1;}
		| selectionstmt  {$$ = $1;}
		| compoundstmt  // {$$ = $1;}
		| returnstmt     {$$ = $1;}
		| breakstmt      {$$ = $1;}
		| continuestmt   {$$ = $1;}
		| labeledstmt    {$$ = $1;}
		| typecaststatement  {$$ = $1;} 
		| blockstmt      {$$ = $1;}
		| outputstatement  {$$ = $1;}
	        | typedefstatement {$$ = $1;}
		| inputstatement   {$$ = $1;} 	
		| local_declaration {$$ = $1;}
		| funccallstmt  {$$ = $1;}
	;
funccallstmt : funcid_list EQUALTO call 
		|func_var_dec_list EQUALTO call ;

funcid_list : ID | funcid_list ',' ID ;

func_var_dec_list :  func_var_dec_list ',' datatype ID 
		| datatype funcid_list ;

returnstmt : RETURN ';' | RETURN expression ';' | RETURN args ';'
;

breakstmt : BREAK ';'
		| BREAK ID ';'
;
continuestmt : CONTINUE ';' ;
selectionstmt : IF '(' simpleexpression ')' statement
	      | IF '(' simpleexpression ')' statement ELSE statement
	      | IF '(' simpleexpression ')' statement ELIF '(' simpleexpression ')'statement ELSE statement
	      | SWITCH '(' simpleexpression ')' statement
;
labeledstmt : CASE '(' simpleexpression ')' statement labeledstmt
		| DEFAULT statement
		| CASE '(' simpleexpression ')' statement ;
compoundstmt : ':'  statementlist   END //{$$ = $2;
					//printf("c--%d\n",$$);}
			| ':'  END; 
local_declaration : datatype init_dec_list 

;
typecaststatement : datatype ID EQUALTO
		datatype '(' simpleexpression')' ';'
		| ID EQUALTO datatype '(' simpleexpression ')' ';'
		;
blockstmt : ID compoundstmt ;
statementlist : statementlist statement 	//{$$ = $2; printf("s--%d\n",$$);}
		| statement 			//{$$ = $1;};
iterationstmt : WHILE '(' simpleexpression ')' statement 
	      | DO statement WHILE '(' simpleexpression ')' ';' 

	      | FOR '(' for_datatype_dec ID EQUALTO NUM ':' 
		NUM ':' NUM ')' statement		
		;

for_datatype_dec : datatype 
		 | 
;

expressionstmt : expression  ';'  {$$ = $1 ;}
		| ';'
;
typedefstatement : STRING ID EQUALTO TYPEDEF '(' ID ')' ';'
		| ID EQUALTO TYPEDEF '(' ID ')' ';' ;
outputstatement :  PRINT '(' outvariable ')'  ';' {printf("%d",$3);}	
		| PRINTLN '(' outvariable ')' ';' {printf("%d\n",$3);};
outvariable : outvariable ',' value | 
			value {$$=$1;}
		| expression {$$=$1;}
;         	
value : ID 			{$$=$1;}


 ; 		 
inputstatement :  ID  EQUALTO  INPUT '(' input_value ')' ';' 
input_value : CONSTANT {$$ = $1;}
	|
;

%%

void yyerror(char *msg)
{
printf("invalid string   \n");
exit(0);
}

int main()
{
yyin=fopen("code.c","r");
yyparse();
}


