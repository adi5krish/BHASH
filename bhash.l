%{
#include<stdio.h>
#include<math.h>
#include "y.tab.h"

void yyerror(char*);
extern int yylval;
%}

letter [a-zA-Z]
digit [0-9]
%%
[ ]				;
[\t]				;
[\n]				{yylineno = yylineno + 1;}
func				{return FUNC;}
input 				{return INPUT;}
int 				{return INT;}
typeDef				{return TYPEDEF;}
block 				{return BLOCK;}
double				{return DOUBLE;}
defvar				{return DEFVAR;}
char 				{return CHAR;}
string 				{return STRING;}
list				{return LIST;}
boolean				{return BOOLEAN;}
while				{return WHILE;}
true				{return TRUE_;}
false				{return FALSE_;}
">"				{ yylval = 7;
				  return RELOP;}
"<"				{ yylval = 8;
				  return RELOP;}		
"++"				{ yylval = 16;
				  return INC;}
"--"				{ yylval= 17; 
				  return DEC;}
"+="				{ yylval = 10;
				  return B_OP;}
"-="				{ yylval = 11;
				  return B_OP;}
"*="				{ yylval = 12;
				  return B_OP;}	
"/="				{ yylval = 13;
				  return B_OP;}
"%="				{ yylval = 14;
				  return B_OP;}
"=" 				{return EQUALTO; }
"=="				{ yylval = 9;
				  return RELOP;}
"and"				{ yylval = 1;
				  return AND_B_RELOP;}
"or"				{ yylval = 2;
				  return OR_B_RELOP;}
"not"				{ yylval = 3;
				  return NOT_B_RELOP;}
"!="				{ yylval = 4;
				  return RELOP;}
">="				{ yylval = 5;
				  return RELOP;}
"<="				{ yylval = 6;
				  return RELOP;}
"+"				{yylval = *yytext;
				 return SUMOP;}
"-"				{yylval = *yytext;
				 return SUMOP;}
"*"				{yylval = *yytext;
				 return MULOP;}
"%"				{yylval = *yytext;
				 return MULOP;}
"/" 				{yylval = *yytext;
				 return MULOP;}
"?"				{return UNARYOP;}

if				{return IF;}
else				{return ELSE;}
elif 				{return ELIF;}
end				{return END;}
case				{return CASE;}
switch				{return SWITCH;}
def				{return DEF;}
for				{return FOR;}
do				{return DO;}
break				{return BREAK;}
continue			{return CONTINUE;}
default 			{return DEFAULT;}
bridge 				{return BRIDGE;}
label				{return LABEL;}
value				{return VALUE;}
return				{return RETURN;}
print 				{return PRINT;}
println 			{return PRINTLN;}
{letter}({letter}|{digit})*	{yylval=* yytext-'a' ;
				return ID;
				}
{digit}+			{ 
				int i,var=0,j=0;
				for(i=yyleng-1;i>=0;i--)
				{	
				var = var +(*(yytext+j)-48)*pow(10,i);
				j++;	
				}
				yylval = var;
				return NUM ;
				}
[\']{letter}[\']		{return CHAR_CONSTANT;}
[\"]{letter}*[\"]		{return CONSTANT;}
"//"(.)*[\n]			{printf("Hello\n");}
"/*"(.|\n)*"*/"			{printf("Hello\n");}


. 				{return yytext[0];}

%%

int yywrap(void) {
 return 1;
}



