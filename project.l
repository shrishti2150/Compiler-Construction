%option yylineno
%{
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
#include <math.h>
%}

%%
"void" {return VOID;}
"main()" {return MAIN;}
"Terminate" {return TERMINATE;}
"Start" {return START;}
"display" {return DISPLAY;}
"Dtype" {return DTYPE;}
"int"								{yylval.data_type=0; return INT;}
"char"								{yylval.data_type=1; return CHAR;}
"float"								{yylval.data_type=2; return FLOAT;}
"double"							{yylval.data_type=3; return DOUBLE;}
"Vars" {return VARS;}
"input" {return INPUT;}
"for" {return FOR;}
"scope" {return SCOPE;}
"execute" {return EXECUTE;}
"endLoop" {return ENDLOOP;}
"till" {return TILL;}
"continue" {return CONTINUE;}
"check" {return CHECK;}
"To" {return TO;}
"True1" {return T;}
"False1" {return F;}
"over" {return OVER;}
"jumpto" {return JUMPTO;}
"DeFunc" {return DEFUNC;}
"funcReturn" {return FUNCRETURN;}
"=" {return EQUAL;}
"," {return COMMA;}
";" {return SCOLON;}
"|" {return BAR;}
[\"] {return DQUOTE;}
"(" {return LB;}
")" {return RB;}
"{" {return LCB;}
"}" {return RCB;}
"[" {return LSQRB;}
"]" {return RSQRB;}
":" {return COLON;}
"++" {return IO;}
"--" {return DO;}
"**" {return POW;}
"*" {return PROD;}
"/" {return DIV;}
"%" {return MOD;}
"+" {return PLUS;}
"-" {return MINUS;}
">" {return GT;}
">=" {return GTE;}
"<" {return LT;}
"<=" {return LTE;}
"<>" {return ET;}
"NOT" {return NOT;}
"%%" {return COMMENT;}
"%*" {return MC1;}
"*%" {return MC2;}
"~" {return NOT;} 
"AND" {return AND;}
"OR" {return OR;}
[a-zA-Z]+ {strcpy(yylval.var_name,yytext); return VAR;}
[0-9]+ {strcpy(yylval.var_name,yytext); return NUMBER;}

[\n\t ]+ {}
. {printf("invalid character %s \n",yytext); exit(0);} 

%%
int yywrap(void)
{
return 1;
}









