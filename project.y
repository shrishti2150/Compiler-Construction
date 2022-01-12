%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<string.h>
	int yylex(void);
	int yyerror(const char *s);
	int success = 1;
	int current_data_type;
	int expn_type = -1;
	int temp;
	int idx = 0;
	int table_idx = 0;
	int tab_count = 0;
	char for_var[30];
	struct symbol_table{char var_name[30]; int type;} sym[20];
	extern int lookup_in_table(char var[30]);
	extern void insert_to_table(char var[30], int type);
	extern void print_tabs();
	char var_list[20][30];	//20 variable names with each variable being atmost 50 characters long
	int string_or_var[20];
	extern int *yytext;
%}
%union{
int data_type;
char var_name[30];
}

%token START VOID MAIN DISPLAY MC1 MC2 DTYPE VARS TERMINATE INPUT FOR SCOPE COMMENT EXECUTE ENDLOOP TO IN CHAR FLOAT DOUBLE CHAR INT TILL CONTINUE CHECK T F OVER JUMPTO DEFUNC FUNCRETURN EQUAL COMMA SCOLON BAR DQUOTE LB RB LCB RCB LSQRB RSQRB COLON IO DO POW PROD DIV MOD PLUS MINUS GT GTE LT LTE ET NOT AND OR VAR NUMBER

%left LAND LOR GEQ LEQ NOT GT LT NEQ DEQ PLUS MINUS MUL DIV MOD

%token<data_type>INT
%token<data_type>CHAR
%token<data_type>FLOAT
%token<data_type>DOUBLE

%type<data_type>TYPE
%type<var_name>VAR

%start prm
%%

prm:	 	        	FUNC TYPE MAIN START COLON{
						printf("#include<stdio.h>\nint main()\n{\n");
						tab_count++;
					}
					STATEMENTS TERMINATE{
						printf("}\n");
					} FUNC

STATEMENTS: 		STATEMENTS {print_tabs();} STATEMENT
				|;

STATEMENT: 			DTYPE EQUAL TYPE SCOLON VARS EQUAL VAR_LIST BAR {
						if(current_data_type == 0)
							printf("int ");
						else if(current_data_type == 1)
							printf("char ");
						else if(current_data_type == 2)
							printf("float ");
						else if(current_data_type == 3)
							printf("double ");
						for(int i = 0; i < idx - 1; i++){
							insert_to_table(var_list[i], current_data_type);	
							printf("%s,", var_list[i]);
						}
						insert_to_table(var_list[idx - 1], current_data_type);
						printf("%s;\n", var_list[idx - 1]);
						idx = 0;
					}
					| VAR {
							printf("%s", yylval.var_name);
							if((temp=lookup_in_table(yylval.var_name))!=-1) {
								if(expn_type==-1)
									expn_type=temp;
								else if(expn_type!=temp) {
									printf("\n type mismatch in the expression\n");
									yyerror("");
									exit(0);
								}
							}
							else {
								printf("\n variable \" %s\" undeclared\n", yylval.var_name);
								yyerror("");
								exit(0);
							}
							expn_type=-1;
					} 
					EQUAL {printf("=");} A_EXPN BAR {
						printf(";\n");
					}
					| INPUT LB DTYPE EQUAL TYPE SCOLON VARS EQUAL VAR_LIST RB BAR{
						printf("scanf(\"");
						for(int i=0;i<idx-1;i++){
                                                   insert_to_table(var_list[i],current_data_type);

						}
						insert_to_table(var_list[idx-1],current_data_type);
						//printf("%s;",var_list[idx-1]);
						//idx=0;
						for(int i = 0; i < idx; i++) {
							if((temp=lookup_in_table(var_list[i])) != -1) {
								if(temp==0)
									printf("%%d");
								else if(temp==1)
									printf("%%c");
								else if(temp==2)
									printf("%%f");
								else
									printf("%%e");
							}
							else
							{
								printf("Cannot read undeclared variable %s !", yylval.var_name);
								yyerror("");
								exit(0);
							}
						}
						printf("\"");
						for(int i = 0; i < idx; i++) {
							printf(",&%s", var_list[i]);
						}
						printf(");\n");
						idx=0;
					}
					|INPUT LB VARS EQUAL VAR_LIST RB BAR{
                                         printf("scanf(\"");
					 for(int i=0;i<idx;i++){
					 if((temp==lookup_in_table(var_list[i])) != -1){
					 if (temp==0)
					 printf("%%d");
					 else if (temp==1)
					 printf("%%c");
					 else if(temp==2)
					 printf("%%f");
					 else
					 printf("%%e");
					 }
					 else{
					 printf("Cannot read undeclared variable %s ",yylval.var_name);
					 yyerror("");
					 exit(0);
					 }
					 }
					 printf("\"");
					 for(int i=0;i<idx;i++){
					 printf(",&%s",var_list[i]);
					 }
					 printf(");\n");
					 idx=0;
					 }

                                        |COMMENT WRITE_VAR_LIST{printf("// %s\n",yylval.var_name);}
					|MC1 WRITE_VAR_LIST MC2 {printf("/* %s */\n",yylval.var_name);}
                                           					

					|DISPLAY LB DQUOTE WRITE_VAR_LIST DQUOTE RB BAR{
printf("printf(\"%s\"); ",yylval.var_name);

					}
					| IF_BLOCK ELSE_BLOCK OVER
					| IF_BLOCK OVER
					| JUMPTO {printf("goto ");} 
    				  VAR {printf("%s", yylval.var_name);} 
					  SCOLON {printf(";\n");}
					| FOR {printf("for(");} 
					  VAR {strcpy(for_var, yylval.var_name); printf("%s=", for_var);} 
					  IN SCOPE LB TERMINALS {printf("; %s", for_var);} 
					  TO {printf("<=");}
					  
					  A_EXPN {printf("; %s++", for_var);} 
					  RB EXECUTE COLON{printf("){\n"); tab_count++;} 
					  STATEMENTS ENDLOOP {tab_count--;print_tabs();printf("}\n");}
					| EXECUTE {printf("do{\n");tab_count++;}
				          STATEMENTS TILL LB {tab_count--;print_tabs();printf("}while(");} 
				      A_EXPN RB {printf(");\n");}
					| TILL LB {tab_count++; printf("while(");}
					  A_EXPN RB CONTINUE {printf("){\n");}
					  STATEMENTS  ENDLOOP {tab_count--;print_tabs();printf("}\n");}
					| VAR BAR {printf("\b\b\b\b\b\b\b\b%s:\n", yylval.var_name);}
				        | NUMBER BAR {printf(";");}	

IF_BLOCK:		 	CHECK LB {printf("if(");} 
					A_EXPN RB T {printf("){\n");tab_count++;} 
					 STATEMENTS
					{tab_count--;print_tabs();printf("}\n");}
					  	
FUNC: DEFUNC VOID WRITE_VAR_LIST LB RB { printf("void %s (",yylval.var_name); printf(")");}
              STATEMENTS{printf("\n");} FUNCRETURN {printf("return 0;\n");}
|;



ELSE_BLOCK: 	  F {print_tabs();printf("else{\n");tab_count++;} 
					STATEMENTS
					{tab_count--;print_tabs();printf("}\n");}
				
VAR_LIST: 			VAR {
						strcpy(var_list[idx], $1); 
						idx++;
					} COMMA VAR_LIST
					| VAR {
						strcpy(var_list[idx], $1); 
						idx++;
					}


TYPE : 				INT {
						$$=$1;
						current_data_type=$1;	
					}
					| CHAR  {
						$$=$1;
						current_data_type=$1;
					}
					| FLOAT {
						$$=$1;
						current_data_type=$1;
					}
					| DOUBLE {
						$$=$1;
						current_data_type=$1; 
					}

WRITE_VAR_LIST:		DQUOTE {
						strcpy(var_list[idx], yylval.var_name); 
						string_or_var[idx]=1; 
						idx++;
					} 
					| VAR {
						strcpy(var_list[idx], yylval.var_name); 
						idx++;
					} COLON WRITE_VAR_LIST
					| DQUOTE{
						strcpy(var_list[idx], yylval.var_name);
						string_or_var[idx]=1;
						idx++;
					}
					| VAR{
						strcpy(var_list[idx], yylval.var_name);
						idx++;
					}

READ_VAR_LIST:		VAR {
						strcpy(var_list[idx], yylval.var_name); 
						idx++;
					} COMMA READ_VAR_LIST
					| VAR {
						strcpy(var_list[idx], yylval.var_name); 
						idx++;
					}

A_EXPN: 		A_EXPN AND {printf("&&");} A_EXPN
				| A_EXPN OR {printf("||");} A_EXPN
	 			| A_EXPN LTE {printf("<=");} A_EXPN
				| A_EXPN GT {printf(">");} A_EXPN
				| A_EXPN LT {printf("<");} A_EXPN
				| A_EXPN EQUAL {printf("=");} A_EXPN
				| A_EXPN ET {printf("==");} A_EXPN
				| NOT {printf("!");} A_EXPN 
				| A_EXPN PLUS {printf("+");} A_EXPN
				| A_EXPN MINUS {printf("-");} A_EXPN
				| A_EXPN PROD {printf("*");} A_EXPN
				| A_EXPN DIV {printf("/");} A_EXPN
				| A_EXPN MOD {printf("%%");} A_EXPN
				| A_EXPN NOT {printf("!");} A_EXPN
			        | A_EXPN IO {printf("++;");}
				| A_EXPN DO {printf("--;");} 
				| A_EXPN GTE {printf(">=");} A_EXPN
				| TERMINALS

TERMINALS:			VAR {
						if((temp=lookup_in_table(yylval.var_name))!=-1) {
							printf("%s", yylval.var_name);
							if(expn_type==-1){
								expn_type=temp;
							}
							else if(expn_type!=temp){
								printf("\ntype mismatch in the expression\n");
								yyerror("");
								exit(0);
							}
						}
						else{
							printf("\n variable \"%s\" undeclared\n", yylval.var_name);
							yyerror("");
							exit(0);
						}
					}
					
					| NUMBER  {printf("%s",yylval.var_name);}


%%

int lookup_in_table(char var[30])
{
	for(int i=0; i<table_idx; i++)
	{
		if(strcmp(sym[i].var_name, var)==0)
			return sym[i].type;
	}
	return -1;
}

void insert_to_table(char var[30], int type)
{
	if(lookup_in_table(var)==-1)
	{
		strcpy(sym[table_idx].var_name,var);
		sym[table_idx].type = type;
		table_idx++;
	}
	else {
		printf("Multiple declaration of variable\n");
		yyerror("");
		exit(0);
	}
}

void print_tabs() {
	for(int i = 0; i < tab_count; i++){
		printf("\t");
	}
	return;
}

int main() {
	yyparse();
	return 0;
}

int yyerror(const char *msg) {
	extern int yylineno;
	printf("Parsing failed\nLine number: %d %s\n", yylineno, msg);
	success = 0;
	return 0;
}
