/*******************************************************/
/*                     Cminus Parser                   */
/*                                                     */
/*******************************************************/

// Brian Berg
// CS4121

/*********************DEFINITIONS***********************/
%{
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <util/general.h>
#include <util/symtab.h>
#include <util/symtab_stack.h>
#include <util/dlink.h>
#include <util/string_utils.h>

#define SYMTABLE_SIZE 100

/*********************EXTERNAL DECLARATIONS***********************/

EXTERN(void,Cminus_error,(const char*));

EXTERN(int,Cminus_lex,(void));

EXTERN(DLinkList *, dlinkListAlloc, (Generic atom));

char var [100][100]; // stores variables
char val [100]; // stores coresponding values 
char buf[100]; // temp buffor 
int scan; // scan buffor
int i = 0; 
int j = 0; 
int k = 0;
int n = 0;

char *fileName;

extern int Cminus_lineno;

extern FILE *Cminus_in;
%}

%name-prefix="Cminus_"
%defines

%start Program

%token AND
%token ELSE
%token EXIT
%token FOR
%token IF 		
%token INTEGER 
%token NOT 		
%token OR 		
%token READ
%token WHILE
%token WRITE
%token LBRACE
%token RBRACE
%token LE
%token LT
%token GE
%token GT
%token EQ
%token NE
%token ASSIGN
%token COMMA
%token SEMICOLON
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token PLUS
%token TIMES
%token IDENTIFIER
%token DIVIDE
%token RETURN
%token STRING	
%token INTCON
%token MINUS

%left OR
%left AND
%left NOT
%left LT LE GT GE NE EQ
%left PLUS MINUS
%left TIMES DIVDE

/***********************PRODUCTIONS****************************/
%%
Program		: Procedures 
		{
			//printf("<Program> -> <Procedures>\n");
		}
	  	| DeclList Procedures
		{
			//printf("<Program> -> <DeclList> <Procedures>\n");
		}
          ;

Procedures 	: ProcedureDecl Procedures
		{
			//printf("<Procedures> -> <ProcedureDecl> <Procedures>\n");
		}
	   	|
		{
			//printf("<Procedures> -> epsilon\n");
		}
	   	;

ProcedureDecl : ProcedureHead ProcedureBody
		{
			//printf("<ProcedureDecl> -> <ProcedureHead> <ProcedureBody>\n");
		}
              ;

ProcedureHead : FunctionDecl DeclList 
		{
			//printf("<ProcedureHead> -> <FunctionDecl> <DeclList>\n");
		}
	      | FunctionDecl
		{
			//printf("<ProcedureHead> -> <FunctionDecl>\n");
		}
              ;

FunctionDecl :  Type IDENTIFIER LPAREN RPAREN LBRACE 
		{
			//printf("<FunctionDecl> ->  <Type> <IDENTIFIER> <LP> <RP> <LBR>\n"); 
		}
	      	;

ProcedureBody : StatementList RBRACE
		{
			//printf("<ProcedureBody> -> <StatementList> <RBR>\n");
		}
	      ;


DeclList 	: Type IdentifierList  SEMICOLON 
		{
			//printf("<DeclList> -> <Type> <IdentifierList> <SC>\n");
		}		
	   		| DeclList Type IdentifierList SEMICOLON
	 	{
			//printf("<DeclList> -> <DeclList> <Type> <IdentifierList> <SC>\n");
	 	}
          	;


IdentifierList 	: VarDecl  
		{
			//printf("<IdentifierList> -> <VarDecl>\n");
		}		
                | IdentifierList COMMA VarDecl
		{
			//printf("<IdentifierList> -> <IdentifierList> <CM> <VarDecl>\n");
		}
                ;

VarDecl 	: IDENTIFIER
		{ 
			// adds new varaibles to the var array
			sprintf(buf, "%s", $1 ); 
			for(j = 0; j < strlen(buf); j++){
				var[i][j] = buf[j];
			}
			i++;

			//printf("<VarDecl> -> <IDENTIFIER\n");
		}
			| IDENTIFIER LBRACKET INTCON RBRACKET
                {
			//printf("<VarDecl> -> <IDENTIFIER> <LBK> <INTCON> <RBK>\n");
		}
		;

Type     	: INTEGER 
		{ 
			$$ = $1;
			//printf("<Type> -> <INTEGER>\n");
		}
                ;

Statement 	: Assignment
		{ 
			//printf("<Statement> -> <Assignment>\n");
		}
                | IfStatement
		{ 
			//printf("<Statement> -> <IfStatement>\n");
		}
		| WhileStatement
		{ 
			//printf("<Statement> -> <WhileStatement>\n");
		}
                | IOStatement 
		{ 
			//printf("<Statement> -> <IOStatement>\n");
		}
		| ReturnStatement
		{ 
			//printf("<Statement> -> <ReturnStatement>\n");
		}
		| ExitStatement	
		{ 
			//printf("<Statement> -> <ExitStatement>\n");
		}
		| CompoundStatement
		{ 
			//printf("<Statement> -> <CompoundStatement>\n");
		}
                ;

Assignment      : Variable ASSIGN Expr SEMICOLON
		{
			// looks for match to variable then assigns coresponding spot
			// in val array
			sprintf(buf, "%s", $1);
			for(j = 0; j < i; j++)
				for(k = 0; k < strlen(buf); k++){
					if(var[j][k] == buf[k]){
						val[j] = $3;
				}
			}

			//printf("<Assignment> -> <Variable> <ASSIGN> <Expr> <SC>\n");
		}
                ;
				
IfStatement	: IF TestAndThen ELSE CompoundStatement
		{

			//printf("<IfStatement> -> <IF> <TestAndThen> <ELSE> <CompoundStatement>\n");
		}
		| IF TestAndThen
		{
			//printf("<IfStatement> -> <IF> <TestAndThen>\n");
		}
		;
		
				
TestAndThen	: Test CompoundStatement
		{
			//printf("<TestAndThen> -> <Test> <CompoundStatement>\n");
		}
		;
				
Test		: LPAREN Expr RPAREN
		{
			$$ = ($2);
			//printf("<Test> -> <LP> <Expr> <RP>\n");
		}
		;
	

WhileStatement  : WhileToken WhileExpr Statement
		{
			//printf("<WhileStatement> -> <WhileToken> <WhileExpr> <Statement>\n");
		}
                ;
                
WhileExpr	: LPAREN Expr RPAREN
		{
			//printf("<WhileExpr> -> <LP> <Expr> <RP>\n");
		}
		;
				
WhileToken	: WHILE
		{
			//printf("<WhileToken> -> <WHILE>\n");
		}
		;


IOStatement     : READ LPAREN Variable RPAREN SEMICOLON
		{
			// looks for variable then assigs scaned in value
			sprintf(buf, "%s", $3);
			scanf("%d", &scan);
			for(j = 0; j < i; j++){
				if(strcmp( var[j], buf ) == 0){
					val[j] = scan; 
				}
				
			}
			
			//printf("<IOStatement> -> <READ> <LP> <Variable> <RP> <SC>\n");
		}
                | WRITE LPAREN Expr RPAREN SEMICOLON
		{
			$$ = ($3);
			printf("%d\n", $$);
			//printf("<IOStatement> -> <WRITE> <LP> <Expr> <RP> <SC>\n");
		}
                | WRITE LPAREN StringConstant RPAREN SEMICOLON         
		{
			$$ = ($3);
			printf("%s\n", $3);
			//printf("<IOStatement> -> <WRITE> <LP> <StringConstant> <RP> <SC>\n");
			
		}
                ;

ReturnStatement : RETURN Expr SEMICOLON
		{
			//printf("<ReturnStatement> -> <RETURN> <Expr> <SC>\n");
		}
                ;

ExitStatement 	: EXIT SEMICOLON
		{
			//printf("<ExitStatement> -> <EXIT> <SC>\n");
		}
		;

CompoundStatement : LBRACE StatementList RBRACE
		{
			$$ = $2;
			//printf("<CompoundStatement> -> <LBR> <StatementList> <RBR>\n");
		}
                ;

StatementList   : Statement
		{		
			$$ = $1;
			//printf("<StatementList> -> <Statement>\n");
		}
                | StatementList Statement
		{		
			// maybe
			$$ = ($1 , $2);
			//printf("<StatementList> -> <StatementList> <Statement>\n");
		}
                ;

Expr            : SimpleExpr
		{
			$$ = $1;
			//printf("<Expr> -> <SimpleExpr>\n");
		}
                | Expr OR SimpleExpr 
		{
			$$ = ($1 || $3);
			//printf("<Expr> -> <Expr> <OR> <SimpleExpr> \n");
		}
                | Expr AND SimpleExpr 
		{
			$$ = ($1 && $3);
			//printf("<Expr> -> <Expr> <AND> <SimpleExpr> \n");
		}
                | NOT SimpleExpr 
		{
			$$= !$2;
			//printf("<Expr> -> <NOT> <SimpleExpr> \n");
		}
                ;

SimpleExpr	: AddExpr
		{
			$$ = $1;
			//printf("<SimpleExpr> -> <AddExpr>\n");
		}
                | SimpleExpr EQ AddExpr
		{
			$$ = ($1 == $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <EQ> <AddExpr> \n");
		}
                | SimpleExpr NE AddExpr
		{
			$$ = ($1 != $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <NE> <AddExpr> \n");
		}
                | SimpleExpr LE AddExpr
		{
			$$ = ($1 <= $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <LE> <AddExpr> \n");
		}
                | SimpleExpr LT AddExpr
		{
			$$ = ($1 < $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <LT> <AddExpr> \n");
		}
                | SimpleExpr GE AddExpr
		{
			$$ = ($1 >= $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <GE> <AddExpr> \n");
		}
                | SimpleExpr GT AddExpr
		{
			$$ = ($1 > $3);
			//printf("<SimpleExpr> -> <SimpleExpr> <GT> <AddExpr> \n");
		}
                ;

AddExpr		:  MulExpr            
		{
			$$ = $1;
			//printf("<AddExpr> -> <MulExpr>\n");
		}
                |  AddExpr PLUS MulExpr
		{
			$$ = $1 + $3;
			//printf("<AddExpr> -> <AddExpr> <PLUS> <MulExpr> \n");
		}
                |  AddExpr MINUS MulExpr
		{
			$$ = $1 - $3;
			//printf("<AddExpr> -> <AddExpr> <MINUS> <MulExpr> \n");
		}
                ;

MulExpr		:  Factor
		{
			$$ = $1;
			//printf("<MulExpr> -> <Factor>\n");
		}
                |  MulExpr TIMES Factor
		{
			$$ = $1 * $3;
		
			//printf("<MulExpr> -> <MulExpr> <TIMES> <Factor> \n");
		}
                |  MulExpr DIVIDE Factor
		{
			$$ = $1 / $3;
			//printf("<MulExpr> -> <MulExpr> <DIVIDE> <Factor> \n");
		}		
                ;
				
Factor          : Variable
		{ 
			// get variable out of data structure using associated postions
			// in val and var
			sprintf(buf, "%s", $1);
			for(j = 0; j < i; j++)
				for(k = 0; k < strlen(buf); k++){
					if(var[j][k] == buf[k]){
						$$ = val[j]; 
				}
			}
			//printf("<Factor> -> <Variable>\n");
		}
                | Constant
		{ 
			$$ = $1;
			//printf("<Factor> -> <Constant>\n");
		}
                | IDENTIFIER LPAREN RPAREN
       	{	
       		
       		$$ = $1;
			//printf("<Factor> -> <IDENTIFIER> <LP> <RP>\n");
		}
         	| LPAREN Expr RPAREN
		{
			$$ = ($2);
			
			//printf("<Factor> -> <LP> <Expr> <RP>\n");
		}
                ;  

Variable        : IDENTIFIER
		{
			// store a variable
			$$ = $1;

			sprintf(buf, "%s", $1 ); 

			// checks to see if variable already exists 
			for(j=0; j < i; j++){
				if(strcmp(var[j], buf) == 0){
				n = 1;
				}
			}
			
			if(n != 1){
				// if doesnt exist adds 
				for(j = 0; j < strlen(buf); j++){
					var[i][j] = buf[j];
				}

			}
			
			//printf("<Variable> -> <IDENTIFIER>\n");
		}
                | IDENTIFIER LBRACKET Expr RBRACKET    
        {
        	//No arrays rn
        	//$$ = $1 [ $3  ]; 
        	
			//printf("<Variable> -> <IDENTIFIER> <LBK> <Expr> <RBK>\n");
        }
                ;			       

StringConstant 	: STRING
		{ 
			$$ = $1;
			//printf("<StringConstant> -> <STRING>\n");
		}
                ;

Constant        : INTCON
		{ 
			$$ = $1;
			//printf("<Constant> -> <INTCON>\n");
		}
                ;

%%


/********************C ROUTINES *********************************/

void Cminus_error(const char *s)
{
  fprintf(stderr,"%s: line %d: %s\n",fileName,Cminus_lineno,s);
}

int Cminus_wrap() {
	return 1;
}

static void initialize(char* inputFileName) {

	Cminus_in = fopen(inputFileName,"r");
        if (Cminus_in == NULL) {
          fprintf(stderr,"Error: Could not open file %s\n",inputFileName);
          exit(-1);
        }

	char* dotChar = rindex(inputFileName,'.');
	int endIndex = strlen(inputFileName) - strlen(dotChar);
	char *outputFileName = nssave(2,substr(inputFileName,0,endIndex),".s");
	stdout = freopen(outputFileName,"w",stdout);
        if (stdout == NULL) {
          fprintf(stderr,"Error: Could not open file %s\n",outputFileName);
          exit(-1);
        }

}

static void finalize() {

    fclose(Cminus_in);
    fclose(stdout);
    

}

int main(int argc, char** argv)

{	
	fileName = argv[1];
	initialize(fileName);
	
        Cminus_parse();
  
  	finalize();
  
  	return 0;
}
/******************END OF C ROUTINES**********************/
