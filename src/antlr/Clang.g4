grammar Clang;

// Parser rules

program : (include | define)* definition+;

//Includes
include :   '#' INCLUDE STRING_LITERAL
        |   '#' INCLUDE WITH_ANGLE_BRACKETS
        ;

define : DEFINE ID (INT_CONSTANT | DOUBLE_CONSTANT | FLOAT_CONSTANT | CHAR_CONSTANT | STRING_LITERAL);

definition  :   functionDefinition
            |   functionDeclaration
            |   declaration
            |   assignmentExpression
            |   struct
            |   union
            ;

declaration : TYPE ('*'|'**')? (varWithExpDeclaration | varWithoutExpDeclaration) (',' (varWithoutExpDeclaration |varWithExpDeclaration))* ';'        #VarDeclaration

            |   TYPE ID '[' expression ']' (',' ID '[' expression ']')* ';'                                                                           #ArrayDeclaration
            ;

varWithExpDeclaration: ID  '=' SIGN? expression ;

varWithoutExpDeclaration: ID ;

functionDeclaration : (TYPE | 'void') ID '(' (typeList | STRING_LITERAL | parameterList)? ')' ';'; // int max(int a, int b); o int f(int, double);

functionDefinition  : (TYPE | 'void') ID '(' parameterList? ')' '{' statementCombination '}'; // int max(int a, int b){ return a>b?a:b;}

parameterList : parameterDeclaration ( ',' parameterDeclaration)*; // int a, int b

parameterDeclaration : TYPE ('*'|'**')? ID ('['']')? ; // int a

typeList : TYPE (',' TYPE)*;   //int, double

statementCombination: statement*;

cast: '(' TYPE ')' ;

statement :     compoundStatement
            |   expression ';'
            |   declaration
			|   ifStatement
			|   switchStatement
			|   whileStatement
			|   forStatement
			|   returnStatement ';'
			|   breakStatement ';'
			|   printfFunction ';'
			|   scanfFunction ';'
			|   sqrtFunction ';'
			|   powFunction ';'
			|   sizeOfFunction ';'
			;

compoundStatement :     '{' '}'
                    |   '{' statementCombination '}'
                    |   '{' declarCombination'}'
                    |   '{' declarCombination statementCombination '}'
                    ;
declarCombination: declaration+ ;

returnStatement :   'return' expression?;

breakStatement  :   BREAK | CONTINUE;

//C Functions:

//Printf
printfFunction :   PRINTF '(' printArgument ')';
printArgument :expression | STRING_LITERAL (',' expression)*;

//Scanf
scanfFunction:   SCANF '(' scanfArgument ')' ;
scanfArgument: SCANF_CONVERSION_SPECIFICATION ',' '&'ID(',''&'ID)* ;

//Sqrt
sqrtFunction : SQRT '(' sqrtArgument ')' ;
sqrtArgument : ID | DOUBLE_CONSTANT | CHAR_CONSTANT;

//Pow
powFunction : POW '(' powArgument ')' ;
powArgument : (ID | DOUBLE_CONSTANT | CHAR_CONSTANT) ',' (ID | DOUBLE_CONSTANT | CHAR_CONSTANT);

//Sizeof
sizeOfFunction: SIZEOF '(' sizeOfArgument ')' ;
sizeOfArgument: (TYPE | ID);

c_functions: printfFunction | scanfFunction | powFunction | sqrtFunction | sizeOfFunction ;

expressionList : expression ( ',' expression)* ;

expression :    '(' expression ')'                                              #ExprParenthesis
            |   functionCall                                                    #ExprFunctionCall
            |   c_functions                                                     #ExpC_FunctionCall
            |   arrayIndexExpression                                            #ExprArrayIndex
            |   left=expression op=('*'|'/'|'%') right= expression              #ExprArit
            |   left=expression op=('+'|'-') right=expression                   #ExprArit
            |   left=expression op=('>'|'>='|'<'|'<=') right=expression         #ExprRel
            |   left=expression op=('=='|'!=')  right=expression                #ExprRel
            |   expression '&&' expression                                      #ExprAnd
            |   expression '||' expression                                      #ExprOr
            |   unaryOperator=('+'|'-'|'++'|'--'|'~'|'!')(INT_CONSTANT | ID)    #ExprUnaryOpPost
            |   ID unaryOperator=('++'|'--')                                    #ExprUnaryOpPre
            |   constant                                                        #ExprCnt
            |   ('&'|'*') ? ID                                                  #ExprId
            |   cast? ID                                                        #ExprCast
            |   assignmentExpression                                            #ExprAssignment
            |   arrayAssigmentExpression                                        #ExpArrayAssigment
            |   expression '?' expression ':' expression                        #TernaryExpression
            ;

constant    :  INT_CONSTANT                                                #IntCnt
            |  CHAR_CONSTANT                                               #CharCnt
            |  FLOAT_CONSTANT                                              #FloatCnt
            |  STRING_LITERAL                                              #StrLCnt
            |  DOUBLE_CONSTANT                                             #DOubleCnt
            ;


functionCall : ID '(' expressionList? ')' ;

assignmentExpression : unaryExpression assignmentOperator cast? expression;

unaryExpression : ID | arrayIndexExpression;
arrayIndexExpression:  ID '[' expression ']' ;

arrayAssigmentExpression: TYPE ID '[' INT_CONSTANT? ']' '=' '{' ((DOUBLE_CONSTANT | CHAR_CONSTANT | STRING_LITERAL | INT_CONSTANT) | ((DOUBLE_CONSTANT | CHAR_CONSTANT | STRING_LITERAL | INT_CONSTANT)  ','))* '}' ;


assignmentOperator: ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '&=' | '^=' | '|=');

ifStatement : IF '(' expression ')' (';' | statement) (ELSE (';' | statement ))?;

whileStatement : WHILE '(' expression ')' (';' | statement) ;

forStatement : FOR '(' expressionList? ';' condExpression? ';' iterExpression? ')' (';' | statement) ;

switchStatement : SWITCH '(' expression ')' '{' cases default?'}' ;

cases: (((CASE (CHAR_CONSTANT | INT_CONSTANT))) ':' cases | switch_actions ';')+;

default : DEFAULT ':' (cases | switch_actions);

switch_actions : expression
                | printfFunction
                | scanfFunction
                | sqrtFunction
                | powFunction
                | sizeOfFunction
                | forStatement
                | whileStatement
                | ifStatement
                | switchStatement
                | breakStatement
                | returnStatement
                ;

condExpression: expression;
iterExpression: expression;

struct: STRUCT ID '{' (declaration)+ '}'(ID (',' ID)*)? ';';

union: UNION '{' (declaration)+ '}' (ID (',' ID)*)? ';' ;



// Scanner rules
WHILE : 'while';
IF : 'if';
ELSE : 'else';
SWITCH: 'switch';
CASE: 'case';
DEFAULT: 'default';
FOR : 'for';
CONST : 'const';
PRINTF : 'printf';
SCANF   :  'scanf';
SQRT : 'sqrt';
POW : 'pow';
SIZEOF: 'sizeof';

DOUBLE_CONSTANT : DIGITS | (DIGITS '.' DIGITS) ;

INT_CONSTANT :  OCT_CONSTANT
             |  HEX_CONSTANT
             |  DEC_CONSTANT
             ;

CHAR_CONSTANT : '\'' '\\'?.  '\'';


SCANF_CONVERSION_SPECIFICATION : '"' ('%Lf' | '%lf' | '%f' | '%lu' | '%ld' | '%u' | '%d' | '%hd' | '%c')+ '"';


FLOAT_CONSTANT : SIGN? ((DIGITS '.') | ('.' FRAC_PART) | (DIGITS '.' FRAC_PART) | (FLOAT_WITH_E)) FLOAT_SUFFIX?;

TYPE : SHORT | INT | LONG | FLOAT | DOUBLE | CHAR;

INCLUDE : 'include';
DEFINE: 'define' ;


FLAGS:  SIGN
     | NU
     | Z
     ;

WIDTH:  INT_CONSTANT
     |  AS
     ;

PRECI:  '.'WIDTH;


//Data types
INT : 'int';
FLOAT : 'float';
VOID : 'void';
DOUBLE : 'double';
CHAR : 'char';
SHORT : 'short';
LONG : 'long';

//Estructures
STRUCT: 'struct' ;

//Unions
UNION: 'union' ;

//Jump statements
BREAK : 'break';
CONTINUE : 'continue';

INC_OPERATOR : '++';
DEC_OPERATOR : '--';

ID :    LETTER LET_DIGIT*;

PATH : ID '.' ID;

STRING_LITERAL : '"' (ESC | .)*? '"';

WITH_ANGLE_BRACKETS: '<' PATH '>';

fragment
PC  :   '%';

fragment
NU: '#';

fragment
AS: '*';

fragment
Z:  '0';
fragment
LEN:    'h' | 'l' | 'L';

fragment
SPEC:    'c' | 'd' | 'i' | 'e' | 'E' | 'f' | 'g' | 'G' | 'o' | 's' | 'u' | 'x' | 'X' | 'p' | 'n' | PC;

fragment
O_WS: ' '  ;

fragment
ESC: '\\"' | '\\\\' | '\\' [btnr"\\]; //\b \t \n

fragment
EXTENSION : '.' ID;

fragment
FRAC_PART : DIGITS+ | (DIGITS+ [eE] SIGN? DIGITS+);

fragment
FLOAT_WITH_E : DIGITS+ [eE] SIGN? DIGITS+;

fragment
OCT_CONSTANT : '0' ('0'..'7')+;

fragment
HEX_CONSTANT : ('0x' | '0X') HEX_DIGIT+;

fragment
DEC_CONSTANT : DIGITS;

fragment
SIGN : '+' | '-';

fragment
FLOAT_SUFFIX: [fFlL];

fragment
COMMENT : LINE_COMMENT | MULTILINE_COMMENT;

fragment
DIGIT  :    [0-9];

fragment
DIGITS : DIGIT+;

fragment
HEX_DIGIT : ([A-F]|[a-f]|DIGIT)+;



fragment
LETTER : [_a-zA-Z];

fragment
LET_DIGIT : DIGIT | LETTER;

fragment
STR_OUT:   (ESC | .)*? ;




LINE_COMMENT : '//' .*? (('\r'? '\n') | EOF )  -> skip;

MULTILINE_COMMENT : '/*' .*? '*/' -> skip;

WS : (
        ' ' |
        '\t'|
        '\r'|
        '\n'
     ) -> skip;


