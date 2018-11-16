grammar checktestdata;

COMMENT : '#' (~'\n')*'\n' -> skip;
STRING_LITERAL : '"' CHAR* '"';
fragment CHAR : ESCAPED_CHAR | ~["];
fragment ESCAPED_CHAR :
	'\\"' 
	| '\\\\' 
	| '\\n' 
	| '\\t' 
	| '\\r' 
	| '\\b'
	| '\\\n'
	| '\\' [0-7][0-7][0-7]
	| '\\' [0-7][0-7]
	| '\\' [0-7];

main : command* EOF;
command :
	simpleCommand
	| read
	| loop
	| assrt
	| 'SET' '(' assignment (',' assignment)* ')'
	| 'UNSET' '(' unassignment (',' unassignment)* ')'
	| if_
	;
if_ : 'IF' '(' expr ')' command* else_ 'END';
else_ : ('ELSE' command*)?;
assrt : 'ASSERT' '(' expr ')';
simpleCommand : SPACE | NEWLINE | EOF_COMMAND;
read :
	type='INT' '(' min=expr ',' max=expr varopt? ')'
	| type='FLOAT' '(' min=expr ',' max=expr (varopt floatopt?)? ')'
	| type='FLOATP' '(' min=expr ',' max=expr ',' mindecimals=expr ',' maxdecimals=expr (varopt floatopt?)? ')'
	| type='STRING' '(' value=expr ')'
	| type='REGEX' '(' value=expr varopt? ')';
loop :
	'REP' '(' count=expr loopsep? ')' body=command* 'END'
	| 'REPI' '(' i=variable ',' count=expr loopsep? ')' body=command* 'END'
	| 'WHILE' '(' condition=expr loopsep? ')' body=command* 'END'
	| 'WHILEI' '(' i=variable ',' condition=expr loopsep? ')' body=command* 'END';
loopsep : ',' separator=command;

INTEGER_LITERAL : '0'|[1-9][0-9]*;
FLOAT_LITERAL : [0-9]+('.'[0-9]*)?([eE][+-]?[0-9]+)?;
VARNAME : [a-z][a-z0-9]*;
compare : '<' | '>' | '<=' | '>=' | '==' | '!=';
logical : '&&' | '||';
variable : VARNAME ('[' expr (',' expr)* ']')?;
expr : 	expr binop='^' expr
	| unop=( '-' | '!' ) expr
	| expr binop=( '*' | '/' | '%' ) expr
	| expr binop=( '+' | '-' ) expr
	| expr binop=( '<' | '>' | '<=' | '>=' ) expr
	| expr binop=( '==' | '!=' ) expr
	| expr binop='&&' expr
	| expr binop='||' expr
	| unop='(' expr ')'
	| literal=INTEGER_LITERAL
	| literal=FLOAT_LITERAL
	| literal=STRING_LITERAL
	| variable
	| function;
function : 'STRLEN' '(' expr ')'
	| 'MATCH' '(' expr ')'
	| 'ISEOF'
	| 'UNIQUE' '(' VARNAME (',' VARNAME)* ')'
	| 'INARRAY' '(' expr ',' VARNAME ')';
assignment : variable '=' expr;
unassignment : VARNAME;
varopt : ',' variable;
floatopt : ',' format=('FIXED' | 'SCIENTIFIC');
WS : [ \t\r\n] -> skip;
EOF_COMMAND : 'EOF';
SPACE : 'SPACE';
NEWLINE : 'NEWLINE';
INT : 'INT';
FLOAT : 'FLOAT';
FLOATP : 'FLOATP';
STRING : 'STRING';
REGEX : 'REGEX';
FIXED : 'FIXED';
SCIENTIFIC : 'SCIENTIFIC';
EQ : '==';
LE : '<=';
GE : '>=';
NE : '!=';
LT : '<';
GT : '>';
PLUS : '+';
MINUS : '-';
MULT : '*';
DIV : '/';
MOD : '%';
POW : '^';
AND : '&&';
OR : '||';
MATCH : 'MATCH';
STRLEN : 'STRLEN';
ISEOF : 'ISEOF';
UNIQUE : 'UNIQUE';
INARRAY : 'INARRAY';
