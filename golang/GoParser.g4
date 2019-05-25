parser grammar GoParser;

options {
    tokenVocab=GoLexer;
    superClass=GoBaseParser;
}

sourceFile
    : packageClause eos (importDecl eos)* (topLevelDecl eos)*
    ;

packageClause
    : 'package' IDENTIFIER
    ;

importDecl
    : 'import' (importSpec | '(' (importSpec eos)* ')')
    ;

importSpec
    : ('.' | IDENTIFIER)? importPath
    ;

importPath
    : string
    ;

topLevelDecl
    : declaration
    | functionDecl
    | methodDecl
    ;

declaration
    : constDecl
    | typeDecl
    | varDecl
    ;

constDecl
    : 'const' (constSpec | '(' (constSpec eos)* ')')
    ;

constSpec
    : identifierList (type? '=' expressionList)?
    ;

identifierList
    : IDENTIFIER (',' IDENTIFIER)*
    ;

expressionList
    : expression (',' expression)*
    ;

typeDecl
    : 'type' (typeSpec | '(' (typeSpec eos)* ')')
    ;

typeSpec
    : IDENTIFIER type
    ;

// Function declarations

functionDecl
    : 'func' IDENTIFIER (function | signature)
    ;

function
    : signature block
    ;

methodDecl
    : 'func' receiver IDENTIFIER (function | signature)
    ;

receiver
    : parameters
    ;

varDecl
    : 'var' (varSpec | '(' (varSpec eos)* ')')
    ;

varSpec
    : identifierList (type ('=' expressionList)? | '=' expressionList)
    ;

block
    : '{' statementList '}'
    ;

statementList
    : (statement eos)*
    ;

statement
    : declaration
    | labeledStmt
    | simpleStmt
    | goStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
    | fallthroughStmt
    | block
    | ifStmt
    | switchStmt
    | selectStmt
    | forStmt
    | deferStmt
    ;

simpleStmt
    : sendStmt
    | expressionStmt
    | incDecStmt
    | assignment
    | shortVarDecl
    | emptyStmt
    ;

expressionStmt
    : expression
    ;

sendStmt
    : expression '<-' expression
    ;

incDecStmt
    : expression ('++' | '--')
    ;

assignment
    : expressionList assign_op expressionList
    ;

assign_op
    : ('+' | '-' | '|' | '^' | '*' | '/' | '%' | '<<' | '>>' | '&' | '&^')? '='
    ;

shortVarDecl
    : identifierList ':=' expressionList
    ;

emptyStmt
    : ';'
    ;

labeledStmt
    : IDENTIFIER ':' statement
    ;

returnStmt
    : 'return' expressionList?
    ;

breakStmt
    : 'break' IDENTIFIER?
    ;

continueStmt
    : 'continue' IDENTIFIER?
    ;

gotoStmt
    : 'goto' IDENTIFIER
    ;

fallthroughStmt
    : 'fallthrough'
    ;

deferStmt
    : 'defer' expression
    ;

ifStmt
    : 'if' (simpleStmt ';')? expression block ('else' (ifStmt | block))?
    ;

switchStmt
    : exprSwitchStmt | typeSwitchStmt
    ;

exprSwitchStmt
    : 'switch' (simpleStmt ';')? expression? '{' exprCaseClause* '}'
    ;

exprCaseClause
    : exprSwitchCase ':' statementList
    ;

exprSwitchCase
    : 'case' expressionList | 'default'
    ;

typeSwitchStmt
    : 'switch' (simpleStmt ';')? typeSwitchGuard '{' typeCaseClause* '}'
    ;

typeSwitchGuard
    : (IDENTIFIER ':=')? primaryExpr '.' '(' 'type' ')'
    ;

typeCaseClause
    : typeSwitchCase ':' statementList
    ;

typeSwitchCase
    : 'case' typeList | 'default'
    ;

typeList
    : type (',' type)*
    ;

selectStmt
    : 'select' '{' commClause* '}'
    ;

commClause
    : commCase ':' statementList
    ;

commCase
    : 'case' (sendStmt | recvStmt) | 'default'
    ;

recvStmt
    : (expressionList '=' | identifierList ':=')? expression
    ;

forStmt
    : 'for' (expression | forClause | rangeClause)? block
    ;

forClause
    : simpleStmt? ';' expression? ';' simpleStmt?
    ;

rangeClause
    : (expressionList '=' | identifierList ':=')? 'range' expression
    ;

goStmt
    : 'go' expression
    ;

type
    : typeName
    | typeLit
    | '(' type ')'
    ;

typeName
    : IDENTIFIER
    | qualifiedIdent
    ;

typeLit
    : arrayType
    | structType
    | pointerType
    | functionType
    | interfaceType
    | sliceType
    | mapType
    | channelType
    ;

arrayType
    : '[' arrayLength ']' elementType
    ;

arrayLength
    : expression
    ;

elementType
    : type
    ;

pointerType
    : '*' type
    ;

interfaceType
    : 'interface' '{' (methodSpec eos)* '}'
    ;

sliceType
    : '[' ']' elementType
    ;

mapType
    : 'map' '[' type ']' elementType
    ;

channelType
    : ('chan' | 'chan' '<-' | '<-' 'chan') elementType
    ;

methodSpec
    : {noTerminatorAfterParams(2)}? IDENTIFIER parameters result
    | typeName
    | IDENTIFIER parameters
    ;

functionType
    : 'func' signature
    ;

signature
    : {noTerminatorAfterParams(1)}? parameters result
    | parameters
    ;

result
    : parameters
    | type
    ;

parameters
    : '(' (parameterList ','?)? ')'
    ;

parameterList
    : parameterDecl (',' parameterDecl)*
    ;

parameterDecl
    : identifierList? '...'? type
    ;

// Operands

operand
    : literal
    | operandName
    | methodExpr
    | '(' expression ')'
    ;

literal
    : basicLit
    | compositeLit
    | functionLit
    ;

basicLit
    : integer
    | string
    | FLOAT_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    ;

integer
    : DECIMAL_LIT
    | OCTAL_LIT
    | HEX_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    ;

operandName
    : IDENTIFIER
    | qualifiedIdent
    ;

qualifiedIdent
    : IDENTIFIER '.' IDENTIFIER
    ;

compositeLit
    : literalType literalValue
    ;

literalType
    : structType
    | arrayType
    | '[' '...' ']' elementType
    | sliceType
    | mapType
    | typeName
    ;

literalValue
    : '{' (elementList ','?)? '}'
    ;

elementList
    : keyedElement (',' keyedElement)*
    ;

keyedElement
    : (key ':')? element
    ;

key
    : IDENTIFIER
    | expression
    | literalValue
    ;

element
    : expression
    | literalValue
    ;

structType
    : 'struct' '{' (fieldDecl eos)* '}'
    ;

fieldDecl
    : ({noTerminatorBetween(2)}? identifierList type | anonymousField) string?
    ;

string
    : RAW_STRING_LIT
    | INTERPRETED_STRING_LIT
    ;

anonymousField
    : '*'? typeName
    ;

functionLit
    : 'func' function
    ;

primaryExpr
    : operand
    | conversion
    | primaryExpr selector
    | primaryExpr index
    | primaryExpr slice
    | primaryExpr typeAssertion
    | primaryExpr arguments
    ;

selector
    : '.' IDENTIFIER
    ;

index
    : '[' expression ']'
    ;

slice
    : '[' ((expression? ':' expression?) | (expression? ':' expression ':' expression)) ']'
    ;

typeAssertion
    : '.' '(' type ')'
    ;

arguments
    : '(' ((expressionList | type (',' expressionList)?) '...'? ','?)? ')'
    ;

methodExpr
    : receiverType '.' IDENTIFIER
    ;

receiverType
    : typeName
    | '(' '*' typeName ')'
    | '(' receiverType ')'
    ;

expression
    : unaryExpr
//    | expression BINARY_OP expression
    | expression ('||' | '&&' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '|' | '^' | '*' | '/' | '%' | '<<' | '>>' | '&' | '&^') expression
    ;

unaryExpr
    : primaryExpr
    | ('+'|'-'|'!'|'^'|'*'|'&'|'<-') unaryExpr
    ;

conversion
    : type '(' expression ','? ')'
    ;

eos
    : ';'
    | EOF
    | {lineTerminatorAhead()}?
    | {checkPreviousTokenText("}")}?
    ;

