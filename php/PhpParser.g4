/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2019, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Student Main for php7 support.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar PhpParser;

options {
    tokenVocab=PhpLexer;
    superClass=PhpParserBase;
}

// HTML
// Also see here: https://github.com/antlr/grammars-v4/tree/master/html

htmlDocument
    : Shebang? (inlineHtml | phpBlock)* EOF
    ;

inlineHtml
    : htmlElement+
    | scriptText
    ;

// TODO: split into html, css and xml elements
htmlElement
    : HtmlDtd
    | HtmlClose
    | HtmlStyleOpen
    | HtmlOpen
    | HtmlName
    | HtmlSlashClose
    | HtmlSlash
    | HtmlText
    | HtmlEquals
    | HtmlStartQuoteString
    | HtmlEndQuoteString
    | HtmlStartDoubleQuoteString
    | HtmlEndDoubleQuoteString
    | HtmlHex
    | HtmlDecimal
    | HtmlQuoteString
    | HtmlDoubleQuoteString

    | StyleBody

    | HtmlScriptOpen
    | HtmlScriptClose

    | XmlStart XmlText* XmlClose
    ;

// Script
// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/javascript if necessary.

scriptText
    : ScriptText+
    ;

// PHP

phpBlock
    : topStatement+
    ;

topStatement
    : statement
    | useDeclaration
    | namespaceDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

useDeclaration
    : Use (Function | Const)? useDeclarationContentList SemiColon
    ;

useDeclarationContentList
    : '\\'? useDeclarationContent (',' '\\'? useDeclarationContent)*
    ;

useDeclarationContent
    : namespaceNameList
    ;

namespaceDeclaration
    : Namespace (namespaceNameList? OpenCurlyBracket namespaceStatement* CloseCurlyBracket | namespaceNameList SemiColon)
    ;

namespaceStatement
    : statement
    | useDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

functionDeclaration
    : attributes Function '&'? id typeParameterListInBrackets? '(' formalParameterList ')' (':' QuestionMark? typeHint)? blockStatement
    ;

classDeclaration
    : attributes Private? modifier?
    (
      classEntryType id typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)? |
      Interface id typeParameterListInBrackets? (Extends interfaceList)?
    )
      OpenCurlyBracket classStatement* CloseCurlyBracket
    ;

classEntryType
    : Class
    | Trait
    ;

interfaceList
    : qualifiedStaticTypeRef (',' qualifiedStaticTypeRef)*
    ;

typeParameterListInBrackets
    : '<:' typeParameterList ':>'
    | '<:' typeParameterWithDefaultsList ':>'
    | '<:' typeParameterList ',' typeParameterWithDefaultsList ':>'
    ;

typeParameterList
    : typeParameterDecl (',' typeParameterDecl)*
    ;

typeParameterWithDefaultsList
    : typeParameterWithDefaultDecl (',' typeParameterWithDefaultDecl)*
    ;

typeParameterDecl
    : attributes id
    ;

typeParameterWithDefaultDecl
    : attributes id Eq (qualifiedStaticTypeRef | primitiveType)
    ;

genericDynamicArgs
    : '<:' typeRef (',' typeRef)* ':>'
    ;

attributes
    : attributesGroup*
    ;

attributesGroup
    : '[' (id ':')? attribute (',' attribute)* ']'
    ;

attribute
    : qualifiedNamespaceName
    | qualifiedNamespaceName '(' attributeArgList ')'
    | qualifiedNamespaceName '(' attributeNamedArgList ')'
    | qualifiedNamespaceName '(' attributeArgList ',' attributeNamedArgList ')'
    ;

attributeArgList
    : expression (',' expression)*
    ;

attributeNamedArgList
    : attributeNamedArg (',' attributeNamedArg)*
    ;

attributeNamedArg
    : VarName '=>' expression
    ;

innerStatementList
    : innerStatement*
    ;

innerStatement
    : statement
    | functionDeclaration
    | classDeclaration
    ;

// Statements

statement
    : id ':'
    | blockStatement
    | ifStatement
    | whileStatement
    | doWhileStatement
    | forStatement
    | switchStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | yieldExpression SemiColon
    | globalStatement
    | staticVariableStatement
    | echoStatement
    | expressionStatement
    | unsetStatement
    | foreachStatement
    | tryCatchFinally
    | throwStatement
    | gotoStatement
    | declareStatement
    | emptyStatement
    | inlineHtmlStatement
    ;

emptyStatement
    : SemiColon
    ;

blockStatement
    : OpenCurlyBracket innerStatementList CloseCurlyBracket
    ;

ifStatement
    : If parentheses statement elseIfStatement* elseStatement?
    | If parentheses ':' innerStatementList elseIfColonStatement* elseColonStatement? EndIf SemiColon
    ;

elseIfStatement
    : ElseIf parentheses statement
    ;

elseIfColonStatement
    : ElseIf parentheses ':' innerStatementList
    ;

elseStatement
    : Else statement
    ;

elseColonStatement
    : Else ':' innerStatementList
    ;

whileStatement
    : While parentheses (statement | ':' innerStatementList EndWhile SemiColon)
    ;

doWhileStatement
    : Do statement While parentheses SemiColon
    ;

forStatement
    : For '(' forInit? SemiColon expressionList? SemiColon forUpdate? ')' (statement | ':' innerStatementList EndFor SemiColon )
    ;

forInit
    : expressionList
    ;

forUpdate
    : expressionList
    ;

switchStatement
    : Switch parentheses (OpenCurlyBracket SemiColon? switchBlock* CloseCurlyBracket | ':' SemiColon? switchBlock* EndSwitch SemiColon)
    ;

switchBlock
    : ((Case expression | Default) (':' | SemiColon))+ innerStatementList
    ;

breakStatement
    : Break expression? SemiColon
    ;

continueStatement
    : Continue expression? SemiColon
    ;

returnStatement
    : Return expression? SemiColon
    ;

expressionStatement
    : expression SemiColon
    ;

unsetStatement
    : Unset '(' chainList ')' SemiColon
    ;

foreachStatement
    : Foreach
        ( '(' chain As '&'? assignable ('=>' '&'? chain)? ')'
        | '(' expression As assignable ('=>' '&'? chain)? ')'
        | '(' chain As List '(' assignmentList ')' ')' )
      (statement | ':' innerStatementList EndForeach SemiColon)
    ;

tryCatchFinally
    : Try blockStatement (catchClause+ finallyStatement? | catchClause* finallyStatement)
    ;

catchClause
    : Catch '(' qualifiedStaticTypeRef ('|' qualifiedStaticTypeRef)* VarName ')' blockStatement
    ;

finallyStatement
    : Finally blockStatement
    ;

throwStatement
    : Throw expression SemiColon
    ;

gotoStatement
    : Goto id SemiColon
    ;

declareStatement
    : Declare '(' declareList ')' (statement | ':' innerStatementList EndDeclare SemiColon)
    ;

inlineHtmlStatement
    : inlineHtml+
    ;

declareList
    : identifierInitializer (',' identifierInitializer)*
    ;

formalParameterList
    : formalParameter? (',' formalParameter)*
    ;

formalParameter
    : attributes QuestionMark? typeHint? '&'? '...'? variableInitializer
    ;

typeHint
    : qualifiedStaticTypeRef
    | Callable
    | primitiveType
    ;

globalStatement
    : Global globalVar (',' globalVar)* SemiColon
    ;

globalVar
    : VarName
    | Dollar (chain | OpenCurlyBracket expression CloseCurlyBracket)
    ;

echoStatement
    : Echo expressionList SemiColon
    ;

staticVariableStatement
    : Static variableInitializer (',' variableInitializer)* SemiColon
    ;

classStatement
    : attributes propertyModifiers typeHint? variableInitializer (',' variableInitializer)* SemiColon
    | attributes memberModifiers? Const typeHint? classIdentifierInitializer (',' classIdentifierInitializer)* SemiColon
    | attributes memberModifiers? Function '&'? entityId typeParameterListInBrackets? '(' formalParameterList ')' baseCtorCall? methodBody
    | Use qualifiedNamespaceNameList traitAdaptations
    ;

classIdentifierInitializer
    : entityId Eq constantInititalizer
    ;

traitAdaptations
    : SemiColon
    | OpenCurlyBracket traitAdaptationStatement* CloseCurlyBracket
    ;

traitAdaptationStatement
    : traitPrecedence
    | traitAlias
    ;

traitPrecedence
    : qualifiedNamespaceName '::' id InsteadOf qualifiedNamespaceNameList SemiColon
    ;

traitAlias
    : (qualifiedNamespaceName '::')? id As (memberModifier | memberModifier? id) SemiColon
    ;

baseCtorCall
    : ':' id arguments?
    ;

methodBody
    : SemiColon
    | blockStatement
    ;

propertyModifiers
    : memberModifiers
    | Var
    ;

memberModifiers
    : memberModifier+
    ;

variableInitializer
    : VarName (Eq constantInititalizer)?
    ;

globalConstantDeclaration
    : attributes Const identifierInitializer (',' identifierInitializer)* SemiColon
    ;

identifierInitializer
    : id Eq constantInititalizer
    ;

expressionList
    : expression (',' expression)*
    ;

parentheses
    : '(' (expression | yieldExpression) ')'
    ;

// Expressions
// Grouped by priorities: http://php.net/manual/en/language.operators.precedence.php
expression
    : Clone expression                                          #CloneExpression
    | newExpr                                                   #NewExpression

    | stringConstant '[' expression ']'                         #IndexerExpression

    | '(' castOperation ')' expression                          #CastExpression
    | ('~' | '@') expression                                    #UnaryOperatorExpression

    | ('!' | '+' | '-') expression                              #UnaryOperatorExpression

    | ('++' | '--') chain                                       #PrefixIncDecExpression
    | chain ('++' | '--')                                       #PostfixIncDecExpression

    | Print expression                                          #PrintExpression

    | chain                                                     #ChainExpression
    | constant                                                  #ScalarExpression
    | string                                                    #ScalarExpression
    | id                                                        #ScalarExpression

    | BackQuoteString                                           #BackQuoteStringExpression
    | parentheses                                               #ParenthesisExpression
    | arrayCreation                                             #ArrayCreationExpression

    | Yield                                                     #SpecialWordExpression
    | List '(' assignmentList ')' Eq expression                 #SpecialWordExpression
    | IsSet '(' chainList ')'                                   #SpecialWordExpression
    | Empty '(' chain ')'                                       #SpecialWordExpression
    | Eval '(' expression ')'                                   #SpecialWordExpression
    | (Exit | Die) ( '(' ')' | parentheses )?                   #SpecialWordExpression
    | (Include | IncludeOnce) expression                        #SpecialWordExpression
    | (Require | RequireOnce) expression                        #SpecialWordExpression
    | HaltCompiler '(' ')'                                      #SpecialWordExpression

    | Static? Function '&'? '(' formalParameterList ')' lambdaFunctionUseVars? (':' typeHint)? blockStatement
                                                                #LambdaFunctionExpression
    | Fn '(' formalParameterList')' '=>' expression             #LambdaFunctionExpression

    | <assoc=right> expression op='**' expression               #ArithmeticExpression
    | expression InstanceOf typeRef                             #InstanceOfExpression
    | expression op=('*' | Divide | '%') expression             #ArithmeticExpression

    | expression op=('+' | '-' | '.') expression                #ArithmeticExpression

    | expression op=('<<' | '>>') expression                    #ComparisonExpression
    | expression op=(Less | '<=' | Greater | '>=') expression   #ComparisonExpression
    | expression op=('===' | '!==' | '==' | IsNotEq) expression #ComparisonExpression

    | expression op='&' expression                              #BitwiseExpression
    | expression op='^' expression                              #BitwiseExpression
    | expression op='|' expression                              #BitwiseExpression
    | expression op='&&' expression                             #BitwiseExpression
    | expression op='||' expression                             #BitwiseExpression

    | expression op=QuestionMark expression? ':' expression     #ConditionalExpression
    | expression op='??' expression                             #NullCoalescingExpression
    | expression op='<=>' expression                            #SpaceshipExpression

    | assignable assignmentOperator expression                  #AssignmentExpression
    | assignable Eq '&' (chain | newExpr)                       #AssignmentExpression

    | expression op=And expression                              #LogicalExpression
    | expression op=Xor expression                              #LogicalExpression
    | expression op=Or expression                               #LogicalExpression
    ;

assignable
    : chain
    | arrayCreation
    ;

arrayCreation
    : (Array '(' arrayItemList? ')' | '[' arrayItemList? ']') ('[' expression ']')?
    ;

newExpr
    : New typeRef arguments?
    ;

assignmentOperator
    : Eq
    | '+='
    | '-='
    | '*='
    | '**='
    | '/='
    | '.='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | '>>='
    | '??='
    ;

yieldExpression
    : Yield expression ('=>' expression)? {SetVersion(Php.V55);}
    | YieldFrom expression {SetVersion(Php.V7);}
    ;

arrayItemList
    : arrayItem (',' arrayItem)* ','?
    ;

arrayItem
    : expression ('=>' expression)?
    | (expression '=>')? '&' chain
    ;

lambdaFunctionUseVars
    : Use '(' lambdaFunctionUseVar (',' lambdaFunctionUseVar)* ')'
    ;

lambdaFunctionUseVar
    : '&'? VarName
    ;

qualifiedStaticTypeRef
    : qualifiedNamespaceName genericDynamicArgs?
    | Static
    ;

typeRef
    : (qualifiedNamespaceName | indirectTypeRef) genericDynamicArgs?
    | primitiveType
    | Static
    | anonymousClass
    ;

anonymousClass
    : attributes Private? modifier? (
      classEntryType typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)?
    | Interface id typeParameterListInBrackets? (Extends interfaceList)? )
      OpenCurlyBracket classStatement* CloseCurlyBracket
    ;

indirectTypeRef
    : chainBase ('->' keyedFieldName)*
    ;

qualifiedNamespaceName
    : Namespace? '\\'? namespaceNameList
    ;

namespaceNameList
    : id ('\\' id)* ('\\' namespaceNameTail)?
    ;

namespaceNameTail
    : id (As id)?
    | OpenCurlyBracket namespaceNameTail (','namespaceNameTail)* ','? CloseCurlyBracket
    ;

qualifiedNamespaceNameList
    : qualifiedNamespaceName (',' qualifiedNamespaceName)*
    ;

arguments
    : '(' ( actualArgument (',' actualArgument)* | yieldExpression)? ','? ')'
    ;

actualArgument
    : '...'? expression
    | '&' chain
    ;

constantInititalizer
    : constant
    | string
    | Array '(' (arrayItemList ','?)? ')'
    | '[' (arrayItemList ','?)? ']'
    | ('+' | '-') constantInititalizer
    ;

constant
    : Null
    | literalConstant
    | magicConstant
    | classConstant
    | qualifiedNamespaceName
    ;

literalConstant
    : Real
    | True
    | False
    | numericConstant
    | stringConstant
    ;

numericConstant
    : Octal
    | Decimal
    | Hex
    | Binary
    ;

classConstant
    : (qualifiedStaticTypeRef | keyedVariable | string | Class) '::' (entityId | keyedVariable) // 'foo'::$bar works in php7
    ;

stringConstant
    : Id
    ;

string
    : StartHereDoc HereDocText* HereDocEnd
    | StartNowDoc HereDocText* HereDocEnd
    | SingleQuoteString
    | DoubleQuote interpolatedStringPart* DoubleQuote
    ;

interpolatedStringPart
    : StringPart
    | UnicodeEscape
    | chain
    ;

chainList
    : chain (',' chain)*
    ;

chain
    : chainOrigin memberAccess*
    ;

chainOrigin
    : chainBase
    | functionCall
    | '(' newExpr ')'
    ;

memberAccess
    : '->' keyedFieldName actualArguments?
    ;

functionCall
    : functionCallName actualArguments
    ;

functionCallName
    : qualifiedNamespaceName
    | classConstant
    | chainBase
    | parentheses
    ;

actualArguments
    : genericDynamicArgs? arguments squareCurlyExpression*
    ;

chainBase
    : keyedVariable ('::' keyedVariable)?
    | qualifiedStaticTypeRef '::' keyedVariable
    ;

keyedFieldName
    : keyedSimpleFieldName
    | keyedVariable
    ;

keyedSimpleFieldName
    : (entityId | OpenCurlyBracket expression CloseCurlyBracket) squareCurlyExpression*
    ;

keyedVariable
    : Dollar* (VarName | Dollar OpenCurlyBracket expression CloseCurlyBracket) squareCurlyExpression*
    ;

squareCurlyExpression
    : '[' expression? ']'
    | OpenCurlyBracket expression CloseCurlyBracket
    ;

assignmentList
    : assignmentListElement? (',' assignmentListElement?)*
    ;

assignmentListElement
    : chain
    | List '('  assignmentList ')'
    | arrayItem
    ;

modifier
    : Abstract
    | Final
    ;

entityId
    : id
    | {CheckVersion(Php.V7)}?
    (// HaltCompiler
      Abstract
    | And
    | Array
    | As
    | Break
    | Callable
    | Case
    | Catch
    | Class
    | Clone
    | Const
    | Continue
    | Declare
    | Default
    | Die
    | Do
    | Echo
    | Else
    | ElseIf
    | Empty
    | EndDeclare
    | EndFor
    | EndForeach
    | EndIf
    | EndSwitch
    | EndWhile
    | Eval
    | Exit
    | Extends
    | Final
    | Finally
    | Fn
    | For
    | Foreach
    | Function
    | Global
    | Goto
    | If
    | Implements
    | Include
    | IncludeOnce
    | InstanceOf
    | InsteadOf
    | Interface
    | IsSet
    | List
    | Namespace
    | New
    | Or
    | Print
    | Private
    | Protected
    | Public
    | Require
    | RequireOnce
    | Return
    | Static
    | Switch
    | Throw
    | Trait
    | Try
    | Unset
    | Use
    | Var
    | While
    | Xor
    | Yield

    | Class__
    | Dir__
    | File__
    | Function__
    | Line__
    | Method__
    | Namespace__
    | Trait__

    | Int
    | Float
    | Bool
    | String
    | True
    | False
    | Null
    | Void
    | Iterable
    | Object

    ) {SetVersion(Php.V7);}
    ;

id
    : Id
    | Resource
    | Mixed
    | Numeric
    ;

memberModifier
    : Public
    | Protected
    | Private
    | Static
    | Abstract
    | Final
    ;

magicConstant
    : Line__
    | File__
    | Dir__
    | Function__
    | Class__
    | Trait__
    | Method__
    | Namespace__
    ;

primitiveType
    : Bool
    | Int
    | Float
    | String
    | Resource
    | Object
    | Array
    ;

castOperation
    : {MatchInt()}? int=(Int | Id)
    | {MatchBool()}? bool=(Bool | Id)
    | {MatchFloat()}? float=(Float | Id)
    | {Next("string")}? string_=(String | Id)
    | Array
    | {Next("object")}? object=(Object | Id)
    | Unset
    | {CheckVersion(Php.V521)}? {Next("binary")}? Id {SetVersion(Php.V521);}
    ;
