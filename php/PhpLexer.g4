/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2019, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Thierry Marianne (thierry.marianne@weaving-the-web.org)
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

lexer grammar PhpLexer;

channels { PhpComments, ErrorLexem, SkipChannel }

tokens { HereDocEnd }

options {
    superClass=PhpLexerBase;
}

SeaWhitespace:  [ \t\r\n]+ -> channel(HIDDEN);
HtmlText:       ~[<#]+;
XmlStart:       '<?xml' -> pushMode(XML);
PHPStartEcho:   PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStart:       PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlScriptOpen: '<script' { _scriptTag = true; } -> pushMode(INSIDE);
HtmlStyleOpen:  '<style' { _styleTag = true; } -> pushMode(INSIDE);
HtmlComment:    '<!--' .*? '-->' -> channel(HIDDEN);
HtmlDtd:        '<!' .*? '>';
HtmlOpen:       '<' -> pushMode(INSIDE);
Shebang
    : '#' { this.IsNewLineOrStart(-2) }? '!' ~[\r\n]*
    ;
NumberSign:     '#' ~'<'* -> more;
Error:          .         -> channel(ErrorLexem);

// TODO: parse xml attributes.
mode XML;

XmlText:                  ~'?'+;
XmlClose:                 '?>' -> popMode;
XmlText2:                 '?' -> type(XmlText);

mode INSIDE;

PHPStartEchoInside: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInside:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlClose: '>' { this.PushModeOnHtmlClose(); };
HtmlSlashClose: '/>' -> popMode;
HtmlSlash:      '/';
HtmlEquals:     '=';

HtmlStartQuoteString:       '\\'? '\'' -> pushMode(HtmlQuoteStringMode);
HtmlStartDoubleQuoteString: '\\'? '"'  -> pushMode(HtmlDoubleQuoteStringMode);
HtmlHex:                    '#' HexDigit+ ;
HtmlDecimal:                Digit+;
HtmlSpace:                  [ \t\r\n]+ -> channel(HIDDEN);
HtmlName:                   HtmlNameStartChar HtmlNameChar*;
ErrorInside:                .          -> channel(ErrorLexem);

mode HtmlQuoteStringMode;

PHPStartEchoInsideQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideQuoteString:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlEndQuoteString:            '\'' '\''? -> popMode;
HtmlQuoteString:               ~[<']+;
ErrorHtmlQuote:                .          -> channel(ErrorLexem);

mode HtmlDoubleQuoteStringMode;

PHPStartEchoDoubleQuoteString: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartDoubleQuoteString:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
HtmlEndDoubleQuoteString:      '"' '"'? -> popMode;
HtmlDoubleQuoteString:         ~[<"]+;
ErrorHtmlDoubleQuote:          .          -> channel(ErrorLexem);

// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/javascript if necessary.
// Php blocks can exist inside Script blocks too.
mode SCRIPT;

ScriptText:               ~'<'+;
// TODO: handle JS strings, but handle <?php tags inside them
//ScriptString:             '"' (~'"' | '\\' ('\r'? '\n' | .))* '"' -> type(ScriptText);
//ScriptString2:            '\'' (~'\'' | '\\' ('\r'? '\n' | .))* '\'' -> type(ScriptText);
HtmlScriptClose:          '</' 'script'? '>' -> popMode;
PHPStartInsideScriptEcho: PhpStartEchoFragment -> type(Echo), pushMode(PHP);
PHPStartInsideScript:     PhpStartFragment -> channel(SkipChannel), pushMode(PHP);
ScriptText2:              '<' -> type(ScriptText);

mode STYLE;

StyleBody: .*? '</' 'style'? '>' -> popMode;

mode PHP;

PHPEnd:             ('?' | '%' {this.HasAspTags()}?) '>'
      |             '</script>' {this.HasPhpScriptTag()}?;
Whitespace:         [ \t\r\n]+ -> channel(SkipChannel);
MultiLineComment:   '/*' .*? '*/' -> channel(PhpComments);
SingleLineComment:  '//' -> channel(SkipChannel), pushMode(SingleLineCommentMode);
ShellStyleComment:  '#' -> channel(SkipChannel), pushMode(SingleLineCommentMode);

// Reserved keywords, https://www.php.net/manual/en/reserved.keywords.php

HaltCompiler:       '__halt_compiler';
Abstract:           'abstract';
And:                'and';
Array:              'array';
As:                 'as';
Break:              'break';
Callable:           'callable' {CheckVersion(Php.V54)}?;
Case:               'case';
Catch:              'catch';
Class:              'class';
Clone:              'clone';
Const:              'const';
Continue:           'continue';
Declare:            'declare';
Default:            'default';
Die:                'die';
Do:                 'do';
Echo:               'echo';
Else:               'else';
ElseIf:             'elseif';
Empty:              'empty';
EndDeclare:         'enddeclare';
EndFor:             'endfor';
EndForeach:         'endforeach';
EndIf:              'endif';
EndSwitch:          'endswitch';
EndWhile:           'endwhile';
Eval:               'eval';
Exit:               'exit';
Extends:            'extends';
Final:              'final';
Finally:            'finally' {CheckVersion(Php.V55)}?;
Fn:                 'fn' {CheckVersion(Php.V74)}?;
For:                'for';
Foreach:            'foreach';
Function:           'function';
Global:             'global';
Goto:               'goto' {CheckVersion(Php.V53)}?;
If:                 'if';
Implements:         'implements';
Include:            'include';
IncludeOnce:        'include_once';
InstanceOf:         'instanceof';
InsteadOf:          'insteadof' {CheckVersion(Php.V54)}?;
Interface:          'interface';
IsSet:              'isset';
List:               'list';
Namespace:          'namespace' {CheckVersion(Php.V53)}?;
New:                'new';
Or:                 'or';
Print:              'print';
Private:            'private';
Protected:          'protected';
Public:             'public';
Require:            'require';
RequireOnce:        'require_once';
Return:             'return';
Static:             'static';
Switch:             'switch';
Throw:              'throw';
Trait:              'trait' {CheckVersion(Php.V54)}?;
Try:                'try';
Unset:              'unset';
Use:                'use';
Var:                'var';
While:              'while';
Xor:                'xor';
Yield:              'yield' {CheckVersion(Php.V55)}?;
YieldFrom:          'yield' Space+ 'from' {CheckVersion(Php.V7)}?;

// Compile-time constants

Class__:            '__class__';
Dir__:              '__dir__' {CheckVersion(Php.V53)}?;
File__:             '__file__';
Function__:         '__function__';
Line__:             '__line__';
Method__:           '__method__';
Namespace__:        '__namespace__' {CheckVersion(Php.V53)}?;
Trait__:            '__trait__' {CheckVersion(Php.V54)}?;

// https://www.php.net/manual/en/reserved.other-reserved-words.php

Int:                'int' {CheckVersion(Php.V7)}?;
Float:              'float' {CheckVersion(Php.V7)}?;
Bool:               'bool' {CheckVersion(Php.V7)}?;
String:             'string' {CheckVersion(Php.V7)}?;
True:               'true' {CheckVersion(Php.V7)}?;
False:              'false' {CheckVersion(Php.V7)}?;
Null:               'null' {CheckVersion(Php.V7)}?;
Void:               'void' {CheckVersion(Php.V71)}?;
Iterable:           'iterable' {CheckVersion(Php.V71)}?;
Object:             'object' {CheckVersion(Php.V72)}?;

// Soft reserved words

Resource:           'resource' {CheckVersion(Php.V7)}?;
Mixed:              'mixed' {CheckVersion(Php.V7)}?;
Numeric:            'numeric' {CheckVersion(Php.V7)}?;

// Operators

Spaceship:          '<=>';
Lgeneric:           '<:';
Rgeneric:           ':>';
DoubleArrow:        '=>';
Inc:                '++';
Dec:                '--';
IsIdentical:        '===';
IsNoidentical:      '!==';
IsEqual:            '==';
IsNotEq:            '<>'
       |            '!=';
IsSmallerOrEqual:   '<=';
IsGreaterOrEqual:   '>=';
PlusEqual:          '+=';
MinusEqual:         '-=';
MulEqual:           '*=';
Pow:                '**';
PowEqual:           '**=';
DivEqual:           '/=';
Concaequal:         '.=';
ModEqual:           '%=';
ShiftLeftEqual:     '<<=';
ShiftRightEqual:    '>>=';
AndEqual:           '&=';
OrEqual:            '|=';
XorEqual:           '^=';
BooleanOr:          '||';
BooleanAnd:         '&&';

NullCoalescing:     '??';
NullCoalescingEqual:'??=';

ShiftLeft:          '<<';
ShiftRight:         '>>';
DoubleColon:        '::';
ObjectOperator:     '->';
NamespaceSeparator: '\\';
Ellipsis:           '...';
Less:               '<';
Greater:            '>';
Ampersand:          '&';
Pipe:               '|';
Bang:               '!';
Caret:              '^';
Plus:               '+';
Minus:              '-';
Asterisk:           '*';
Percent:            '%';
Divide:             '/';
Tilde:              '~';
SuppressWarnings:   '@';
Dollar:             '$';
Dot:                '.';
QuestionMark:       '?';
OpenRoundBracket:   '(';
CloseRoundBracket:  ')';
OpenSquareBracket:  '[';
CloseSquareBracket: ']';
OpenCurlyBracket:   '{';
CloseCurlyBracket:  '}'
{ this.PopModeOnCurlyBracketClose(); };
Comma:              ',';
Colon:              ':';
SemiColon:          ';';
Eq:                 '=';
Quote:              '\'';
BackQuote:          '`';

VarName:            '$' NameString;
Id:                 [a-z_][a-z_0-9]*;
Decimal:            '0'
       |            [1-9] Digit* DigitPart*;
Hex:                '0x' HexDigit+ ('_' HexDigit+)*;
Octal:              '0' [0-7]+ ('_' [0-7]+)*;
Binary:             '0b' [01]+ ('_' [01]+)*;
Real:               (Digit+ DigitPart* '.' Digit* DigitPart* | '.' Digit+ DigitPart*) ExponentPart?
    |               Digit+ DigitPart* ExponentPart;

fragment DigitPart:     '_' Digit+;
fragment ExponentPart:  'e' [+-]? Digit+;
fragment Digit:         [0-9];
fragment HexDigit:      [a-f0-9];

BackQuoteString:   '`' ~'`'* '`';
SingleQuoteString: '\'' (~('\'' | '\\') | '\\' . )* '\'';
DoubleQuote:       '"' -> pushMode(InterpolationString);

StartNowDoc
    : '<<<' [ \t]* '\'' NameString '\'' '\r'? '\n' -> pushMode(HereDoc)
    ;
StartHereDoc
    : '<<<' [ \t]* NameString '\r'? '\n'           -> pushMode(HereDoc)
    ;
ErrorPhp:                   .          -> channel(ErrorLexem);

mode InterpolationString;

VarNameInInterpolation:     '$' NameString                                      -> type(VarName); // TODO: fix such cases: "$people->john"
DollarString:               '$'                                                 -> type(StringPart);
CurlyDollar:                '{' { this.IsCurlyDollar(1) }? { this.SetInsideString(); }  -> channel(SkipChannel), pushMode(PHP);
CurlyString:                '{'                                                 -> type(StringPart);
EscapedChar:                '\\' .                                              -> type(StringPart);
DoubleQuoteInInterpolation: '"'                                                 -> type(DoubleQuote), popMode;
UnicodeEscape:              '\\u{' [a-zA-Z0-9][a-zA-Z0-9]+ '}';
StringPart:                 ~[${\\"]+;

mode SingleLineCommentMode;

Comment:                 ~[\r\n?]+ -> channel(PhpComments);
PHPEndSingleLineComment: '?' '>';
CommentQuestionMark:     '?' -> type(Comment), channel(PhpComments);
CommentEnd:              [\r\n] -> channel(SkipChannel), popMode; // exit from comment.

mode HereDoc;  // TODO: interpolation for heredoc strings.

HereDocText    : ~[\r\n;]+;
HereDocSemi    : (';' | '\r'? '\n') -> type(HereDocText);

// fragments.
// '<?=' will be transformed to 'echo' token.
// '<?= "Hello world"; ?>' will be transformed to '<?php echo "Hello world"; ?>'
fragment PhpStartEchoFragment: '<' ('?' '=' | { this.HasAspTags() }? '%' '=');
fragment PhpStartFragment:     '<' ('?' 'php'? | { this.HasAspTags() }? '%');
fragment NameString: [a-zA-Z_\u0080-\ufffe][a-zA-Z0-9_\u0080-\ufffe]*;
fragment HtmlNameStartChar
    : [:a-z]
    | '\u2070'..'\u218F'
    | '\u2C00'..'\u2FEF'
    | '\u3001'..'\uD7FF'
    | '\uF900'..'\uFDCF'
    | '\uFDF0'..'\uFFFD'
    ;
fragment HtmlNameChar
    : HtmlNameStartChar
    | '-'
    | '_'
    | '.'
    | Digit
    | '\u00B7'
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;
fragment Space:                [ \t\r\n]+;