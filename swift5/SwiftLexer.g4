/*
 * [The "BSD license"]
 Copyright (c) 2014 Terence Parr
 All rights reserved.

 Redistribution and use in source and
 * binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 1.
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following
 * disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions
 * and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 3. The
 * name of the author may not be used to endorse or promote products
 derived from this software without specific prior
 * written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE

 Converted from Apple's doc, http://tinyurl.com/n8rkoue, to
 * ANTLR's
 meta-language.
 */

lexer grammar SwiftLexer;

options{ superClass=SwiftBaseLexer; }

// Tested all alphanumeric tokens in playground.
// E.g. "let mutating = 1".
// E.g. "func mutating() {}".
//
// In source code of swift there are multiple cases of error diag::keyword_cant_be_identifier.
// Maybe it is not even a single error when keyword can't be identifier.
//

// Keywords in declarations/labels
PROTOCOL_CAPITAL			:	'Protocol';
ALPHA						:	'alpha';
ARCH						:	'arch';
ARM							:	'arm';
ARM64						:	'arm64';
ASSIGNMENT					:	'assignment';
ASSOCIATIVITY				:	'associativity';
BLUE						:	'blue';
CONVENIENCE					:	'convenience';
DIDSET						:	'didSet';
DYNAMIC						:	'dynamic';
FILE						:	'file';
FINAL						:	'final';
GET							:	'get';
GREEN						:	'green';
HIGHERTHAN					:	'higherThan';
I386						:	'i386';
IOS							:	'iOS';
IOSAPPLICATIONEXTENSION		:	'iOSApplicationExtension';
INDIRECT					:	'indirect';
INFIX						:	'infix';
LAZY						:	'lazy';
LEFT						:	'left';
LINE						:	'line';
LOWERTHAN					:	'lowerThan';
MACOS						:	'macOS';
OSX                         :   'OSX';
MACOSAPPLICATIONEXTENSION	:	'macOSApplicationExtension';
MUTATING					:	'mutating';
NONE						:	'none';
NONMUTATING					:	'nonmutating';
OF							:	'of';
OPTIONAL					:	'optional';
OS							:	'os';
OVERRIDE					:	'override';
POSTFIX						:	'postfix';
PRECEDENCE					:	'precedence';
PREFIX						:	'prefix';
RED							:	'red';
REQUIRED					:	'required';
RESOURCENAME				:	'resourceName';
RIGHT						:	'right';
SAFE						:	'safe';
SET							:	'set';
SWIFT						:	'swift';
TVOS						:	'tvOS';
UNOWNED						:	'unowned';
UNSAFE						:	'unsafe';
WATCHOS						:	'watchOS';
WEAK						:	'weak';
WILLSET						:	'willSet';
X86_64						:	'x86_64';
PRECEDENCEGROUP				:	'precedencegroup';

// Declarations and Type Keywords

ASSOCIATED_TYPE	:	'associatedtype';
CLASS			:	'class';
DEINIT			:	'deinit';
DESTRUCTOR		:	'destructor';
ENUM			:	'enum';
EXTENSION		:	'extension';
FILEPRIVATE		:	'fileprivate';
FUNC			:	'func';
IMPORT			:	'import';
INIT			:	'init';
INOUT			:	'inout';
INTERNAL		:	'internal';
LET				:	'let';
OPEN			:	'open';
OPERATOR		:	'operator';
PRIVATE			:	'private';
PROTOCOL		:	'protocol';
PUBLIC			:	'public';
STATIC			:	'static';
STRUCT			:	'struct';
SUBSCRIPT		:	'subscript';
TYPE_CAPITAL	:	'Type';
TYPEALIAS		:	'typealias';
VAR				:	'var';

// Statements Keywords
BREAK		:	'break';
CASE		:	'case';
CONTINUE	:	'continue';
DEFAULT		:	'default';
DEFER		:	'defer';
DO			:	'do';
ELSE		:	'else';
FALLTHROUGH	:	'fallthrough';
FOR			:	'for';
GUARD		:	'guard';
IF			:	'if';
IN			:	'in';
REPEAT		:	'repeat';
RETURN		:	'return';
SWITCH		:	'switch';
THEN		:	'then';
WHERE		:	'where';
WHILE		:	'while';

ANY				:	'any';
AS				:	'as';
CATCH			:	'catch';
IS				:	'is';
NEW				:	'new';
RETHROWS		:	'rethrows';
SELF_CAPITAL	:	'Self';
SELF_LOWER		:	'self';
SUPER			:	'super';
THROW			:	'throw';
THROWS			:	'throws';
TRY				:	'try';
TYPE_LOWER		:	'type';

// Directives Keywords

AVAILABLE_DIRECTIVE			:	'#available';
COLUMN_DIRECTIVE			:	'#column';
COLOR_DIRECTIVE				:	'#colorLiteral';
DSOHANDLE_DIRECTIVE			:	'#dsohandle';
ELSEIF_DIRECTIVE			:	'#elseif';
ELSE_DIRECTIVE				:	'#else';
ENDIF_DIRECTIVE				:	'#endif';
FILELITERAL_DIRECTIVE		:	'#fileLiteral';
IMAGE_DIRECTIVE				:	'#imageLiteral';
IF_DIRECTIVE				:	'#if';
FILE_DIRECTIVE				:	'#file';
FUNCTION_DIRECTIVE			:	'#function';
LINE_DIRECTIVE				:	'#line';
KEYPATH_DIRECTIVE			:	'#keyPath';
SELECTOR_DIRECTIVE			:	'#selector';
SOURCE_LOCATION_DIRECTIVE	:	'#sourceLocation';

TRUE	:	'true';
FALSE	:	'false';

// Operators

LT			:	'<';
GT			:	'>';
BANG		:	'!';
QUESTION	:	'?';
AT			:	'@';
AND			:	'&';
SUB			:	'-';
ASSIGN		:	'=';
OR			:	'|';
DIV			:	'/';
ADD			:	'+';
MUL			:	'*';
MOD			:	'%';
CARET		:	'^';
TILDE		:	'~';
COLON		:	':';

// Punctuation

DOT			:	'.';
LCURLY		:	'{';
LPAREN		:	'(' { this.CheckIfInsideInterpolatedExpression(); };
LBRACK		:	'[';
RCURLY		:	'}';
RPAREN		:	')' { this.PopModeOnInterpolatedExpressionClose(); };
RBRACK		:	']';
COMMA		:	',';
SEMI		:	';';
BACKTICK	:	'`';
SHARP		:	'#';

UNDERSCORE	:	'_';

// Attributes
GETTER_ATTR	:	'getter:';
SETTER_ATTR	:	'setter:';

UNOWNED_SAFE	:	'unowned(safe)';
UNOWNED_UNSAFE	:	'unowned(unsafe)';

// Hidden tokens

COMMENT			:	'/*' (COMMENT | .)*? '*/'		-> channel(HIDDEN);
LINE_COMMENT	:	'//' ~[\r\n]*					-> channel(HIDDEN);
TERMINATOR		:	[\r\n]+							-> channel(HIDDEN);
WS				:	[ \n\r\t\u000B\u000C\u0000]+	-> channel(HIDDEN);

IDENTIFIER_TOKEN:
    IDENTIFIER_HEAD IDENTIFIER_CHARACTERS?
    |'`' IDENTIFIER_HEAD IDENTIFIER_CHARACTERS? '`'
	| '$' PURE_DECIMAL_DIGIT+
	;

// Number literals
BINARY_LIT			:	'0b' PURE_BINARY_DIGIT BINARY_DIGIT*;
DECIMAL_LIT			:	'0'? PURE_DECIMAL_DIGIT DECIMAL_DIGIT*;
OCTAL_LIT			:	'0o' PURE_OCTAL_DIGIT OCTAL_DIGIT*;
HEX_LIT				:	'0x' PURE_HEX_DIGIT HEX_DIGIT*;
PURE_DECIMAL_DIGITS	:	PURE_DECIMAL_DIGIT+;

FLOAT_LIT:
	DECIMAL_LIT DECIMAL_FRACTION? DECIMAL_EXPONENT?
	| HEX_LIT HEX_FRACTION? HEX_EXPONENT
	;

NIL_LIT : 'nil';

Platform_name_platform_version : Platform_name WS Platform_version;

Operator_head_other: // valid operator chars not used by Swift itself
	[\u00A1-\u00A7]
	| [\u00A9\u00AB]
	| [\u00AC\u00AE]
	| [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
	| [\u2016-\u2017\u2020-\u2027]
	| [\u2030-\u203E]
	| [\u2041-\u2053]
	| [\u2055-\u205E]
	| [\u2190-\u23FF]
	| [\u2500-\u2775]
	| [\u2794-\u2BFF]
	| [\u2E00-\u2E7F]
	| [\u3001-\u3003]
	| [\u3008-\u3030]
	;

Operator_following_character:
	[\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE00-\uFE0F]
	| [\uFE20-\uFE2F]
	//| [\uE0100-\uE01EF]  ANTLR can't do >16bit char
	;

Implicit_parameter_name : '$' DECIMAL_LIT;

DOUBLE_QUOTE: '"' -> pushMode(InterpolationString);

TRIPLE_QUOTE: '"""' -> pushMode(Multiline);

mode InterpolationString;

INTERPOLATION_EXPRESSION: '\\(' { this.SetInsideString(); } -> channel(HIDDEN), pushMode(DEFAULT_MODE);
CLOSE_STRING: '"' -> type(DOUBLE_QUOTE), popMode;
STRING_PART: ESCAPED_VALUE+;

mode Multiline;
MULTILINE_INTERPOLATION_EXPRESSION: '\\(' { this.SetInsideString(); } -> channel(HIDDEN), pushMode(DEFAULT_MODE);
ESCAPED_BACKSLASH: '\\' -> type(MULTILINE_PART);
ESCAPED_TRIPLE_QUOTE: '\\"""' -> type(MULTILINE_PART);
TRIPLE_QUOTE_CLOSE: QUOTE? '"""' -> type(TRIPLE_QUOTE), popMode;
QUOTE : '"'+ -> type(MULTILINE_PART);
MULTILINE_PART: (ESCAPED_VALUE | '\n' | '\r' | '\t')+;

// Fragments

fragment Platform_name
    : 'iOS'
	| 'iOSApplicationExtension'
	| 'macOS'
	| 'macOSApplicationExtension'
	| 'watchOS'
	| 'tvOS'
	| 'OSX'
	;

fragment Platform_version:
	PURE_DECIMAL_DIGITS
	| PURE_DECIMAL_DIGITS '.' PURE_DECIMAL_DIGITS
	| PURE_DECIMAL_DIGITS '.' PURE_DECIMAL_DIGITS '.' PURE_DECIMAL_DIGITS
	;

fragment DECIMAL_EXPONENT : FLOATING_POINT_E SIGN? DECIMAL_LIT;

fragment DECIMAL_FRACTION : DOT DECIMAL_LIT;

fragment HEX_EXPONENT : FLOATING_POINT_P SIGN? DECIMAL_LIT;

fragment HEX_FRACTION : '.' PURE_HEX_DIGIT HEX_DIGIT*;

fragment SIGN : ADD | SUB;

fragment FLOATING_POINT_E : 'e' | 'E';

fragment FLOATING_POINT_P : 'p' | 'P';

fragment ESCAPED_VALUE:
	'\\' [0\\tnr"']
	| '\\x' HEX_DIGIT HEX_DIGIT
	| '\\u' '{' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT '}'
	| '\\u' '{' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT '}'
	| ~["\n\r\\]
	;

fragment IDENTIFIER_CHARACTER:
	PURE_DECIMAL_DIGIT
	| [\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE20-\uFE2F]
	| IDENTIFIER_HEAD
	;

fragment PURE_BINARY_DIGIT	:	[01];
fragment BINARY_DIGIT		:	PURE_BINARY_DIGIT | '_';

fragment PURE_DECIMAL_DIGIT	:	[0-9];
fragment DECIMAL_DIGIT		:	[0-9] | '_';

fragment PURE_HEX_DIGIT	:	[0-9a-fA-F];
fragment HEX_DIGIT		:	PURE_HEX_DIGIT | '_';

fragment PURE_OCTAL_DIGIT	:	[0-7];
fragment OCTAL_DIGIT		:	PURE_OCTAL_DIGIT | '_';

fragment IDENTIFIER_HEAD:
	[a-zA-Z]
	| '_'
	| '\u00A8'
	| '\u00AA'
	| '\u00AD'
	| '\u00AF'
	| [\u00B2-\u00B5]
	| [\u00B7-\u00BA]
	| [\u00BC-\u00BE]
	| [\u00C0-\u00D6]
	| [\u00D8-\u00F6]
	| [\u00F8-\u00FF]
	| [\u0100-\u02FF]
	| [\u0370-\u167F]
	| [\u1681-\u180D]
	| [\u180F-\u1DBF]
	| [\u1E00-\u1FFF]
	| [\u200B-\u200D]
	| [\u202A-\u202E]
	| [\u203F-\u2040]
	| '\u2054'
	| [\u2060-\u206F]
	| [\u2070-\u20CF]
	| [\u2100-\u218F]
	| [\u2460-\u24FF]
	| [\u2776-\u2793]
	| [\u2C00-\u2DFF]
	| [\u2E80-\u2FFF]
	| [\u3004-\u3007]
	| [\u3021-\u302F]
	| [\u3031-\u303F]
	| [\u3040-\uD7FF]
	| [\uF900-\uFD3D]
	| [\uFD40-\uFDCF]
	| [\uFDF0-\uFE1F]
	| [\uFE30-\uFE44]
	| [\uFE47-\uFFFD]
	/*
	 | U+10000-U+1FFFD | U+20000-U+2FFFD | U+30000-U+3FFFD | U+40000-U+4FFFD
 | U+50000-U+5FFFD |
 U+60000-U+6FFFD
 |
	 U+70000-U+7FFFD | U+80000-U+8FFFD
 | U+90000-U+9FFFD | U+A0000-U+AFFFD |
 U+B0000-U+BFFFD | U+C0000-U+CFFFD
 |
	 U+D0000-U+DFFFD or U+E0000-U+EFFFD
	 */
	;

fragment IDENTIFIER_CHARACTERS : IDENTIFIER_CHARACTER+;