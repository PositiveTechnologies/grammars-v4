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

parser grammar SwiftParser;

options {
	tokenVocab = SwiftLexer;
	superClass = SwiftBaseParser;
}

top_level : statements? EOF;

statements : (statement eos?)+;
/* statements : statements_impl[-1];

 statements_impl[int indexBefore]
 locals[
 int indexAfter = -1
 ]:
 {SwiftBaseParser.isSeparatedStatement(_input, $indexBefore)}? statement {$indexAfter = _input.Index;}
 statements_impl[$indexAfter]?
 ;
 */
// code_block : '{' statements? '}';

// Statements
statement: (
		expression
		| declaration
		| loop_statement
		| if_statement
		| guard_statement
		| switch_statement
		| labeled_statement
		| break_statement
		| continue_statement
		| fallthrough_statement
		| return_statement
		| throw_statement
		| defer_statement
		| do_statement
		| compiler_control_statement
	) eos?
	;
// proper logic with semicolons is not supported yet. compiler_control_statement should be separated with a newline, but not with a semicolon

// GRAMMAR OF A LOOP STATEMENT

loop_statement:
	for_statement
	| for_in_statement
	| while_statement
	| repeat_while_statement
	;

// GRAMMAR OF A FOR STATEMENT
// Deprecated since Swift3

for_statement:
	FOR for_init? ';' expression? ';' expression? code_block
	| FOR LPAREN for_init? ';' expression? ';' expression? ')' code_block
	;

for_init : variable_declaration | expression_list;

// GRAMMAR OF A FOR_IN STATEMENT

for_in_statement:
	'for' 'case'? pattern 'in' expression where_clause? code_block
	;

// GRAMMAR OF A WHILE STATEMENT

while_statement : 'while' condition_list code_block;

condition_list : condition (',' condition)*;

condition:
	expression
	| availability_condition
	| case_condition
	| optional_binding_condition
	;

case_condition : 'case' pattern initializer where_clause?;

optional_binding_condition:
	'let' pattern initializer
	| 'var' pattern initializer
	;

// GRAMMAR OF A REPEAT-WHILE STATEMENT

repeat_while_statement : 'repeat' code_block 'while' expression;

// GRAMMAR OF A BRANCH STATEMENT

// GRAMMAR OF AN IF STATEMENT

if_statement	:	'if' condition_list code_block ('else' (code_block | if_statement))?;

// GRAMMAR OF A GUARD STATEMENT

guard_statement : 'guard' condition_list 'else' code_block;

// GRAMMAR OF A SWITCH STATEMENT

switch_statement	:	'switch' expression '{' switch_cases? '}';
switch_cases		:	switch_case switch_cases?;
switch_case			:	case_label statements | default_label statements;
case_label			:	'case' case_item_list ':';
case_item_list:
	pattern where_clause?
	| pattern where_clause? ',' case_item_list
	;
default_label		:	'default' ':';
where_clause		:	'where' where_expression;
where_expression	:	expression;

// GRAMMAR OF A LABELED STATEMENT

labeled_statement : declaration_identifier ':'  ( loop_statement
	                                            | if_statement
	                                            | switch_statement
	                                            | do_statement
                                                );

// GRAMMAR OF A BREAK STATEMENT

break_statement : 'break' declaration_identifier?;

// GRAMMAR OF A CONTINUE STATEMENT

continue_statement : 'continue' declaration_identifier?;

// GRAMMAR OF A FALLTHROUGH STATEMENT

fallthrough_statement : 'fallthrough';

// GRAMMAR OF A RETURN STATEMENT

return_statement : 'return' expression?;

// GRAMMAR OF A THROW STATEMENT
throw_statement : 'throw' expression;

// GRAMMAR OF A DEFER STATEMENT

defer_statement : 'defer' code_block;

// GRAMMAR OF A DO STATEMENT

do_statement	:	'do' code_block catch_clause*;
catch_clause	:	'catch' pattern? where_clause? code_block;

// GRAMMAR OF A COMPILER CONTROL STATEMENT

compiler_control_statement:
	conditional_compilation_block
	| line_control_statement
	;

// GRAMMAR OF A CONDITIONAL COMPILATION BLOCK

conditional_compilation_block:
	if_directive_clause elseif_directive_clauses? else_directive_clause? endif_directive
	;

if_directive_clause : if_directive compilation_condition statements?;

elseif_directive_clauses	:	(elseif_directive_clause)+;
elseif_directive_clause		:	elseif_directive compilation_condition statements?;
else_directive_clause		:	else_directive statements?;

if_directive		:	'#if';
elseif_directive	:	'#elseif';
else_directive		:	'#else';
endif_directive		:	'#endif';

compilation_condition:
	platform_condition
	| label_identifier
	| booleanLiteral
	| LPAREN compilation_condition ')'
	| '!' compilation_condition
	| compilation_condition compilation_condition_AND compilation_condition
	| compilation_condition compilation_condition_OR compilation_condition
	;

platform_condition:
	'os' LPAREN operating_system ')'
	| 'arch' LPAREN architecture ')'
	| 'swift' LPAREN compilation_condition_GE swift_version ')'
	;

swift_version : DECIMAL_LIT '.' DECIMAL_LIT;

// Rule from docs: operating-system → macOS­ | iOS­ | watchOS­ | tvOS
//
// Code from apple/swift:­ #if (!os(Windows) || CYGWIN) && (arch(i386) || arch(x86_64))
//
// "#if os(Any)" gives error that Any is not an identifier. So I decided to use declaration_identifier
operating_system	:	declaration_identifier;
architecture		:	declaration_identifier;

// These rules don't work: operating_system : 'macOS' | 'iOS' | 'watchOS' | 'tvOS' ; architecture : 'i386' | 'x86_64' |
// 'arm' | 'arm64' ;

// GRAMMAR OF A LINE CONTROL STATEMENT

// NOTE: The rule is changed. Original rule:
//
// line_control_statement : '#sourceLocation' LPAREN 'file:' file_name ',' 'line:' line_number ')' | '#sourceLocation' LPAREN
// ')' ;
//
// This defines a token "file:", but this is valid for swift compiler: '#sourceLocation(file : "", line : 1)' (notice
// spaces between "file" and ":")
//
// Modified rule:
line_control_statement:
	'#sourceLocation' LPAREN 'file' ':' file_name ',' 'line' ':' line_number ')'
	| '#sourceLocation' LPAREN ')'
	;

line_number : integer;
// TODO: A decimal integer greater than zero
file_name : static_string_literal;

// GRAMMAR OF AN AVAILABILITY CONDITION

availability_condition : '#available' LPAREN availability_arguments ')';

availability_arguments : availability_argument (',' availability_argument)*;

availability_argument : Platform_name_platform_version | '*';

// Generic Parameters and Arguments

// GRAMMAR OF A GENERIC PARAMETER CLAUSE

generic_parameter_clause	:	'<' generic_parameter_list '>';
generic_parameter_list		:	generic_parameter (',' generic_parameter)*;
generic_parameter:
	type_name
	| type_name ':' type_identifier
	| type_name ':' protocol_composition_type
	;

generic_where_clause	:	'where' requirement_list;
requirement_list		:	requirement (',' requirement)*;
requirement				:	conformance_requirement | same_type_requirement;

conformance_requirement:
	type_identifier ':' type_identifier
	| type_identifier ':' protocol_composition_type
	;
same_type_requirement : type_identifier same_type_equals type;

// GRAMMAR OF A GENERIC ARGUMENT CLAUSE

generic_argument_clause	:	'<' generic_argument_list '>';
generic_argument_list	:	generic_argument (',' generic_argument)*;
generic_argument		:	type;

// context-sensitive. Allow < as pre, post, or binary op lt : {_input.LT(1).getText().equals("<")}? operator ; gt :
// {_input.LT(1).getText().equals(">")}? operator ; Declarations

// GRAMMAR OF A DECLARATION

declaration
    : import_declaration
	| constant_declaration
	| variable_declaration
	| typealias_declaration
	| function_declaration
	| enum_declaration
	| class_or_struct_declaration
	| protocol_declaration
	| initializer_declaration
	| deinitializer_declaration
	| extension_declaration
	| subscript_declaration
	| operator_declaration
	| operator_declaration
	| precedence_group_declaration
	;

class_or_struct_member_declaration
    : import_declaration
	| typealias_declaration
	| field_declaration
	| function_declaration
	| enum_declaration
	| class_or_struct_declaration
	| protocol_declaration
	| initializer_declaration
	| deinitializer_declaration
	| extension_declaration
	| subscript_declaration
	| operator_declaration
	| operator_declaration
	| precedence_group_declaration
	;

field_declaration
    : constant_declaration | variable_declaration;

// GRAMMAR OF A TOP-LEVEL DECLARATION

// top_level_declaration : statements? ;

// GRAMMAR OF A CODE BLOCK

code_block : '{' statements? '}';

// GRAMMAR OF AN IMPORT DECLARATION

import_declaration : attributes? 'import' import_kind? import_path;
import_kind:
	'typealias'
	| 'struct'
	| 'class'
	| 'enum'
	| 'protocol'
	| 'var'
	| 'func'
	;
import_path				:	import_path_identifier ('.' import_path_identifier)*;
import_path_identifier	:	declaration_identifier | operator;

// GRAMMAR OF A CONSTANT DECLARATION

constant_declaration:
	attributes? declaration_modifiers? 'let' pattern_initializer_list
	;
pattern_initializer_list : pattern_initializer (',' pattern_initializer)*;

/** rule is ambiguous. can match "var x = 1" with x as pattern
 OR with x as expression_pattern.
 ANTLR resolves in
 favor or first choice: pattern is x, 1 is initializer.
 */
pattern_initializer	:	pattern initializer?;
initializer			:	assignment_operator expression;

// GRAMMAR OF A VARIABLE DECLARATION

variable_declaration:
	variable_declaration_head ( variable_name type_annotation ( getter_setter_block
	                                                          | getter_setter_keyword_block
															  | initializer? willSet_didSet_block
															  | code_block)
	                          | ( variable_name initializer willSet_didSet_block
							    | pattern_initializer_list)
	                          );

variable_declaration_head	:	attributes? declaration_modifiers? 'var';
variable_name				:	declaration_identifier;

getter_setter_block:
	'{' getter_clause setter_clause? '}'
	| '{' setter_clause getter_clause '}'
	;
getter_clause	:	attributes? mutation_modifier? 'get' code_block;
setter_clause	:	attributes? mutation_modifier? 'set' setter_name? code_block;
setter_name		:	LPAREN declaration_identifier ')';

getter_setter_keyword_block:
	'{' getter_keyword_clause setter_keyword_clause? '}'
	| '{' setter_keyword_clause getter_keyword_clause '}'
	;
getter_keyword_clause	:	attributes? mutation_modifier? 'get';
setter_keyword_clause	:	attributes? mutation_modifier? 'set';

willSet_didSet_block:
	'{' willSet_clause didSet_clause? '}'
	| '{' didSet_clause willSet_clause '}'
	;
willSet_clause	:	attributes? 'willSet' setter_name? code_block;
didSet_clause	:	attributes? 'didSet' setter_name? code_block;

// GRAMMAR OF A TYPE ALIAS DECLARATION

typealias_declaration:
	attributes? access_level_modifier? 'typealias' typealias_name generic_parameter_clause? typealias_assignment
	;
typealias_name			:	declaration_identifier;
typealias_assignment	:	assignment_operator type;

// GRAMMAR OF A FUNCTION DECLARATION
function_declaration:
	function_head function_name generic_parameter_clause? function_signature generic_where_clause? code_block
	;

function_head : attributes? declaration_modifiers? 'func';

function_name : declaration_identifier | operator;

function_signature : parameter_clause ('throws' | 'rethrows')? function_result?;

function_result : arrow_operator attributes? type;

parameter_clause	:	LPAREN parameter? (',' parameter)* ')';

parameter:
	external_parameter_name? local_parameter_name type_annotation default_argument_clause?
	| external_parameter_name? local_parameter_name type_annotation range_operator
	;
external_parameter_name : label_identifier;
// TODO: Check that deleting " | '_'" doesn't break anything
local_parameter_name : label_identifier;
// TODO: Check that deleting " | '_'" doesn't break anything
default_argument_clause : assignment_operator expression;

// GRAMMAR OF AN ENUMERATION DECLARATION

enum_declaration:
	attributes? access_level_modifier? union_style_enum
	| attributes? access_level_modifier? raw_value_style_enum
	;

union_style_enum:
	'indirect'? 'enum' enum_name generic_parameter_clause? type_inheritance_clause? generic_where_clause? '{'
		union_style_enum_members? '}'
	;

union_style_enum_members : union_style_enum_member union_style_enum_members?;

union_style_enum_member:
	declaration
	| union_style_enum_case_clause
	| compiler_control_statement
	;

union_style_enum_case_clause:
	attributes? 'indirect'? 'case' union_style_enum_case_list
	;

union_style_enum_case_list:
	union_style_enum_case
	| union_style_enum_case ',' union_style_enum_case_list
	;

union_style_enum_case : enum_case_name tuple_type?;

enum_name : declaration_identifier;

enum_case_name : declaration_identifier;

raw_value_style_enum:
	'enum' enum_name generic_parameter_clause? type_inheritance_clause generic_where_clause? '{'
		raw_value_style_enum_members '}'
	;

raw_value_style_enum_members:
	raw_value_style_enum_member raw_value_style_enum_members?
	;

raw_value_style_enum_member:
	declaration
	| raw_value_style_enum_case_clause
	| compiler_control_statement
	;

raw_value_style_enum_case_clause:
	attributes? 'case' raw_value_style_enum_case_list
	;

raw_value_style_enum_case_list: raw_value_style_enum_case (',' raw_value_style_enum_case)*;

raw_value_style_enum_case : enum_case_name raw_value_assignment?;

raw_value_assignment : assignment_operator raw_value_literal;

raw_value_literal : numeric_literal | static_string_literal | booleanLiteral;

// GRAMMAR OF A CLASS AND STRUCT DECLARATIONS

class_or_struct_declaration
    : attributes? access_level_modifier? ('final'? 'class' | 'struct') declaration_identifier generic_parameter_clause? type_inheritance_clause?
		generic_where_clause? class_body
	| attributes? access_level_modifier? 'final' access_level_modifier? 'class' declaration_identifier generic_parameter_clause?
		type_inheritance_clause? generic_where_clause? class_body
	;

class_body	: '{' ((class_or_struct_member_declaration | compiler_control_statement) SEMI?)* '}';

// GRAMMAR OF A PROTOCOL DECLARATION
// generic_where_clause has been erased since Swift 3

protocol_declaration : attributes? access_level_modifier? 'protocol' declaration_identifier type_inheritance_clause? generic_where_clause? protocol_body;
protocol_body	:	'{' ((protocol_member_declaration | compiler_control_statement) SEMI?)* '}';

protocol_member_declaration
    : protocol_property_declaration
	| protocol_method_declaration
	| protocol_initializer_declaration
	| protocol_subscript_declaration
	| protocol_associated_type_declaration
	| typealias_declaration
	;

// GRAMMAR OF A PROTOCOL PROPERTY DECLARATION

protocol_property_declaration:
	variable_declaration_head variable_name type_annotation getter_setter_keyword_block
	;

// GRAMMAR OF A PROTOCOL METHOD DECLARATION

protocol_method_declaration:
	function_head function_name generic_parameter_clause? function_signature generic_where_clause?
	;

// GRAMMAR OF A PROTOCOL INITIALIZER DECLARATION

protocol_initializer_declaration: initializer_head generic_parameter_clause? parameter_clause ('throws' | 'rethrows')? generic_where_clause?;

// GRAMMAR OF A PROTOCOL SUBSCRIPT DECLARATION

protocol_subscript_declaration:
	subscript_head subscript_result getter_setter_keyword_block
	;

// GRAMMAR OF A PROTOCOL ASSOCIATED TYPE DECLARATION

protocol_associated_type_declaration:
	attributes? access_level_modifier? 'associatedtype' typealias_name type_inheritance_clause? typealias_assignment?
	;

// GRAMMAR OF AN INITIALIZER DECLARATION

initializer_declaration: initializer_head generic_parameter_clause? parameter_clause ('throws' | 'rethrows')? generic_where_clause? code_block;

initializer_head:
	attributes? declaration_modifiers? 'init'
	| attributes? declaration_modifiers? 'init' '?'
	| attributes? declaration_modifiers? 'init' '!'
	;

// GRAMMAR OF A DEINITIALIZER DECLARATION

deinitializer_declaration : attributes? 'deinit' code_block;

// GRAMMAR OF AN EXTENSION DECLARATION

extension_declaration:
	attributes? access_level_modifier? 'extension' type_identifier type_inheritance_clause? extension_body
	| attributes? access_level_modifier? 'extension' type_identifier generic_where_clause extension_body
	;

extension_body : '{' (declaration | compiler_control_statement)* '}';

// GRAMMAR OF A SUBSCRIPT DECLARATION

subscript_declaration:
	subscript_head subscript_result code_block
	| subscript_head subscript_result getter_setter_block
	| subscript_head subscript_result getter_setter_keyword_block
	;

subscript_head:
	attributes? declaration_modifiers? 'subscript' parameter_clause
	;
subscript_result : arrow_operator attributes? type;

// GRAMMAR OF AN OPERATOR DECLARATION

operator_declaration:
	prefix_operator_declaration
	| postfix_operator_declaration
	| infix_operator_declaration
	;

prefix_operator_declaration		:	'prefix' 'operator' operator;
postfix_operator_declaration	:	'postfix' 'operator' operator;
infix_operator_declaration		:	'infix' 'operator' operator infix_operator_group?;

infix_operator_group : ':' precedence_group_name;

// GRAMMAR OF A PRECEDENCE GROUP DECLARATION

precedence_group_declaration:
	'precedencegroup' precedence_group_name '{' precedence_group_attribute* '}'
	;

precedence_group_attribute:
	precedence_group_relation
	| precedence_group_assignment
	| precedence_group_associativity
	;

precedence_group_relation:
	'higherThan' ':' precedence_group_names
	| 'lowerThan' ':' precedence_group_names
	;

precedence_group_assignment : 'assignment' ':' booleanLiteral;

precedence_group_associativity	:	'associativity' ':' associativity;
associativity					:	'left' | 'right' | 'none';

precedence_group_names	:	precedence_group_name (',' precedence_group_name)*;
precedence_group_name	:	declaration_identifier;

// GRAMMAR OF A DECLARATION MODIFIER
declaration_modifier:
	'class'
	| 'convenience'
	| 'dynamic'
	| 'final'
	| 'infix'
	| 'lazy'
	| 'optional'
	| 'override'
	| 'postfix'
	| 'prefix'
	| 'required'
	| 'static'
	| 'unowned'
	| 'unowned' LPAREN 'safe' ')'
	| 'unowned' LPAREN 'unsafe' ')'
	| 'weak'
	| access_level_modifier
	| mutation_modifier
	;

declaration_modifiers : declaration_modifier+;

access_level_modifier:
	'private'
	| 'private' LPAREN 'set' ')'
	| 'fileprivate'
	| 'fileprivate' LPAREN 'set' ')'
	| 'internal'
	| 'internal' LPAREN 'set' ')'
	| 'public'
	| 'public' LPAREN 'set' ')'
	| 'open'
	| 'open' LPAREN 'set' ')'
	;

mutation_modifier : 'mutating' | 'nonmutating';

// Patterns

// GRAMMAR OF A PATTERN

pattern:
	UNDERSCORE type_annotation?
	| identifier_pattern type_annotation?
	| value_binding_pattern
	| tuple_pattern type_annotation?
	| enum_case_pattern
	| optional_pattern
	| 'is' type
	| pattern 'as' type
	| expression_pattern
	;

// GRAMMAR OF AN IDENTIFIER PATTERN

identifier_pattern : declaration_identifier;

// GRAMMAR OF A VALUE_BINDING PATTERN

value_binding_pattern : 'var' pattern | 'let' pattern;

// GRAMMAR OF A TUPLE PATTERN

tuple_pattern : LPAREN tuple_pattern_element_list? ')';
tuple_pattern_element_list:
	tuple_pattern_element (',' tuple_pattern_element)*
	;
tuple_pattern_element : pattern;

// GRAMMAR OF AN ENUMERATION CASE PATTERN

enum_case_pattern : type_identifier? '.' enum_case_name tuple_pattern?;

// GRAMMAR OF AN OPTIONAL PATTERN
optional_pattern : identifier_pattern '?';

// GRAMMAR OF A TYPE CASTING PATTERN

// integrated directly into pattern to avoid indirect left-recursion

// GRAMMAR OF AN EXPRESSION PATTERN

/** Doc says "Expression patterns appear only in switch statement case labels." */
expression_pattern : expression;

// Attributes

// GRAMMAR OF AN ATTRIBUTE

attribute					:	'@' attribute_name attribute_argument_clause?;
attribute_name				:	declaration_identifier;
attribute_argument_clause	:	LPAREN balanced_tokens ')';
attributes					:	attribute+;
balanced_tokens				:	balanced_token*;

// https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Attributes.html#//apple_ref/swift/grammar/attributes
//
// Quote: balanced-token → (­balanced-tokens­opt­)­ balanced-token → [­balanced-tokens­opt­]­ balanced-token →
// {­balanced-tokens­opt­}­ balanced-token → Any identifier, keyword, literal, or operator balanced-token → Any
// punctuation except (­, )­, [­, ]­, {­, or }­
//
// Example: @available(*, deprecated, message: "it will be removed in Swift 4.0. Please use 'Collection' instead") Apple
// doesn't provide proper grammar for attributes. It says "Any punctuation except (­, )­, [­, ]­, {­, or }­".
balanced_token:
	LPAREN balanced_tokens ')'
	| '[' balanced_tokens ']'
	| '{' balanced_tokens '}'
	| label_identifier
	| literal
	| operator
	| Platform_name_platform_version // there is a kludge, see Platform_name_platform_version; it is a token
	| any_punctuation_for_balanced_token
	;

// https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/swift/grammar/identifier
// Quote: The following tokens are reserved as punctuation and can’t be used as custom operators: (, ), {, }, [, ], .,
// ,, :, ;, =, @, #, & (as a prefix operator), ->, `, ?, and ! (as a postfix operator).
any_punctuation_for_balanced_token:
	('.' | ',' | ':' | ';' | '=' | '@' | '#' | '`' | '?')
	| arrow_operator
	|	{SwiftBaseParser.isPrefixOp(_input)}? '&'
	|	{SwiftBaseParser.isPostfixOp(_input)}? '!'
	;

// Expressions

// GRAMMAR OF AN EXPRESSION
// Broke canonical right-accoc rule
// since the most of binaryExprs are left-accoc

expression : try_operator?  prefix_or_binary_expression;

expression_list : expression (',' expression)*;

// GRAMMAR OF A PREFIX EXPRESSION
// Corresponds to the prefixExpr from the Swift spec

prefix_expression_original
    : prefix_operator? postfix_expression
	| '&' declaration_identifier
	;

// GRAMMAR OF A BINARY EXPRESSION

prefix_or_binary_expression:
	prefix_or_binary_expression binary_operator prefix_or_binary_expression
	| prefix_or_binary_expression assignment_operator try_operator? prefix_or_binary_expression
	| <assoc=right> prefix_or_binary_expression conditional_operator try_operator? prefix_or_binary_expression
	| prefix_or_binary_expression type_casting_operator
	| prefix_expression_original
	;

// GRAMMAR OF A CONDITIONAL OPERATOR

conditional_operator : '?' try_operator? expression ':';

// GRAMMAR OF A TYPE_CASTING OPERATOR

type_casting_operator : ('is' | 'as' ('!' | '?')? ) type;

// GRAMMAR OF A PRIMARY EXPRESSION

primary_expression
    : declaration_identifier generic_argument_clause?
	| literal_expression
	| self_expression
	| superclass_expression
	| closure_expression
	| parenthesized_expression
	| tuple_expression
	| implicit_member_expression
	| UNDERSCORE
	| selector_expression
	| key_path_expression
	;

// GRAMMAR OF A LITERAL EXPRESSION

literal_expression
    : array_literal
	| literal
	| dictionary_literal
	| '#file'
	| '#line'
	| '#column'
	| '#function'
	| '#dsohandle' // Private Apple stuff. Not in docs, but in compiler and in sources of swift.
	;

array_literal : '[' expression_list? ','? ']';

dictionary_literal : '[' dictionary_literal_item (',' dictionary_literal_item)* ','? ']' | '[' ':' ']';

dictionary_literal_item : expression ':' expression;

playground_literal:
	'#colorLiteral' LPAREN 'red' ':' expression ',' 'green' ':' expression ',' 'blue' ':' expression ',' 'alpha' ':'
		expression ')'
	| '#fileLiteral' LPAREN 'resourceName' ':' expression ')'
	| '#imageLiteral' LPAREN 'resourceName' ':' expression ')'
	;

// GRAMMAR OF A SELF EXPRESSION

self_expression:
	'self'
	| 'self' '.' declaration_identifier
	| 'self' '[' expression_list ']'
	| 'self' '.' 'init'

	// From ParseExpr.cpp. self and Self parsed with same code:
	//
	// case tok::kw_self: // self case tok::kw_Self: // Self Result = makeParserResult(parseExprIdentifier());
	//

	// However, later something happens and Self[1], Self
	//
	// Example code from SetAlgebra.swift:
	//
	// public var isEmpty: Bool { return self == Self() }
	//
	// Also a valid code:
	//
	// return self == Self() && self == Self.init() && Self.Other() == Self.Other()
	//
	// So this is undocumented:
	| 'Self'							// Self()
	| 'Self' '.' declaration_identifier	// Self.This()
	| 'Self' '.' 'init'					// Self.init()
	;

// GRAMMAR OF A SUPERCLASS EXPRESSION

superclass_expression : SUPER ( '.' (declaration_identifier
                                    | 'init')
							  | '[' function_call_argument (',' function_call_argument)* ']');

// GRAMMAR OF A CLOSURE EXPRESSION

closure_expression : '{' closure_signature? statements? '}';

closure_signature:
	capture_list? closure_parameter_clause 'throws'? function_result? 'in'
	| capture_list 'in'
	;

closure_parameter_clause:
	  LPAREN (closure_parameter (',' closure_parameter)*)? ')'
	| declaration_identifier (',' declaration_identifier)*
	;


// According to the Swift Book:
// "The parameters have the same form as the parameters in a function declaration, as described in Function Declaration."
// https://docs.swift.org/swift-book/ReferenceManual/Expressions.html#grammar_closure-expression
// Although, grammar rule from spec is different with function parameters and both of them
// are incomplete. Mixed rule below is satisfying.

closure_parameter
    : external_parameter_name? label_identifier type_annotation? default_argument_clause?
	| external_parameter_name? label_identifier type_annotation range_operator
	;

capture_list : '[' capture_list_items ']';

capture_list_items : capture_list_item (',' capture_list_item)*;

capture_list_item : capture_specifier? expression;

capture_specifier : 'weak' | 'unowned' | 'unowned(safe)' | 'unowned(unsafe)';

// GRAMMAR OF A IMPLICIT MEMBER EXPRESSION

implicit_member_expression : '.' label_identifier;
// let a: MyType = .default; static let `default` = MyType()

// GRAMMAR OF A PARENTHESIZED EXPRESSION

parenthesized_expression : LPAREN expression ')';

// GRAMMAR OF A TUPLE EXPRESSION

tuple_expression : LPAREN ')' | LPAREN tuple_element (',' tuple_element)+ ')';

tuple_element : expression | label_identifier ':' expression;

// GRAMMAR OF A SELECTOR EXPRESSION

selector_expression:
	'#selector' LPAREN expression ')'
	| '#selector' LPAREN 'getter:' expression ')'
	| '#selector' LPAREN 'setter:' expression ')'
	;

// GRAMMAR OF A KEY-PATH EXPRESSION

key_path_expression
    : '#keyPath' LPAREN expression ')'
	// | '\\' expression  // TODO: fixed failure with improper baclslash symbol generation
	;

// GRAMMAR OF A POSTFIX EXPRESSION (inlined many rules from spec to avoid indirect left-recursion)

postfix_expression:
	primary_expression															# primary
	| postfix_expression postfix_operator										# postfix_operation
	| postfix_expression function_call_argument_clause? closure_expression		# function_call_expression_with_closure
	| postfix_expression function_call_argument_clause							# function_call_expression
	| postfix_expression '.' 'init'												# initializer_expression
	| postfix_expression '.' 'init' LPAREN argument_names ')'						# initializer_expression_with_args
	| postfix_expression '.' PURE_DECIMAL_DIGITS								# explicit_member_expression1
	| postfix_expression '.' (declaration_identifier | label_identifier) generic_argument_clause?	# explicit_member_expression2
	| postfix_expression '.' (declaration_identifier | label_identifier) LPAREN argument_names ')'		# explicit_member_expression3
	// This does't exist in the swift grammar, but this valid swift statement fails without it self.addTarget(self,
	// action: #selector(nameOfAction(_:)))
	| postfix_expression LPAREN argument_names ')'		# explicit_member_expression4
	| postfix_expression '.' 'self'					# postfix_self_expression
	| dynamic_type_expression						# dynamic_type
	| postfix_expression '[' expression_list ']'	# subscript_expression
	// postfix_expression ('!' | '?')                # forced_value_expression
	// ! is a postfix operator already | postfix_expression '!' # forced_value_expression ? is a postfix operator
	// already | postfix_expression '?' # optional_chaining_expression
	;

// GRAMMAR OF A FUNCTION CALL EXPRESSION

// See the optimization in postfix_expression. It should be doing exactly this:
//
// function-call-expression → postfix-expression­ function-call-argument-clause­ function-call-expression →
// postfix-expression­ function-call-argument-clause­?­ trailing-closure

/*
function_call_argument_clause : LPAREN function_call_argument_list? ')';

function_call_argument_list:
	function_call_argument (',' function_call_argument)*
	;
*/

function_call_argument_clause : LPAREN function_call_argument? (',' function_call_argument)* ')';

function_call_argument
    : expression
	| operator
	| label_identifier ':' (expression | operator)
	;

// GRAMMAR OF AN EXPLICIT MEMBER EXPRESSION

argument_names	:	argument_name+;
argument_name	:	label_identifier ':';

// GRAMMAR OF A DYNAMIC TYPE EXPRESSION

dynamic_type_expression : 'type' LPAREN 'of' ':' expression ')';

// GRAMMAR OF A TYPE

type
    : array_type				# the_array_type
	| dictionary_type			# the_dictionary_type
	| function_type				# the_function_type
	| type_identifier			# the_type_identifier
	| tuple_type				# the_tuple_type
	| type '?'					# the_optional_type
	| type '!'					# the_implicitly_unwrapped_optional_type
	| protocol_composition_type	# the_protocol_composition_type
	| type '.' 'Type'			# the_metatype_type_type
	| type '.' 'Protocol'		# the_metatype_protocol_type
	| ANY						# the_any_type
	| SELF_CAPITAL				# the_self_type
	;

// GRAMMAR OF A TYPE ANNOTATION

type_annotation : ':' attributes? 'inout'? type;

// GRAMMAR OF A TYPE IDENTIFIER

type_identifier
    : type_name generic_argument_clause? '.' type_identifier
	| type_name generic_argument_clause?
	;

type_name
    : declaration_identifier
	| label_identifier
	;

// GRAMMAR OF A TUPLE TYPE

tuple_type : LPAREN tuple_type_element_list? ')';
tuple_type_element_list:
	tuple_type_element
	| tuple_type_element ',' tuple_type_element_list
	;
tuple_type_element	:	element_name type_annotation | type;
element_name		:	label_identifier;

// GRAMMAR OF A FUNCTION TYPE

function_type: attributes? LPAREN function_type_argument? (',' function_type_argument)* range_operator? ')' (THROWS | RETHROWS)? arrow_operator type;

function_type_argument:
	attributes? 'inout'? type
	| UNDERSCORE? label_identifier type_annotation
	;

// GRAMMAR OF AN ARRAY TYPE

array_type : '[' type ']';

// GRAMMAR OF A DICTIONARY TYPE

dictionary_type : '[' type ':' type ']';

// GRAMMAR OF AN OPTIONAL TYPE

// The following sets of rules are mutually left-recursive [type, optional_type, implicitly_unwrapped_optional_type,
// metatype_type] optional_type : type '?' ;

// GRAMMAR OF AN IMPLICITLY UNWRAPPED OPTIONAL TYPE

// The following sets of rules are mutually left-recursive [type, optional_type, implicitly_unwrapped_optional_type,
// metatype_type] implicitly_unwrapped_optional_type : type '!' ;

// GRAMMAR OF A PROTOCOL COMPOSITION TYPE

protocol_composition_type	:	protocol_identifier ('&' protocol_identifier)+;
protocol_identifier			:	type_identifier;

// GRAMMAR OF A METATYPE TYPE

// The following sets of rules are mutually left-recursive [type, optional_type, implicitly_unwrapped_optional_type,
// metatype_type] metatype_type : type '.' 'Type' | type '.' 'Protocol' ;

// GRAMMAR OF A TYPE INHERITANCE CLAUSE

type_inheritance_clause
    : ':' (CLASS ',')? type_identifier (',' type_identifier)*
	| ':' CLASS
	;

// ---------- Lexical Structure -----------

// GRAMMAR OF AN IDENTIFIER

// identifier : Identifier | context_sensitive_keyword ;
//
// identifier is context sensitive

// var x = 1; funx x() {}; class x {}
declaration_identifier : identifier | keyword_as_identifier_in_declarations;

// external, internal argument name
label_identifier : identifier | keyword_as_identifier_in_labels;

/*
 declaration : variable_declaration;

 variable_declaration : VAR identifier ASSIGN literal;

 // Expressions
 expression_list : expression (',' expression)*;

 expression : try_operator?; //prefix_expression
 prefix_or_binary_expressions?;
 */

/*
 type:
 array_type # the_array_type
 | dictionary_type # the_dictionary_type
 | function_type # the_function_type
 |
 type_identifier # the_type_identifier
 | tuple_type # the_tuple_type
 | type '?' # the_optional_type
 | type
 '!'
 #
 the_implicitly_unwrapped_optional_type
 | protocol_composition_type # the_protocol_composition_type
 | type
 '.'
 'Type'
 # the_metatype_type_type
 | type '.' 'Protocol' # the_metatype_protocol_type
 | 'Any' # the_any_type
 |
 'Self'
 #
 the_self_type
 ;
 */

// Operators

try_operator : TRY (QUESTION | BANG)?;

// Identifiers and Literals

identifier : IDENTIFIER_TOKEN | UNDERSCORE;

identifier_list : identifier (COMMA identifier)*;

integer : BINARY_LIT | DECIMAL_LIT | OCTAL_LIT | HEX_LIT;

literal : booleanLiteral | numeric_literal | NIL_LIT | string_literal;

booleanLiteral: TRUE | FALSE;

// TODO : Fix predicate for correct processing for negative numbers
numeric_literal : SUB? (integer | FLOAT_LIT);

// TODO: Support strings with balanced tokens
// TODO: Try to glue a few *STRING_PART before ParseTree consuming

string_literal
    : static_string_literal
    | DOUBLE_QUOTE string_literal_content* DOUBLE_QUOTE
	| TRIPLE_QUOTE multiString_content* TRIPLE_QUOTE
    ;

static_string_literal
    : DOUBLE_QUOTE STRING_PART* DOUBLE_QUOTE
	| TRIPLE_QUOTE MULTILINE_PART* TRIPLE_QUOTE
	;

string_literal_content
    : STRING_PART+
	| expression
	;

multiString_content
    : MULTILINE_PART+
	| expression
	;

eos : ';' | EOF;

// func x(Any: Any)
keyword_as_identifier_in_declarations:
	PROTOCOL_CAPITAL
	| TYPE_CAPITAL
	| ALPHA
	| ARCH
	| ARM
	| ARM64
	| ASSIGNMENT
	| ASSOCIATIVITY
	| BLUE
	| CONVENIENCE
	| DIDSET
	| DYNAMIC
	| FILE
	| FINAL
	| GET
	| GREEN
	| HIGHERTHAN
	| I386
	| IOS
	| IOSAPPLICATIONEXTENSION
	| INDIRECT
	| INFIX
	| LAZY
	| LEFT
	| LINE
	| LOWERTHAN
	| MACOS
	| MACOSAPPLICATIONEXTENSION
	| MUTATING
	| NONE
	| NONMUTATING
	| OF
	| OPEN
	| OPTIONAL
	| OS
	| OSX
	| OVERRIDE
	| POSTFIX
	| PRECEDENCE
	| PREFIX
	| RED
	| REQUIRED
	| RESOURCENAME
	| RIGHT
	| SAFE
	| SET
	| SWIFT
	| TVOS
	| TYPE_LOWER
	| UNOWNED
	| UNSAFE
	| WATCHOS
	| WEAK
	| WILLSET
	| X86_64
	;
/*'Any'
	| 'Protocol'
	| 'Self'
	| 'Type'
	| 'alpha'
	| 'arch'
	| 'arm'
	| 'arm64'
	| 'as'
	| 'assignment'
	| 'associatedtype'
	| 'associativity'
	| 'blue'
	| 'break'
	| 'case'
	| 'catch'
	| 'class'
	| 'continue'
	| 'convenience'
	| 'default'
	| 'defer'
	| 'deinit'
	| 'didSet'
	| 'do'
	| 'dynamic'
	| 'else'
	| 'enum'
	| 'extension'
	| 'fallthrough'
	| 'false'
	| 'file'
	| 'fileprivate'
	| 'final'
	| 'for'
	| 'func'
	| 'get'
	| 'green'
	| 'guard'
	| 'higherThan'
	| 'i386'
	| 'iOS'
	| 'iOSApplicationExtension'
	| 'if'
	| 'import'
	| 'in'
	| 'indirect'
	| 'infix'
	| 'init'
	| 'internal'
	| 'is'
	| 'lazy'
	| 'left'
	| 'line'
	| 'lowerThan'
	| 'macOS'
	| 'macOSApplicationExtension'
	| 'mutating'
	| 'nil'
	| 'none'
	| 'nonmutating'
	| 'of'
	| 'open'
	| 'operator'
	| 'optional'
	| 'os'
	| 'override'
	| 'postfix'
	| 'precedence'
	| 'precedencegroup'
	| 'prefix'
	| 'private'
	| 'protocol'
	| 'public'
	| 'red'
	| 'repeat'
	| 'required'
	| 'resourceName'
	| 'rethrows'
	| 'return'
	| 'right'
	| 'safe'
	| 'self'
	| 'set'
	| 'static'
	| 'struct'
	| 'subscript'
	| 'super'
	| 'swift'
	| 'switch'
	| 'throw'
	| 'throws'
	| 'true'
	| 'try'
	| 'tvOS'
	| 'type'
	| 'typealias'
	| 'unowned'
	| 'unsafe'
	| 'watchOS'
	| 'weak'
	| 'where'
	| 'while'
	| 'willSet'
	| 'x86_64'
	;*/

keyword_as_identifier_in_labels
    : ANY
	| PROTOCOL_CAPITAL
	| SELF_CAPITAL
	| TYPE_CAPITAL
	| ALPHA
	| ARCH
	| ARM
	| ARM64
	| AS
	| ASSIGNMENT
	| ASSOCIATED_TYPE
	| ASSOCIATIVITY
	| BLUE
	| BREAK
	| CASE
	| CATCH
	| CLASS
	| CONTINUE
	| CONVENIENCE
	| DEFAULT
	| DEFER
	| DEINIT
	| DIDSET
	| DO
	| DYNAMIC
	| ELSE
	| ENUM
	| EXTENSION
	| FALLTHROUGH
	| FALSE
	| FILE
	| FILEPRIVATE
	| FINAL
	| FOR
	| FUNC
	| GET
	| GREEN
	| GUARD
	| HIGHERTHAN
	| I386
	| IOS
	| IOSAPPLICATIONEXTENSION
	| IF
	| IMPORT
	| IN
	| INDIRECT
	| INFIX
	| INIT
	| INTERNAL
	| IS
	| LAZY
	| LEFT
	| LINE
	| LOWERTHAN
	| MACOS
	| MACOSAPPLICATIONEXTENSION
	| MUTATING
	| NIL_LIT
	| NONE
	| NONMUTATING
	| OF
	| OPEN
	| OPERATOR
	| OPTIONAL
	| OS
	| OVERRIDE
	| POSTFIX
	| PRECEDENCE
	| PRECEDENCEGROUP
	| PREFIX
	| PRIVATE
	| PROTOCOL
	| PUBLIC
	| RED
	| REPEAT
	| REQUIRED
	| RESOURCENAME
	| RETHROWS
	| RETURN
	| RIGHT
	| SAFE
	| SELF_LOWER
	| SET
	| STATIC
	| STRUCT
	| SUBSCRIPT
	| SUPER
	| SWIFT
	| SWITCH
	| THROW
	| THROWS
	| TRUE
	| TRY
	| TVOS
	| TYPE_LOWER
	| TYPEALIAS
	| UNOWNED
	| UNSAFE
	| WATCHOS
	| WEAK
	| WHERE
	| WHILE
	| WILLSET
	| X86_64
	;

// GRAMMAR OF OPERATORS

/*
From doc on operators:
 The tokens =, ->, //, /*, *\/ [without the escape on \/], .,
 the prefix operators <, &, and ?, the infix
 operator ?, and the postfix operators >, !, and ? are reserved. These tokens
 can’t be overloaded, nor can they be used as custom operators.

 The whitespace around an operator is used to determine whether an operator
 is used as a prefix operator, a postfix operator, or a binary operator.

	* If an operator has whitespace around both sides or around neither
	  side, it is treated as a binary operator. As an example, the +
	  operator in a+b and a + b is treated as a binary operator.

	* If an operator has whitespace on the left side only, it is treated
	  as a prefix unary operator. As an example, the ++ operator in a ++b
	  is treated as a prefix unary operator.

	* If an operator has whitespace on the right side only, it is treated
	  as a postfix unary operator. As an example, the ++ operator in a++ b
	  is treated as a postfix unary operator.

	* If an operator has no whitespace on the left but is followed
	  immediately by a dot (.), it is treated as a postfix unary
	  operator. As an example, the ++ operator in a++.b is treated as a
	  postfix unary operator (a++ .b rather than a ++ .b).

 For the purposes of these rules, the characters (, [, and { before an operator,
 the characters ), ], and } after an operator, and the characters ,, ;, and :
 are also considered whitespace.

 There is one caveat to the rules above. If the ! or ? predefined operator has
 no whitespace on the left, it is treated as a postfix operator, regardless of
 whether it has whitespace on the right. To use the ? as the optional-chaining
 operator, it must not have whitespace on the left. To use it in the ternary
 conditional (? :) operator, it must have whitespace around both sides.

 In certain constructs, operators with a leading < or > may be split
 into two or more tokens. The remainder is treated the same way and may
 be split again. As a result, there is no need to use whitespace to
 disambiguate between the closing > characters in constructs like
 Dictionary<String, Array<Int>>. In this example, the closing >
 characters are not treated as a single token that may then be
 misinterpreted as a bit shift >> operator.
*/

//operator : Binary_operator | Prefix_operator | Postfix_operator ;

/* these following tokens are also a Binary_operator so much come first as special case */ assignment_operator:
	{SwiftBaseParser.isBinaryOp(_input)}? '='
	;

/** Need to separate this out from Prefix_operator as it's referenced in numeric_literal
 *  as specifically a negation prefix op.
 */
negate_prefix_operator : {SwiftBaseParser.isPrefixOp(_input)}? '-';

compilation_condition_AND	:		{SwiftBaseParser.isOperator(_input,"&&")}? '&' '&';
compilation_condition_OR	:		{SwiftBaseParser.isOperator(_input,"||")}? '|' '|';
compilation_condition_GE	:		{SwiftBaseParser.isOperator(_input,">=")}? '>' '=';
arrow_operator				:		{SwiftBaseParser.isOperator(_input,"->")}? '-' '>';
range_operator				:		{SwiftBaseParser.isOperator(_input,"...")}? '.' '.' '.';
same_type_equals			:		{SwiftBaseParser.isOperator(_input,"==")}? '=' '=';

/**
 "If an operator has whitespace around both sides or around neither side,
 it is treated as a binary operator. As an example, the + operator in a+b
  and a + b is treated as a binary operator."
*/
binary_operator : {SwiftBaseParser.isBinaryOp(_input)}? operator;

/**
 "If an operator has whitespace on the left side only, it is treated as a
 prefix unary operator. As an example, the ++ operator in a ++b is treated
 as a prefix unary operator."
*/
prefix_operator : {SwiftBaseParser.isPrefixOp(_input)}? operator;

/**
 "If an operator has whitespace on the right side only, it is treated as a
 postfix unary operator. As an example, the ++ operator in a++ b is treated
 as a postfix unary operator."

 "If an operator has no whitespace on the left but is followed immediately
 by a dot (.), it is treated as a postfix unary operator. As an example,
 the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
 rather than a ++ .b)."
 */
postfix_operator : {SwiftBaseParser.isPostfixOp(_input)}? operator;

operator:
	operator_head (
		{_input.Get(_input.Index-1).Type!=WS}? operator_character
	)*
	| dot_operator_head (
		{_input.Get(_input.Index-1).Type!=WS}? dot_operator_character
	)*
	;

operator_head: (
		'/'
		| '='
		| '-'
		| '+'
		| '!'
		| '*'
		| '%'
		| '&'
		| '|'
		| '<'
		| '>'
		| '^'
		| '~'
		| '?'
	) // wrapping in (..) makes it a fast set comparison
	| Operator_head_other
	;

operator_character : operator_head | Operator_following_character;

dot_operator_head		:	'.';
dot_operator_character	:	'.' | operator_character;
