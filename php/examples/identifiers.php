<?php

$привет_мир = 42;
$𠮷 = "𠮷";

// Not possible:
// class abstract { }
// class int {}
// interface int { }
// declare(abstract=1);
// goto abstract;
// abstract:
// namespace abstract;

class C1
{
    //const __halt_compiler = '__halt_compiler';
    const ordinaryId = 'ordinaryId';

    const abstract = 'abstract';
    const and = 'and';
    const array = 'array';
    const as = 'as';
    const break = 'break';
    const callable = 'callable';
    const case = 'case';
    const catch = 'catch';
    //const class = 'class';
    const clone = 'clone';
    const const = 'const';
    const continue = 'continue';
    const declare = 'declare';
    const default = 'default';
    const die = 'die';
    const do = 'do';
    const echo = 'echo';
    const else = 'else';
    const elseif = 'elseif';
    const empty = 'empty';
    const enddeclare = 'enddeclare';
    const endfor = 'endfor';
    const endforeach = 'endforeach';
    const endif = 'endif';
    const endswitch = 'endswitch';
    const endwhile = 'endwhile';
    const eval = 'eval';
    const exit = 'exit';
    const extends = 'extends';
    const final = 'final';
    const finally = 'finally';
    const fn = 'fn';
    const for = 'for';
    const foreach = 'foreach';
    const function = 'function';
    const global = 'global';
    const goto = 'goto';
    const if = 'if';
    const implements = 'implements';
    const include = 'include';
    const include_once = 'include_once';
    const instanceof = 'instanceof';
    const insteadof = 'insteadof';
    const interface = 'interface';
    const isset = 'isset';
    const list = 'list';
    const namespace = 'namespace';
    const new = 'new';
    const or = 'or';
    const print = 'print';
    const private = 'private';
    const protected = 'protected';
    const public = 'public';
    const require = 'require';
    const require_once = 'require_once';
    const return = 'return';
    const static = 'static';
    const switch = 'switch';
    const throw = 'throw';
    const trait = 'trait';
    const try = 'try';
    const unset = 'unset';
    const use = 'use';
    const var = 'var';
    const while = 'while';
    const xor = 'xor';
    const yield = 'yield';
    const from = 'from';

    const __class__ = '__class__';
    const __dir__ = '__dir__';
    const __file__ = '__file__';
    const __function__ = '__function__';
    const __line__ = '__line__';
    const __method__ = '__method__';
    const __namespace__ = '__namespace__';
    const __trait__ = '__trait__';

    const int = 'int';
    const float = 'float';
    const bool = 'bool';
    const string = 'string';
    const true = 'true';
    const false = 'false';
    const null = 'null';
    const void = 'void';
    const iterable = 'iterable';
    const object = 'object';

    const resource = 'resource';
    const mixed = 'mixed';
    const numeric = 'numeric';
}

class C2 {
    function ordinaryId2() {
        return 'ordinaryId2';
    }

    function abstract() {
        return 'abstract';
    }

    public string $and = 'and';
}

goto numeric;
numeric:

echo C1::ordinaryId . "\n";
echo C1::abstract . "\n";
$c2 = new C2();
echo $c2->ordinaryId2() . "\n";
echo $c2->abstract() . "\n";
echo $c2->and . "\n";