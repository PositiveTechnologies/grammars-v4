# def_parameters: def_parameter (COMMA def_parameter)*;
# def_parameter: named_parameter (ASSIGN test)?;
# named_parameter: NAME (COLON test)?;

# NAME COLON test
def single_typed(x: int): pass

# NAME COLON test ASSIGN test
def single_default_typed(x: int = 4): pass

# NAME COMMA NAME ASSIGN test
def second_default(x, y = 4): pass

# Positional-only parameters, https://docs.python.org/3.8/whatsnew/3.8.html#positional-only-parameters
def f(a, b, /, c, d, *, e, f):
    print(a, b, c, d, e, f)

f(10, 20, 30, d=40, e=50, f=60)