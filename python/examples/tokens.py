DECIMAL = 0000000000
DECIMAL = 1234567890
OCT_1   = 0o01234567
OCT_2   = 0O01234567
HEX_1   = 0x0123456789abcdef
HEX_2   = 0X0123456789ABCDEF
BIN_1   = 0b01
BIN_1   = 0B01
IMAG_1  = 0123456789.0123456789j
IMAG_2  = 0123456789J
FLOAT_1 = 0123456789.e1234567890
FLOAT_2 = .0123456789E1234567890

LINE_JOIN_EXPR  = 2 + \
    2

SHORT_STRING_1 = 'a \'\\ b'
SHORT_STRING_2 = "a \"\\ b"
LONG_STRING_1  = b""" asdf " qwer 
zxcv
"""
LONG_STRING_1  = r''' aasdf ' qwer 
zxcv
'''
STRING_WITH_LINE_JOIND = "a \
b"

# COMMENT

decimal_with_underscore = 123_456_789
hex_with_underscore     = 0xffff_ffff
oct_with_underscore     = 0o_0
bin_with_underscore     = 0b_0011_1111_0100_1110
float_with_underscore   = 10_000_000.0

exec = 12345

def print():
    pass