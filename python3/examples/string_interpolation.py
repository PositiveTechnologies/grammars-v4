a1 = "LEFT OUTER JOIN body b ON f.id = b.flow_id " \
    "AND f.id IN ({','.join(['?' for _ in range(len(ids))])});"

a2 = "LEFT OUTER JOIN body b ON f.id = b.flow_id " \
    f"AND f.id IN ({','.join(['?' for _ in range(len(ids))])});"

print(f"cibuild.{attr}={getattr(self, attr)}", file=fp)

tmpdir.mkdir("mitmproxy").join("version.py").write(f'VERSION = "{version}"')

append(
    f'[Previous line repeated {count} more'
    f'time{"s" if count > 1 else ""}]\n'
)

append(
    '1234'
    '5678'
)

# comment
def __repr__(self):
    return f'<{type(self).__name__} at {id(self):#x06} {self._format()}>'

f'abc{expr1:spec1}{expr2!r:spec2}def{expr3}ghi'

name = "Fred"
f"He said his name is {name!r}."
f"He said his name is {repr(name)}."  # repr() is equivalent to !r
width = 10
precision = 4
value = decimal.Decimal("12.34567")
f"result: {value:{width}.{precision}}"  # nested fields
today = datetime(year=2017, month=1, day=27)
f"{today=:%B %d, %Y}" # using date format specifier and debugging
number = 1024
f"{number:#0x}"  # using integer format specifier
foo = "bar"
f"{ foo = }" # preserves whitespace
line = "The mill's closed"
f"{line = }"
f"{line = :20}"
f"{line = !r:20}"

f'<{res[1:-1]} [{extra}]>'

f'''abc {a['x']} def'''
print(f'typing.Callable'
      f'[[{", ".join([_type_repr(a) for a in self.__args__[:-1]])}], '
      f'{_type_repr(self.__args__[-1])}]')

f"""ab'c"""

print(f"{1+2}"
'abc')