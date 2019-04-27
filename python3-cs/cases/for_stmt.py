# for_stmt: FOR exprlist IN testlist COLON suite (else_clause)?;

# FOR exprlist IN testlist COLON suite
for x in range(1):
    pass

# FOR exprlist IN testlist COLON suite (else_clause)?
for x in range(1):
    x
else:
    pass
