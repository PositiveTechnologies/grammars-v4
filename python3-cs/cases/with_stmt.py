# with_stmt: WITH with_item (COMMA with_item)*  COLON suite;

# WITH with_item COLON suite
with open("with_stmt.py"):
    pass

# WITH with_item COMMA with_item COLON suite
with open("with_stmt.py") as a, open("with_stmt.py") as b:
    pass
