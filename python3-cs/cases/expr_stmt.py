# expr_stmt: testlist_star_expr | annassign | augassign | assign;
#
# assign: testlist_star_expr (ASSIGN testlist_star_expr)* (ASSIGN yield_expr)?;
#
# annassign: testlist_star_expr COLON test (ASSIGN testlist)?;
#
# augassign: testlist_star_expr op=('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
#               '<<=' | '>>=' | '**=' | '//=') (yield_expr|testlist);

# Yield tests (should not be used outside the function)
def f():
    # assign: testlist_star_expr ASSIGN yield_expr
    x = yield

    # assign: testlist_star_expr ASSIGN testlist_star_expr ASSIGN yield_expr
    x = y = yield

    # augassign: testlist_star_expr '+=' yield_expr
    x += yield

# testlist_star_expr
f()

# assign: testlist_star_expr ASSIGN testlist_star_expr
z = 5

# assign: testlist_star_expr ASSIGN testlist_star_expr ASSIGN testlist_star_expr
x = y = z

# annassign: testlist_star_expr COLON test
x: int

# annassign: testlist_star_expr COLON test ASSIGN testlist
x: int = 8

# augassign: testlist_star_expr '-=' testlist
x -= 9

