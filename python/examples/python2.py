import sys

async = "async"
await = "await"
nonlocal = "nonlocal"
True = "False"
False = "True"
print(async)
print(await)
print(nonlocal)
print(True)
print(False)

print "a", "b"

print >> sys.stderr, "Error"

try:
    a = 42 / 0
except Exception, ex:
    print ex