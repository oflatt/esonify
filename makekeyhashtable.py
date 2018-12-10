from sys import stdin

i = 0
s = "(make-hash-table size 26 data ("
while i < 26:
    a = stdin.readline()[0]
    s = s + str(ord(a))
    s = s + " " + str(i)
    i+=1
    s = s + " "
s = s + ")"
print(s)
