
table = {}

def ilog10(x):
    r = -1
    while x != 0:
        x //= 10
        r += 1
    return r

def compute(x, n) -> int:
    key = x * 75 + (n - 1)
    l = ilog10(x)
    h = l // 2 + 1
    if n == 0:
        return 1
    if key in table:
        return table[key]
    if x == 0:
        table[key] = compute(1, n - 1)
    elif l % 2 == 0:
        table[key] = compute(x * 2024, n - 1)
    else:
        table[key] = compute(x // 10 ** h, n - 1) + compute(x % 10 ** h, n - 1)
    return table[key]

s = 0

for x in map(int, open(0).read().split()):
    s += compute(x, 75)

print(s)
