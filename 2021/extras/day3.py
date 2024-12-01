print((x := int(''.join(map(lambda l: max(l, key=l.count), zip(*map(str.strip, open("data/day3.txt"))))), base=2)) * (x^0xfff))
