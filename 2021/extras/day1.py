from itertools import islice, starmap, tee
from operator import sub

flip = lambda f: lambda x, y: f(y, x)
drop = lambda n, xs: islice(xs, n, None)
nwise = lambda n, xs: zip(*starmap(drop, enumerate(tee(xs, n))))
pos = lambda x: x > 0
increases = lambda xs: sum(map(pos, starmap(flip(sub), nwise(2, xs))))

numbers = [*map(int, open("data/day1.txt"))]
window_sum = map(sum, nwise(3, numbers))

print(increases(numbers))
print(increases(window_sum))
