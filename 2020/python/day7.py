from collections import defaultdict

def parse_line(line):
    parent, children_str = line.split(" bags contain ")
    if children_str.startswith("no"):
        return parent, []

    children = [
        (f"{first} {second}", int(count))
        for ch in children_str.split(', ')
        for count, first, second, *_ in [ch.split(" ")]
    ]
    return parent, children


class Graph:
    def __init__(self):
        self.children = defaultdict(list)

    def list_children(self, node):
        children = set()
        queue = [ch for ch, _ in self.children[node]]
        visited = set()
        for ch in queue:
            if ch in visited:
                continue
            visited.add(ch)
            children.add(ch)
            queue.extend([ch for ch, _ in self.children[ch] if ch not in visited])
        return children

    def weight(self, node):
        return 1 + sum(w * self.weight(ch) for ch, w in self.children[node])



if __name__ == "__main__":
    g = Graph()
    ig = Graph()
    with open("data/day7.txt") as f:
        parsed = (parse_line(l) for l in f)
        for parent, children in parsed:
            g.children[parent].extend(children)
            for ch, _ in children:
                ig.children[ch].append((parent, 1))

    print(sum("shiny gold" in g.list_children(n) for n in g.children))
    print(len(ig.list_children("shiny gold")))
    print(g.weight("shiny gold"))
