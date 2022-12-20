class Node(object):
    def __init__(self, node_type, position, height):
        self.node_type = node_type
        self.position = position
        self.height = height
        self.neighbors = []
        self.visited = False

    def __repr__(self) -> str:
        return f"Node({self.node_type}, {self.position}, {self.height}, {[x.position for x in self.neighbors]})"

def bfs(nodes, search_nodes, nsteps):
    queue = []
    if not search_nodes:
        return len(nodes)

    for search_node in search_nodes:
        if search_node.visited:
            continue

        search_node.visited = True
        if search_node.node_type == 'end':
            # We are doneso!
            return nsteps
        
        queue += search_node.neighbors
    return bfs(nodes, queue, nsteps + 1)

def main():
    with open('data/day12.txt', 'r') as f:
        lines = f.readlines()
    lines = [l.strip() for l in lines]

    # Map to nodes
    nodes = {}
    for j, line in enumerate(lines):
        for i, char in enumerate(line):
            # Create a node for that character.
            chars = 'abcdefghijklmnopqrstuvwxyz'
            assert len(chars) == 26
            if char == 'S':
                # Start
                node_type = 'start'
                height = 0
            elif char == 'E':
                node_type = 'end'
                height = 25
            else:
                node_type = 'other'
                height = chars.index(char)

            nodes[(i, j)] = Node(node_type, (i, j), height)
    
    # Find nodes which are attainable from the current node.
    start_node = None
    potential_start_nodes = []
    for (x, y), v in nodes.items():
        if v.node_type == 'start':
            start_node = v
        if v.height == 0:
            potential_start_nodes.append(v)

        neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        valid_neighbors = []
        for n in neighbors:
            if n in nodes:
                # How about the heights?
                neighbor_node = nodes[n]
                if v.height >= neighbor_node.height - 1:
                    valid_neighbors.append(neighbor_node)
        v.neighbors = valid_neighbors

    nsteps = bfs(nodes, [start_node], 0)
    print(nsteps)
    
    nstepss = []
    for start_node in potential_start_nodes:
        for n in nodes.values():
            n.visited = False
        
        nsteps = bfs(nodes, [start_node], 0)

        print(nsteps)
        nstepss.append(nsteps)

    print(min(nstepss))
                

if __name__ == "__main__":
    main()