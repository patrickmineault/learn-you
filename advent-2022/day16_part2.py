import copy

from collections import namedtuple

Valve = namedtuple('Valve', ['name', 'flow_rate', 'connected'])

class Path:
    def __init__(self, start_node):
        self.h_path = [start_node]
        self.e_path = [start_node]
        self.open_valves = set()
        self.value = 0
        self.flow_rate = 0

    def __lt__(self, other):
        return self.value < other.value

    def update_flow_rate(self, valves):
        self.flow_rate = sum([v.flow_rate for k, v in valves.items() if k in self.open_valves])

    def update(self):
        self.value += self.flow_rate
    

def main():
    with open('data/day16.txt', 'r') as f:
        lines = f.readlines()

    lines = [l.strip() for l in lines]

    # Parse each line
    valves = {}
    for line in lines:
        name = line[6:8]
        flow_rate = int(line.split('=')[-1].split(';')[0])
        connected = line.split('valve')[-1][1:].strip().split(', ')

        valves[name] = Valve(name, flow_rate, connected)

    max_paths = 10000
    paths = [Path('AA')]
    for i in range(26):
        print(i, len(paths))
        new_paths = []
        for path in paths:
            # Do two actions at once.
            last_node = path.h_path[-1]
            if last_node == 'WAIT':
                # Let's have the valve open from now on.
                path.open_valves = path.open_valves.union({path.h_path[-2]})
                path.update_flow_rate(valves)
                last_node = path.h_path[-2]

            if last_node not in path.open_valves and valves[last_node].flow_rate > 0:
                new_path = copy.deepcopy(path)
                new_path.h_path.append('WAIT')
                new_paths.append(new_path)

            for neighbor in valves[last_node].connected:
                new_path = copy.deepcopy(path)
                new_path.h_path.append(neighbor)
                new_paths.append(new_path)

        new_paths2 = []
        for path in new_paths:
            # Do two actions at once.
            last_node = path.e_path[-1]
            if last_node == 'WAIT':
                # Let's have the valve open from now on.
                path.open_valves = path.open_valves.union({path.e_path[-2]})
                path.update_flow_rate(valves)
                last_node = path.e_path[-2]

            if last_node not in path.open_valves and valves[last_node].flow_rate > 0:
                new_path = copy.deepcopy(path)
                new_path.e_path.append('WAIT')
                new_paths2.append(new_path)

            for neighbor in valves[last_node].connected:
                new_path = copy.deepcopy(path)
                new_path.e_path.append(neighbor)
                new_paths2.append(new_path)

        new_paths = new_paths2

        for n in new_paths:
            n.update()

        if len(new_paths) > max_paths:
            # Remove the worst options.
            new_paths = sorted(new_paths)
            new_paths = new_paths[-max_paths:]

        paths = new_paths

    new_paths = sorted(new_paths)
    print(new_paths[-1].e_path)
    print(new_paths[-1].h_path)
    print(new_paths[-1].open_valves)
    print(new_paths[-1].value)

if __name__ == '__main__':
    main()
