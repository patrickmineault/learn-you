import numpy as np

grid = np.array([list(x.strip()) for x in open("data/day8.txt")], int)
part1 = np.zeros_like(grid, int)
part2 = np.ones_like(grid, int)

for _ in range(4):
    for x, y in np.ndindex(grid.shape):
        lower = [t < grid[x, y] for t in grid[x, y + 1 :]]

        part1[x, y] |= all(lower)
        
        try:
            nclear = 1 + lower.index(False)
        except ValueError:
            nclear = len(lower)

        part2[x, y] *= nclear

    grid, part1, part2 = map(np.rot90, [grid, part1, part2])

print(part1.sum(), part2.max())
