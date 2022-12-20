def calculate_score(tup):
    scoreboard = {
        ("A", "X"): 3 + 1,
        ("B", "X"): 0 + 1,
        ("C", "X"): 6 + 1,
        ("A", "Y"): 6 + 2,
        ("B", "Y"): 3 + 2,
        ("C", "Y"): 0 + 2,
        ("A", "Z"): 0 + 3,
        ("B", "Z"): 6 + 3,
        ("C", "Z"): 3 + 3,
    }

    return tup, scoreboard[tup]


# Written tuple
# Use cases:
# -- coordinates: (x, y, z)
# return multiple returns
# A dictionary is a map from keys to values
#
# list: [0, 5, 10, 20, "abc"] -> keys of a list are consecutive integers
# dict: {
#   "a": "val",
#   1: "example",
#   (0, 0): "etc",
# }
if __name__ == "__main__":
    # Input
    with open("data/day2.txt", "r") as f:
        lines = f.readlines()

    # IO
    # Split into tuples
    clean_lines = []
    for line in lines:
        clean_lines.append((line[0], line[2]))

    score = 0
    for clean_line in clean_lines:
        # Tuple unpacking
        _, line_score = calculate_score(clean_line)
        score += line_score

    print(score)
