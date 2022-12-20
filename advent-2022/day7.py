from collections import namedtuple


FileDescriptor = namedtuple("FileDescriptor", ["name", "size"])
# Directory = namedtuple("Directory", ["name", "parent", "files", "size", "cum_size"])
class Directory:
    def __init__(self, name, parent, files):
        self.name = name
        self.parent = parent
        self.files = files
        self.size = 0
        self.cum_size = 0

    def __repr__(self):
        return f"Directory({self.name}, parent={self.parent}, files={len(self.files)}, size=({self.size}), cum_size=({self.cum_size}))"

    def calc_filesize(self):
        size = 0
        for f in self.files:
            size += f.size
        self.size = size
        self.cum_size += size


with open("data/day7_test.txt", "r") as f:
    lines = f.readlines()

curr_dir = []
files = []
dirs = []

lines.append("$ cd ..")

for line in lines[1:]:
    line = line.strip()
    print(curr_dir)
    if line.startswith("$ cd"):
        if files:
            dirs.append(
                Directory(
                    name="/" + "/".join(curr_dir),
                    parent="/" + "/".join(curr_dir[:-1]),
                    files=files,
                ),
            )
            files = []

        if line.startswith("$ cd .."):
            # Go back up.
            curr_dir.pop()
        elif line.startswith("$ cd "):
            new_dirname = line.split(" ")[-1]
            curr_dir.append(new_dirname)
            # and go inside the directory

    elif line.startswith("$ ls"):
        # What follows is a listing
        pass
    elif line[0] in "0123456789":
        # This is a file with a certain size
        parts = line.split(" ")
        filesize = int(parts[0])
        filename = parts[1]
        files.append(FileDescriptor(name=filename, size=filesize))
        # files.append((filename, filesize))
    elif line.startswith("dir"):
        # Don't do anything
        pass


def propagate_to_parent(dirs, curr_dir, size):
    if curr_dir.name == "/":
        # We're done.
        return

    # Find the parent
    parent_names = [x.name for x in dirs]
    position = parent_names.index(curr_dir.parent)
    parent = dirs[position]
    parent.cum_size += size
    propagate_to_parent(dirs, parent, size)


# print("\n".join([str(f) for f in files]))
for d in dirs[::-1]:
    d.calc_filesize()
    propagate_to_parent(dirs, d, d.size)

for d in dirs:
    print(d)
