with open("data/day5_test.txt") as f:
    lines = f.readlines()

boxes = []
for line in lines:
    if "[" in line:
        boxes.append(line[1::4])

print(boxes)
before_strip = ["".join(x) for x in list(zip(*boxes))]
print(before_strip)
after_strip = [x.strip() for x in before_strip]
print(after_strip)
