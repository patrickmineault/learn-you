import functools
import json


def is_ordered(left, right):
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return 1
        elif left > right:
            return -1
        else:
            return 0
    elif isinstance(left, list) and isinstance(right, list):
        for i in range(min(len(left), len(right))):
            cmp = is_ordered(left[i], right[i])
            if cmp in (-1, 1):
                return cmp

        if len(left) < len(right):
            return 1
        elif len(right) < len(left):
            return -1
        else:
            return 0
    elif isinstance(left, list) and isinstance(right, int):
        return is_ordered(left, [right])
    elif isinstance(left, int) and isinstance(right, list):
        return is_ordered([left], right)
    else:
        print(left)
        print(right)
        raise NotImplementedError("Bad input")


def main():
    with open('data/day13.txt', 'r') as f:
        lines = [l.strip() for l in f.readlines() if l.strip()]
    
    assert len(lines) % 2 == 0
    inputs = [json.loads(l) for l in lines]
    ordered = [is_ordered(a, b) for a, b in zip(inputs[::2], inputs[1::2])]
    sum_indices = sum([y for (x, y) in zip(ordered, range(1, len(ordered)+1)) if x == 1])
    print(sum_indices)
    
    sorted_input = sorted(inputs + [[[2]], [[6]]], key=functools.cmp_to_key(is_ordered))[::-1]
    found = (sorted_input.index([[2]]) + 1) * (sorted_input.index([[6]]) + 1)
    print(found)

if __name__ == '__main__':
    main()