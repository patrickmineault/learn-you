import numpy as np
import tqdm
import re

def main():
    ref_line = 2
    search_area = 4_000_000
    with open("data/day15.txt", "r") as f:
        lines = f.readlines()

    clean_lines=[]

    for line in lines:
        line = line.strip()
        numbers=re.findall('-?\d+',line)
        clean_lines.append([int(x) for x in numbers])

    sensors=[]
    beacons=[]

    for line in clean_lines:
        sensor=(line[0],line[1])
        beacon=(line[2],line[3])
        sensors.append(sensor)
        beacons.append(beacon)

    intervals = []
    for sensor, beacon in tqdm.tqdm(zip(sensors, beacons)):
        x0, y0 = sensor
        x1, y1 = beacon
        distance = abs(x0 - x1) + abs(y0 - y1)

        # Figure out what on line 10 the diamond touches.
        vertical_distance = abs(ref_line - y0)
        diamond_half_width = distance - vertical_distance
        if diamond_half_width >= 0:
            interval = (x0 - diamond_half_width, x0 + diamond_half_width)
            intervals.append(interval)

    minl = min([l for l, r in intervals])
    maxr = max([r for l, r in intervals])

    offset = -minl
    line = np.zeros((maxr - minl + 1), dtype=np.str_)
    for (l, r) in intervals:
        line[(l+offset):(r+offset+1)] = 'X'

    for sensor, beacon in zip(sensors, beacons):
        (x1, y1) = beacon
        if y1 == ref_line:
            line[x1 + offset] = 'B'
    



    #print(len(line))
    #print((line == 'X').sum())

    # Find the one location where the beacon can't be within the search area
    for ref_line in range(search_area+1):
        intervals = []
        for sensor, beacon in zip(sensors, beacons):
            x0, y0 = sensor
            x1, y1 = beacon
            distance = abs(x0 - x1) + abs(y0 - y1)

            # Figure out what on line 10 the diamond touches.
            vertical_distance = abs(ref_line - y0)
            diamond_half_width = distance - vertical_distance
            if diamond_half_width >= 0:
                interval = (x0 - diamond_half_width, x0 + diamond_half_width)
                intervals.append(interval)
        if ref_line % 100000 == 0:
            print(intervals)
        merged_intervals = merge_multi_intervals(intervals)
        if len(merged_intervals) > 1:
            print(f"Possible match at line {ref_line}")
            print(merged_intervals)

        """
        minl = min([l for l, r in intervals])
        maxr = max([r for l, r in intervals])

        line = ' ' * (search_area + 1)
        line = np.array([x for x in line], dtype=np.str_)
        for (l, r) in intervals:
            l = max(l, 0)
            r = min(r, search_area)
            line[l:r+1] = 'X'

        for sensor, beacon in zip(sensors, beacons):
            (x1, y1) = beacon
            if y1 == ref_line and (x1 >= 0 and x1 < search_area+1):
                line[x1] = 'B'
        
        
        line = ''.join(line.tolist())
        try:
            x = line.index(' ')
            print((x, ref_line))
            print(x * 4000000 + ref_line)
            break
        except ValueError:
            pass
        """

def merge_multi_intervals(intervals):
    new_intervals = None
    for i in range(len(intervals)):
        for j in range(i+1, len(intervals)):
            # Let's try merging
            merged = merge_intervals(intervals[i], intervals[j])
            if len(merged) == 1:
                # It's merged!
                del intervals[j]
                del intervals[i]
                new_intervals = intervals + merged
                break
        if new_intervals is not None:
            break
    if new_intervals is None:
        # No merging was accomplished!
        return intervals
    else:
        # Great, we have merged once! Let's merge
        return merge_multi_intervals(new_intervals)
                

def merge_intervals(interval0, interval1):
    (l0, r0) = interval0
    (l1, r1) = interval1

    new_intervals = []
    merged = None
    if l0 < l1:
        if r0 >= l1 - 1:
            # We can merge!
            merged = (l0, max(r0, r1))
    else:
        if r1 >= l0 - 1:
            # We can merge!
            merged = (l1, max(r0, r1))
    if merged is None:
        return [interval0, interval1]
    else:
        return [merged]


if __name__ == '__main__':
    """
    print(merge_intervals((0, 10), (11, 20)))
    print(merge_intervals((0, 10), (9, 20)))
    print(merge_intervals((0, 10), (12, 20)))
    print(merge_intervals((11, 20), (0, 10)))
    print(merge_intervals((9, 20), (0, 10)))
    print(merge_intervals((12, 20), (0, 10)))
    print(merge_intervals((0, 20), (1, 5)))
    print(merge_intervals((1, 5), (0, 20)))
    print(merge_intervals((0, 0), (1, 1)))
    """
    main()