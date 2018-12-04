#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import Counter, defaultdict
import sys


def iter_guards():
    guard_id = None
    sleep_start = None
    for line in sorted(sys.stdin):
        now = int(line[15:17])
        if "Guard" in line:
            guard_id = int(line[26 : line.find(" ", 26)])
            sleep_start = None
        elif "asleep" in line:
            sleep_start = now
        elif "wakes" in line:
            if guard_id is not None and sleep_start is not None:
                yield guard_id, sleep_start, now


def main():
    # Accumulate the count of each minute slept for each guard
    minute_count_per_guard = defaultdict(Counter)
    for guard, sleep_start, sleep_end in iter_guards():
        minute_count_per_guard[guard].update(range(sleep_start, sleep_end))

    # Part 1
    guard_who_slept_most, minute_slept = max(
        minute_count_per_guard.items(), key=lambda e: sum(e[1].values())
    )
    most_slept_minute = max(minute_slept.items(), key=lambda e: e[1])[0]
    print("Part 1", most_slept_minute * guard_who_slept_most)

    # Part 2
    guard, (most_frequent_minute, _) = max(
        (
            (guard, max(minute_slept.items(), key=lambda e: e[1]))
            for guard, minute_slept in minute_count_per_guard.items()
        ),
        key=lambda e: e[1][1],
    )
    print("Part 2", guard * most_frequent_minute)


if __name__ == "__main__":
    main()
