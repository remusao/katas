
import re

def solve1():
    expected1 = {'pid', 'eyr', 'ecl', 'iyr', 'hcl', 'hgt', 'cid', 'byr'}
    expected2 = {'pid', 'eyr', 'ecl', 'iyr', 'hcl', 'hgt', 'byr'}
    valid = 0
    with open('./input1.txt') as inputs:
        for passport in inputs.read().strip().split('\n\n'):
            keys = set()
            for entry in re.compile(r'\s+').split(passport):
                key, value = entry.split(':')
                keys.add(key)
            if keys==expected1 or keys==expected2:
                valid += 1
    print(valid)

def solve2():
    expected1 = {'pid', 'eyr', 'ecl', 'iyr', 'hcl', 'hgt', 'cid', 'byr'}
    expected2 = {'pid', 'eyr', 'ecl', 'iyr', 'hcl', 'hgt', 'byr'}
    valid = 0
    with open('./example2.txt') as inputs:
        for passport in inputs.read().strip().split('\n\n'):
            keys = set()
            for entry in re.compile(r'\s+').split(passport):
                key, value = entry.split(':')

                if key == 'byr':
                    if len(value) != 4:
                        continue

                    try:
                        v = int(value)
                        if v < 1920 or v >2002:
                            continue
                    except:
                        continue

                if key == 'iyr':
                    if len(value) != 4:
                        continue

                    try:
                        v = int(value)
                        if v < 2010 or v > 2020:
                            continue
                    except:
                        continue


                if key == 'eyr':
                    if len(value) != 4:
                        continue

                    try:
                        v = int(value)
                        if v < 2020 or v > 2030:
                            continue
                    except:
                        continue

                if key == 'hgt':
                    if not value.endswith('cm') and not value.endswith('in'):
                        continue

                    unit = value[-2:]
                    value = value[:-2]

                    try:
                        v = int(value)
                        if unit == 'cm':
                            if v < 150 or v > 193:
                                continue
                        elif unit == 'in':
                            if v < 59 or v > 76:
                                continue
                    except:
                        continue

                if key == 'hcl':
                    if not value.startswith('#'):
                        continue

                    value= value[1:]
                    if len(value) != 6:
                        continue

                    if not re.compile(r'^[0-9a-z]{6}$').match(value):
                        continue

                if key == 'ecl':
                    if value not in ('amb','blu','brn','gry','grn','hzl','oth'):
                        continue

                if key == 'pid':
                    if len(value) != 9:
                        continue
                    try:
                        int(value)
                    except:
                        continue


                keys.add(key)
            if (keys==expected1 or keys==expected2):
                valid += 1
    print(valid)




if __name__ == "__main__":
    solve2()
