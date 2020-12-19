#!/usr/bin/env python


def parse():
    with open("./input1.txt") as inputs:
        rules, lines = (
            group.split("\n") for group in inputs.read().strip().split("\n\n")
        )
        return rules, lines


def generate(rules, lines):
    print("module Main (main) where")
    print()
    print("import Text.ParserCombinators.ReadP")
    print()

    names = []
    for rule in rules:
        name, constraints = rule.split(": ")
        # print(f"p{name} :: Parser String")
        parsers = []
        for option in constraints.split(" | "):
            parser = " *> ".join(
                f"p{name}" if name.startswith('"') is False else f"string {name}"
                for name in option.split()
            )
            parsers.append(f"({parser})")

        names.append(f"p{name}")
        print(f'p{name} = {" +++ ".join(parsers)}')
    print()

    print("validate :: String -> Bool")
    print("validate = not . null . filter (null . snd) . readP_to_S p0")
    print()

    print("main :: IO ()")
    strings = [f'"{string}"' for string in lines]
    print(f'main = print $ length $ filter validate [{", ".join(strings)}]')


def solve1():
    generate(*parse())


def solve2():
    rules, lines = parse()

    # Update
    rules = [r for r in rules if not r.startswith("8: ") and not r.startswith("11: ")]
    rules.append("8: 42 | 42 8")
    rules.append("11: 42 31 | 42 11 31")

    generate(rules, lines)


if __name__ == "__main__":
    # solve1()
    solve2()
