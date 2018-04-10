
TRANSLATOR = 1
WRITER = 2
BOTH = 3


def main():
    _ = input()
    coins = list(map(int, input().strip().split()))
    workers = list(map(int, input().strip().split()))
    select = lambda kind: [i for i, w in enumerate(workers) if w == kind]

    scores = []

    # Try to hire a writer + a translator
    writers = select(WRITER)
    translators = select(TRANSLATOR)
    if writers and translators:
        scores.append(
            coins[min(select(WRITER), key=lambda i: coins[i])] +
            coins[min(select(TRANSLATOR), key=lambda i: coins[i])]
        )

    # Try to hire on writer-translator
    both = select(BOTH)
    if both:
        scores.append(coins[min(select(BOTH), key=lambda i: coins[i])])

    print(min(scores))


if __name__ == "__main__":
    main()
