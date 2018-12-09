function solve(players, lastMarble) {
  const mkNode = value => ({
    prev: null,
    next: null,
    value
  });

  // We create a circular list of one node
  let root = mkNode(0);
  root.next = root;
  root.prev = root;

  let currentMarble = 1;
  let currentPlayer = 0;
  const scores = new Uint32Array(players);

  // Start the game
  while (currentMarble <= lastMarble) {
    if (currentMarble % 23 === 0) {
      // Move 7 spots counter-clockwise
      for (let i = 0; i < 7; i += 1) {
        root = root.prev;
      }

      scores[currentPlayer] += currentMarble + root.value;

      // Remove current node
      root.prev.next = root.next;
      root.next.prev = root.prev;
      root = root.next;
    } else {
      // Move one step clockwise
      root = root.next;

      // Insert node
      const newNode = mkNode(currentMarble);
      newNode.next = root.next;
      newNode.prev = root;
      root.next.prev = newNode;
      root.next = newNode;

      root = newNode;
    }

    currentMarble += 1;
    currentPlayer = (currentPlayer + 1) % players;
  }

  return Math.max(...scores);
}

function assert(players, lastMarble, expected) {
  const t0 = Date.now();
  const result = solve(players, lastMarble);
  const total = Date.now() - t0;
  if (result !== expected) {
    console.error("Wrong", players, lastMarble, result, expected);
  } else {
    console.log("Test", {
      players,
      lastMarble,
      total
    });
  }
}

assert(9, 25, 32);
assert(10, 1618, 8317);
assert(13, 7999, 146373);
assert(17, 1104, 2764);
assert(21, 6111, 54718);
assert(30, 5807, 37305);

console.log("Part 1:", solve(435, 71184));
console.log("Part 2:", solve(435, 71184 * 100));
