const fs = require("fs");
const LEFT = 0;
const RIGHT = 1;
const UP = 2;
const DOWN = 3;
const STRAIGHT = 4;

function pprint(lines, carts) {
  const pos = new Map();
  for (let i = 0; i < carts.length; i += 1) {
    const cart = carts[i];
    const { x, y } = cart;
    pos.set(`${x},${y}`, cart);
  }

  for (let y = 0; y < lines.length; y += 1) {
    const line = lines[y];
    console.log(
      [...line]
        .map((c, x) => {
          const p = `${x},${y}`;
          if (pos.has(p)) {
            const { dir } = pos.get(p);
            if (dir === RIGHT) {
              return ">";
            } else if (dir === LEFT) {
              return "<";
            } else if (dir === UP) {
              return "^";
            } else if (dir === DOWN) {
              return "v";
            }
          } else if (c === "^" || c === "v") {
            return "|";
          } else if (c === "<" || c === ">") {
            return "-";
          } else {
            return c;
          }
        })
        .join("")
    );
    for (let x = 0; x > line.length; x += 1) {}
  }
}

function solve(input) {
  const dirs = [LEFT, STRAIGHT, RIGHT];
  const lines = input.split("\n");

  // Extract carts
  const carts = [];
  for (let y = 0; y < lines.length; y += 1) {
    const line = lines[y];
    for (let x = 0; x < line.length; x += 1) {
      if (line[x] === "<") {
        carts.push({
          x,
          y,
          dir: LEFT,
          nextDir: 0,
          alive: true
        });
      } else if (line[x] === ">") {
        carts.push({
          x,
          y,
          dir: RIGHT,
          nextDir: 0,
          alive: true
        });
      } else if (line[x] === "v") {
        carts.push({
          x,
          y,
          dir: DOWN,
          nextDir: 0,
          alive: true
        });
      } else if (line[x] === "^") {
        carts.push({
          x,
          y,
          dir: UP,
          nextDir: 0,
          alive: true
        });
      }
    }
  }

  // Start simulation
  let aliveCarts = carts.length;
  while (true) {
    carts.sort((c1, c2) => {
      if (c1.y < c2.y) {
        return -1;
      } else if (c1.y > c2.y) {
        return 1;
      } else if (c1.x < c2.x) {
        return -1;
      } else if (c1.x > c2.x) {
        return 1;
      }

      return 0;
    });

    const positions = new Map();
    for (let i = 0; i < carts.length; i += 1) {
      const cart = carts[i];
      const { x, y, alive } = cart;
      if (alive) {
        positions.set(`${x},${y}`, cart);
      }
    }

    for (let i = 0; i < carts.length; i += 1) {
      const cart = carts[i];
      const { x, y, dir, nextDir, alive } = cart;
      if (!alive) {
        continue;
      }
      const c = lines[y].charCodeAt(x);
      positions.delete(`${x},${y}`);

      if (c === 43) {
        const newDir = dirs[nextDir];
        cart.nextDir = (nextDir + 1) % 3;

        // Handle direction change
        if (newDir === LEFT) {
          if (dir === LEFT) {
            cart.dir = DOWN;
          } else if (dir === DOWN) {
            cart.dir = RIGHT;
          } else if (dir === RIGHT) {
            cart.dir = UP;
          } else if (dir === UP) {
            cart.dir = LEFT;
          }
        } else if (newDir === RIGHT) {
          if (dir === LEFT) {
            cart.dir = UP;
          } else if (dir === UP) {
            cart.dir = RIGHT;
          } else if (dir === RIGHT) {
            cart.dir = DOWN;
          } else if (dir === DOWN) {
            cart.dir = LEFT;
          }
        }

        // Move the cart one step
        if (cart.dir === LEFT) {
          cart.x -= 1;
        } else if (cart.dir === UP) {
          cart.y -= 1;
        } else if (cart.dir === RIGHT) {
          cart.x += 1;
        } else if (cart.dir === DOWN) {
          cart.y += 1;
        }
      } else if (c === 47) {
        // '/'
        if (dir === RIGHT) {
          cart.dir = UP;
          cart.y -= 1;
        } else if (dir === DOWN) {
          cart.dir = LEFT;
          cart.x -= 1;
        } else if (dir === LEFT) {
          cart.dir = DOWN;
          cart.y += 1;
        } else if (dir === UP) {
          cart.dir = RIGHT;
          cart.x += 1;
        }
      } else if (c === 92) {
        // '\'
        if (dir === LEFT) {
          cart.dir = UP;
          cart.y -= 1;
        } else if (dir === UP) {
          cart.dir = LEFT;
          cart.x -= 1;
        } else if (dir === RIGHT) {
          cart.dir = DOWN;
          cart.y += 1;
        } else if (dir === DOWN) {
          cart.dir = RIGHT;
          cart.x += 1;
        }
      } else {
        if (dir === LEFT) {
          cart.x -= 1;
        } else if (dir === UP) {
          cart.y -= 1;
        } else if (dir === RIGHT) {
          cart.x += 1;
        } else if (dir === DOWN) {
          cart.y += 1;
        }
      }

      // Check collision
      if (positions.has(`${cart.x},${cart.y}`)) {
        cart.alive = false;
        positions.get(`${cart.x},${cart.y}`).alive = false;
        positions.delete(`${cart.x},${cart.y}`);

        if (aliveCarts === carts.length) {
          console.log(`Part 1: ${cart.x},${cart.y}`);
        }

        aliveCarts -= 2;
      } else {
        positions.set(`${cart.x},${cart.y}`, cart);
      }
    }

    if (aliveCarts === 1) {
      for (let k = 0; k < carts.length; k += 1) {
        if (carts[k].alive) {
          console.log(`Part 2: ${carts[k].x},${carts[k].y}`);
          return;
        }
      }
    }
  }
}

function main() {
  // solve(fs.readFileSync("example.txt", { encoding: "utf-8" }));
  solve(fs.readFileSync("input.txt", { encoding: "utf-8" }));
  // solve(fs.readFileSync("test.txt", { encoding: "utf-8" }));
}

main();
