function solve(n, xs, ys) {
  const max_x = Math.max(...xs);
  const max_y = Math.max(...ys);
  const max_distance = max_x + max_y;

  const counter = new Int32Array(n);
  for (let x = 0; x <= max_x; x += 1) {
    for (let y = 0; y <= max_y; y += 1) {
      let point = n;
      let min1 = max_distance;
      let min2 = max_distance;
      for (let i = 0; i < n; i += 1) {
        const distance = Math.abs(xs[i] - x) + Math.abs(ys[i] - y);
        if (distance <= min1) {
          point = i;
          min2 = min1;
          min1 = distance;
        } else if (distance < min2) {
          min2 = distance;
        }
      }

      // Update counter and ignore infinite areas
      if (min1 !== min2 && counter[point] !== -1) {
        counter[point] += 1;
        if (y === 0 || x === 0 || y === max_y || x === max_x) {
          counter[point] = -1; // infinite area
        }
      }
    }
  }

  return Math.max(...counter);
}

function main() {
  const coordinates = `
81, 252
67, 186
206, 89
97, 126
251, 337
93, 101
193, 113
101, 249
276, 304
127, 140
289, 189
289, 264
79, 66
178, 248
91, 231
75, 157
260, 221
327, 312
312, 141
112, 235
97, 354
50, 200
192, 303
108, 127
281, 359
128, 209
50, 306
67, 314
358, 270
87, 122
311, 83
166, 192
170, 307
322, 320
352, 265
167, 342
296, 145
231, 263
340, 344
134, 132
72, 281
135, 352
140, 119
58, 325
247, 123
256, 346
330, 356
281, 177
216, 145
278, 98
  `
    .trim()
    .split("\n")
    .map(line => line.split(", ").map(c => parseInt(c, 10)));

  const n = coordinates.length | 0;
  const xs = new Uint32Array(n);
  const ys = new Uint32Array(n);
  for (let i = 0; i < n; i += 1) {
    const [x, y] = coordinates[i];
    xs[i] = x | 0;
    ys[i] = y | 0;
  }

  const t0 = Date.now();
  for (let i = 0; i < 200; i += 1) {
    solve(n, xs, ys);
  }
  const total = (Date.now() - t0) / 200;
  console.log(total);

  console.log(`Part 1: ${solve(n, xs, ys)}`);
}

main();
