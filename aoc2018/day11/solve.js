function power(serialNumber, x, y) {
  let cellPower = 0;
  cellPower += x + 10;
  cellPower *= y;
  cellPower += serialNumber;
  cellPower *= x + 10;
  cellPower = Math.floor((cellPower % 1000) / 100); // hundreds digit
  cellPower -= 5;
  return cellPower;
}

function get(integral, x, y) {
  return integral[y * 301 + x];
}
function set(integral, x, y, v) {
  integral[y * 301 + x] = v;
}

function solve(serialNumber) {
  const integral = new Int32Array(301 * 301);
  let maxi = 0;
  let maxX = 0;
  let maxY = 0;
  let maxK = 0;
  for (let x = 1; x <= 300; x += 1) {
    for (let y = 1; y <= 300; y += 1) {
      set(
        integral,
        x,
        y,
        power(serialNumber, x, y) +
          get(integral, x, y - 1) +
          get(integral, x - 1, y) -
          get(integral, x - 1, y - 1)
      );

      for (let k = 1; k <= Math.min(x, y); k += 1) {
        const squarePower =
          get(integral, x, y) +
          get(integral, x - k, y - k) -
          get(integral, x, y - k) -
          get(integral, x - k, y);

        if (squarePower > maxi) {
          maxi = squarePower;
          maxX = x - k + 1;
          maxY = y - k + 1;
          maxK = k;
        }
      }
    }
  }

  return `${maxX},${maxY},${maxK}`;
}

console.log("Solution:", solve(7672));

const t0 = Date.now();
solve(7672);
const total = Date.now() - t0;
console.log(`${total}ms`);
