const fs = require("fs");

function fuelRec(mass) {
  const fuel = Math.floor(mass / 3) - 2;
  if (fuel <= 0) {
    return 0;
  }

  return fuel + fuelRec(fuel);
}

let total = 0;
for (const mass of fs
  .readFileSync(process.argv[process.argv.length - 1], "utf-8")
  .split(/[\r\n]+/g)
  .filter(l => l.length !== 0)
  .map(Number)
) {
  const fuel = fuelRec(mass);
  console.log(`${mass} -> ${fuel}`);
  total += fuel;
}
console.log(total);
