function factorial(n) {
  if (n <= 0) {
    return 0;
  }

  let total = n;
  while (--n) {
    total *= n;
  }
  return total;
}

function adjacentSwapsRec(arr, k, permutations) {
  if (k === 0) {
    permutations.push(arr.toString());
    return;
  }

  for (let i = 1; i < arr.length; i += 1) {
    let tmp = arr[i];
    arr[i] = arr[i - 1];
    arr[i - 1] = tmp;
    adjacentSwapsRec(arr, k - 1, permutations);
    arr[i - 1] = arr[i];
    arr[i] = tmp;
  }
}

function adjacentSwaps(n, k) {
  const arr = [];
  for (let i = 1; i <= n; i += 1) {
    arr.push(i);
  }

  const permutations = [];
  adjacentSwapsRec(arr, k, permutations);
  return permutations;
}

for (let n = 1; n < 9; n += 1) {
  for (let k = 1; k <= (n + 1); k += 1) {
    const permutations = adjacentSwaps(n, k);
    const unique = new Set(permutations);
    console.log(`n=${n}, k=${k}`, {
      total: factorial(n),
      totalK: permutations.length,
      unique: unique.size,
      dup: permutations.length - unique.size,
    });
  }
  console.log();
}
