

const fs = require('fs');

function findPrefixes(hashes) {
  hashes.sort();
  for (let i = 0; i < hashes.length - 1; i += 1) {
    const sha1 = hashes[i];
    const sha2 = hashes[i + 1];
    for (let j = 0; j < sha1.length; j += 1) {
      if (sha1[j] !== sha2[j]) {
        // console.log('Prefix', sha1, sha1.slice(0, j + 1));
        break;
      }
    }
  }
}

findPrefixes(fs.readFileSync('sha1.txt', { encoding: 'utf-8' }).trim().split('\n'));
// findPrefixes([
//   'foo',
//   'faa',
//   'bar',
//   'baz',
// ]);
