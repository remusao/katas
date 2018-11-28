
const fs = require('fs');

function findPrefixes(hashes) {
  // Build Trie
  const trie = { nexts: {}, count: 0 };
  for (const sha1 of hashes) {
    let node = trie;
    node.count += 1;
    for (const c of sha1) {
      if (node.nexts[c] === undefined) {
        node.nexts[c] = { nexts: {}, count: 0 };
      }
      node = node.nexts[c];
      node.count += 1;
    }
  }

  // Get shortest prefix for each hash
  for (const sha1 of hashes) {
    let node = trie;
    let prefix = '';
    for (const c of sha1) {
      prefix += c;
      node = node.nexts[c];
      if (node.count === 1) {
        console.log('Prefix', sha1, prefix);
        break;
      }
    }
  }
}

findPrefixes(fs.readFileSync('sha1.txt', { encoding: 'utf-8' }).trim().split('\n'));
// findPrefixes([
//   'foo',
//   'bar',
//   'baz',
// ]);

