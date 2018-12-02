const lines = [
  "xdmgyjkpruszabaqwficevtjeo",
  "xdmgybkgwuszlbaqwfichvtneo",
  "xdmgyjkpruszlbcwwfichvtndo",
  "xdmgcjkprusyibaqwfichvtneo",
  "xdmgyjktruszlbwqwficuvtneo",
  "xdmgxjkpruszlbaqyfichvtnvo",
  "xdmgytkpruszlbaqwficuvtnlo",
  "xdmgydkpruszlbaqwfijhvtnjo",
  "xfmgyjkmruszlbaqwfichvtnes",
  "xdmgyrktruszlraqwfichvtneo",
  "xdmgyjkihuszlbaqdfichvtneo",
  "hdmgyjkpruszeiaqwfichvtneo",
  "xdmzyjkpruszlbaqwgichvtnxo",
  "xdmgyjknquszlbpqwfichvtneo",
  "idmgyjrpruszlbtqwfichvtneo",
  "xkmgyjkpruuzlbaqwfichvfneo",
  "xdmgyjkpruszlfaqwficnvtner",
  "xdmgyjkpruszlbpqwficwvteeo",
  "xdmgyjkpwuszlbiqwfhchvtneo",
  "xdmgyjkpruszwbaqwfichrtnbo",
  "xdpgyjkprusblbaqwfgchvtneo",
  "xdmryjkcruszlbaqwfichvtnee",
  "xwmgylkpruszlbaqwfcchvtneo",
  "xdmgyjkpruszflaqwfixhvtneo",
  "xdmgyjkmruszloaqwfichvteeo",
  "xvmgrjkpruszlbaqwfichvsneo",
  "xdmvyjkprusmlbaqwfichvtnes",
  "xdmgyjkpruszlbaqwfichkgbeo",
  "xdmgyikpruxzlbaqwfichvtnei",
  "xdmgyjkprugzlbaqhfichvtveo",
  "xdmgyjkpruszlbaqjaichftneo",
  "xdmzijkpruszlbaqwwichvtneo",
  "xdmgyjkprsszlbaqwfihhvlneo",
  "xdmgyjkprusqlwaqzfichvtneo",
  "ximgyjkpruszlbawwfichvtnen",
  "xsmgyjzpruszlbaqwfichvaneo",
  "xdmgyjkpruszlcaoyfichvtneo",
  "xdmgyjkprusmlbaqvnichvtneo",
  "xdmgyjkvruszmbaqwfichvtueo",
  "xdmgyjppuuszleaqwfichvtneo",
  "xddgyjkprubzlbaqwfichvaneo",
  "xdmgwjkpruszebaswfichvtneo",
  "xdogyjkpruszlblqwfichvdneo",
  "xdkgyjgpruszlbaqwfizhvtneo",
  "xdvgyjkpruszlbdqwfichvtqeo",
  "xdmgyjlpruszlbapwficgvtneo",
  "xdmgyjkpruszlbaqofickvtngo",
  "xdmgyjkprqszliaywfichvtneo",
  "xdqgyjkpruszlbcqwficnvtneo",
  "xdmgdjkpruszlbaqwxichvtseo",
  "xdmgyjkpruczlbaqwfichdtnfo",
  "xdmgyjkpruszluaqwficzvtnjo",
  "xdmgyjkproszlbaqwfacevtneo",
  "xfmgijkpruszlbrqwfichvtneo",
  "odmgyjkpluszlbaqwfichvuneo",
  "xdmgyjkpruszlbaqwwichukneo",
  "xdmgdjkpruszwbaqwfichvtnet",
  "xdmgyjkzrusvlbaqwrichvtneo",
  "xdmgylkprutzlbaqwfichvtnbo",
  "xdmgyjkpruszsbaqwfijtvtneo",
  "xdmgyjkproszlbjqwfichntneo",
  "xdmgyhkpluszlbaqwfichvtnlo",
  "xdmgyjhprushlbaqwfichvtnzo",
  "gdmoyjkpruszlbarwfichvtneo",
  "cdmgyjkpruszlbaqwfcchvtned",
  "xgmgyjkpruszlbaqwfschvtnek",
  "xdmgyjkprusnlzamwfichvtneo",
  "xdmgyjkprgszlbaxwfichvuneo",
  "txmgyjksruszlbaqwfichvtneo",
  "xdmgyjkprusbbbpqwfichvtneo",
  "xdmoyjkpruszlbaqwfighvtxeo",
  "xdmgyjkpruslhbaqwfichptneo",
  "xdmgzjkpruszlbaqwffcmvtneo",
  "xdmgyjkiruszlbaqgficuvtneo",
  "vdbgyjkpruszlbaqwfichvtnek",
  "xdmgyjspruszlbaqwfochvtney",
  "xdmgyjkpruszibaqwfivhvteeo",
  "xdmgyjkpruszfbaqwficbvtgeo",
  "xdmgyjkprystlbaqwxichvtneo",
  "xdmfyjkpryszlxaqwfichvtneo",
  "xdmgyjgpruspybaqwfichvtneo",
  "xdmgyjklruszlbjqwdichvtneo",
  "xdmgyjkzruszltaqwfichvtnek",
  "xdmgqjkpruszlzaqwfichvtneh",
  "xdmgyjhnruszlbaqwficqvtneo",
  "xdmgyjkproszlbaqweichvtnez",
  "xdmgyjkprurzlbaawfichytneo",
  "xdmgyfkpruszlbaqwfschutneo",
  "xdmnyjkpruszlbaawjichvtneo",
  "xdmgyjkpybszlbaqwfichvwneo",
  "xdmgtjkhruszlbaqwfichatneo",
  "xamgyjkprurzlbaqwfichvaneo",
  "xdmgyjkpruszlbaqwgichvtnqv",
  "ndmgyjkpruszlsaqwfuchvtneo",
  "xdmgygkpgusrlbaqwfichvtneo",
  "xdmgyjkpruszfbaqwfichvtnmy",
  "xdmgyjkprupflbaqwfichvjneo",
  "ndmgyjkpruszlbagwfichvtnxo",
  "xdmgyjkpruszlbafwfilhvcneo",
  "xdmgyjkpruszlbaqwfichvjsea",
  "xebgyjkpruszlbaqafichvtneo",
  "xdmkyjdpruszlbaqwfichvtnei",
  "xomgyjkprufzlbaqwfochvtneo",
  "xdmgyjkprfsllbaqwfiihvtneo",
  "xdmyyjkpruszebaqwficmvtneo",
  "xdmnyjkpruczlbarwfichvtneo",
  "xdmgyjkpruszcbaqwbichvtneg",
  "xdmgxjkpluszlbapwfichvtneo",
  "xgrlyjkpruszlbaqwfichvtneo",
  "xdmgyjkpruszlraqwxcchvtneo",
  "xdmhyjupruszlbaqafichvtneo",
  "xdmgnjkpruszlbkqwfjchvtneo",
  "xdmgyjkpruszlwaqwfichvtndg",
  "xdmgfjkpruvqlbaqwfichvtneo",
  "xdmgejkptuszlbdqwfichvtneo",
  "xlmgyjkpruszlnaqwfochvtneo",
  "xdmgcjkpruszlbaqwfiqhvaneo",
  "xdmgyjupruyzlbaywfichvtneo",
  "gdmgyjkpruyzlbaqwficevtneo",
  "xdmgyjkaruazlbapwfichvtneo",
  "xsmiyjkpruszlbaqwfichvtveo",
  "xdmiyjkprukzlbaqwfichvtnea",
  "xdbgmjkxruszlbaqwfichvtneo",
  "xdmgyjkpruskvbaqwfichdtneo",
  "xdmgyjkprusznbaqwficshtneo",
  "xdmgyjkprusrlbaqwfzchetneo",
  "xdmgyrkpruszzbaqwfichvtned",
  "xdmgyjkprusolbacwmichvtneo",
  "xdmgypkpruszlbaqwfichvtmgo",
  "xdmgyjkprumzlbhqwfichttneo",
  "xdmgydkprusglbaqwfichvtnei",
  "xdmuyjkpruszlbpqwfichvtyeo",
  "xdmtymkprusslbaqwfichvtneo",
  "xdmgyjjprkszlbaqwfqchvtneo",
  "xdmgvjdpruszlbaqwfichgtneo",
  "xdtgyjkpruwzlbaqwfjchvtneo",
  "xdmgyjkpruszlbafseichvtneo",
  "xdmgvjkpruszlraawfichvtneo",
  "xdmgyukprgszlbatwfichvtneo",
  "xhmgyjkpruszliaqwnichvtneo",
  "xdmgyjspruszlbwqyfichvtneo",
  "xdmgyjkjruszlqaqwfichvtnvo",
  "xdmgyjkiruszlbnqwfichmtneo",
  "ximgyjkpruszlbaqwfvcevtneo",
  "xdmdyjkpruszlbaqwsithvtneo",
  "ndmgyjkpruszlbaqwfilhatneo",
  "xdmgyjkpruszlbaqwfinhvcnez",
  "xdmgypkpsuszlbajwfichvtneo",
  "xdpgmjkpluszlbaqwfichvtneo",
  "xdmgyjnprupzlbaqwfichvtnel",
  "xbmgyjkprmszlfaqwfichvtneo",
  "xdmgyjkpausllbaqwfichvtseo",
  "xdmgyjkpruszlbaqwfqchttnes",
  "xgmgyjkpruszlbaxwfichvtneb",
  "xdmgyjkpruszabqqwfichvineo",
  "xdmgpjkpquszlbaqwfichvdneo",
  "xdmgyjkeruszlbaqdficbvtneo",
  "xdmaujkpruszlbaqwfichvteeo",
  "xdmgyjkpruszlbaqwrirhvtnev",
  "xdmgyjkpsugzllaqwfichvtneo",
  "xdmgyjkpruszlbaqwfichctnlm",
  "xdmeyjkpruszlbacwfiwhvtneo",
  "xdmgyjkpiuhzlbaqwfijhvtneo",
  "xdmgyjkpruszlbmqhfiohvtneo",
  "xdegyjkpbuszlbbqwfichvtneo",
  "xdmggxkpruszlbaqwfirhvtneo",
  "xdmgojkpruszlbaqvfichvtteo",
  "xdmgyjhtruszlbaqwmichvtneo",
  "rdmgyjkpruszlbaqwfichvthek",
  "xdlgyjqpruszlbaqwfbchvtneo",
  "xdmgyjspriszlbavwfichvtneo",
  "rdkgyjkpruszlbaqwfichvtnuo",
  "tdmgyjkuruszlbaqwfichvtnev",
  "xdmgyjkpxuszlbaqwfkchvtnso",
  "xdegyjkpruszlbbqxfichvtneo",
  "xdmgyjkpruszlbaqwficpvtket",
  "xdmgyjkpruszliaqwfnchvtnec",
  "xdmgyjkpreszlbaqwficdvtdeo",
  "rdmgyjkpruszlbaywfychvtneo",
  "xdmgywkpruszlbaqwficrvtaeo",
  "xdmgyjkpruszlbanwflchvoneo",
  "xdmgyjkpruyzlbaqufychvtneo",
  "symgyjkpruszlbaqwfichvtqeo",
  "xdmgyjkpruszlbaqwfichvbzqo",
  "xzfgyjkpruszlbaqwfichvtveo",
  "udmgyjepruszlbaqwfichbtneo",
  "xhmgyjkpruszlbaqwfjchvtnef",
  "xdhgyjkpruszlbaqaftchvtneo",
  "xdmzyjkjruszlbaqwfichvtnwo",
  "xdmgyjepruszlbaqwffchvtnef",
  "xdmgyjkprurzlbaqwfikhvtneq",
  "xomoyjkpruszkbaqwfichvtneo",
  "xdmgyjkpiuszubaqwfichktneo",
  "xdmgyjkprusdlbaqwhihhvtneo",
  "xdmgyjkpruszlbaqwwirhvxneo",
  "xdmgyjkpruszlbaqwficgitzeo",
  "xdmgyjlpruszlbaqwfichpjneo",
  "xjmgyjkpxuszlbaqwfichatneo",
  "xdmgylkpruszlbaqwfiehvtnez",
  "xdmgbjkpruszmbaqwfihhvtneo",
  "xdmgyjkprubzlwaqwfichvtxeo",
  "xdmgyjhlrustlbaqwfichvtneo",
  "xdmmyjkpruszlbaqwfdchitneo",
  "xdmgyjkpruszlbaqwoichhtbeo",
  "xdzgyjkprvszlcaqwfichvtneo",
  "ndmgyjkpruszlbaqwficavxneo",
  "xdmgyjfpruszxbaqzfichvtneo",
  "xdmgyjkpeuszlbaqzficdvtneo",
  "xdmgyjkpruszlbmqffidhvtneo",
  "xdnvyjkpruszlbafwfichvtneo",
  "xdygyjkpruszlbljwfichvtneo",
  "xdigyjkpruszlbeqwfuchvtneo",
  "xdmgyjkpruszlbpzwfichvteeo",
  "bdmgyjbpruszldaqwfichvtneo",
  "xdmgyjkprrszlbaqmpichvtneo",
  "idmgyjkpruszlbaqyfichvtkeo",
  "xdmgyjkmrqsclbaqwfichvtneo",
  "xdmgyjkpruazlbeqwfichvtxeo",
  "ddmgyjkpruszlbsqwfichotneo",
  "xdmgyqkpruszjbaqwfxchvtneo",
  "xdmnyjkpruozlbaqwfichvtreo",
  "edmgyjkpruszlbuqwwichvtneo",
  "xdmgyjkprmshlbaqwfichctneo",
  "xdmgyjkpruszlbaqwffghotneo",
  "xdmcyjkprfszlbaqnfichvtneo",
  "xdmgyjypruszhbaqwficyvtneo",
  "xdmgyjkprzszlyaqwficmvtneo",
  "xlmgyjkprzszlbaqwficyvtneo",
  "xdmgyjkprutulbaqwfithvtneo",
  "xdygyjkpruszlbpqwfichvpneo",
  "xdmgsjkpoumzlbaqwfichvtneo",
  "xdmgyjkpyuszlbaqdfnchvtneo",
  "xdxgyjkpruszlbaqwfizhvtnjo",
  "xdmgyjkpruszlbaqwfschvkndo",
  "xdmgpjkprnszlcaqwfichvtneo",
  "xhmgyjkpruszlbaqwficgvtnet",
  "xdmgyjkpruswlbaqwfichvtqer",
  "ddmgyjkprcszlbaqwfichqtneo",
  "xdmgyjkpruhhlbaqwpichvtneo",
  "xdmgyjkeraszlbaqwfichvtnso",
  "nomgyjkpruszlbaqwficavxneo",
  "xdmgyjkprdszlbaqwfobhvtneo",
  "xdmgyjkprgszlbaqwfichvtdao",
  "xomgyjspruswlbaqwfichvtneo",
  "xdzgyjkpruszlbaqwficwvpneo",
  "admgejkpruszlbaqwfimhvtneo",
  "xdtgyjkpruszlmaqwfiqhvtneo",
  "xdmgymkprusqlbaqwtichvtneo",
  "xdmgyjkpluszlbaqwfidhvtnea",
  "ztmgyjjpruszlbaqwfichvtneo"
];

// const lines = [
//   'fghij',
//   'abcde',
//   'klmno',
//   'pqrst',
//   'fguij',
//   'axcye',
//   'wvxyz',
// ];

function mkNode() {
  const node = [];
  for (let i = 0; i < 27; i += 1) {
    node.push(undefined);
  }
  return node;
}

function buildTrie(lines) {
  const trie = mkNode();
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    let node = trie;
    for (let j = 0; j < line.length; j += 1) {
      const c = line.charCodeAt(j) - 97;
      if (node[c] === undefined) {
        node[c] = mkNode();
      }
      node = node[c];
    }
    node[26] = true;
  }
  return trie;
}

function solve(trie, lines) {
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    const queue = [{ node: trie, index: 0, error: -1 }];
    while (queue.length !== 0) {
      const { node, error, index } = queue.pop();

      // Are we at a terminal node?
      if (node[26] === true) {
        if (error !== -1) {
          return line.substring(0, error) + line.substring(error + 1);
        }
      } else {
        const c = line.charCodeAt(index) - 97;

        // Follow exact match
        if (node[c] !== undefined) {
          queue.push({ node: node[c], index: index + 1, error });
        }

        // Allow exploring different branches
        if (error === -1) {
          for (let j = 0; j < 26; j += 1) {
            const nextNode = node[j];
            if (c !== j && nextNode !== undefined) {
              queue.push({ node: node[j], index: index + 1, error: index });
            }
          }
        }
      }
    }
  }

  return "";
}

const trie = buildTrie(lines);
const t0 = Date.now();
for (let i = 0; i < 100; i += 1) {
  solve(trie, lines);
}
const total = Date.now() - t0;
console.log(total / 100);
console.log(solve(trie, lines));
