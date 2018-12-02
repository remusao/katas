const lines1 = [
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

function solve(lines) {
  const trie = new Array(27);
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    let node = trie;
    for (let j = 0; j < line.length; j += 1) {
      const c = line.charCodeAt(j) - 97;
      if (node[c] === undefined) {
        node[c] = new Array(27);
      }
      node = node[c];
    }
    node[26] = true;
  }

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

function indexOfUniqueDiff(s1, s2) {
  let indexOfDiff = -1;



  return indexOfDiff;
}

function solve_loop(lines) {
  for (let i = 0; i < lines.length; i += 1) {
    const line1 = lines[i];
    for (let j = 0; j < lines.length; j += 1) {
      const line2 = lines[j];
      let indexOfDiff = -1;
      for (let k = 0; k < line1.length; k += 1) {
        if (line1[k] !== line2[k]) {
          // Only allow one diff
          if (indexOfDiff !== -1) {
            indexOfDiff = -1;
            break;
          }
          indexOfDiff = k;
        }
      }

      if (indexOfDiff !== -1) {
        return line1.slice(0, indexOfDiff) + line1.slice(indexOfDiff + 1);
      }
    }
  }
}



function randomLines(n) {
  const letters = 'abcdefghijklmnopqrstuvwxyz';
  const lines = [];
  for (let i = 1; i < n; i += 1) {
    let line = '';
    for (let j = 0; j < 26; j += 1) {
      line += letters[Math.floor(Math.random() * 26)];
    }
    lines.push(line);
  }

  // Make sure there is a duplicate in there
  const line = lines[0];
  lines.push(line.slice(0, 15) + 'u' + line.slice(16));

  return lines;
}

const methods = [
  ['linear', lines => lines.map(l => l.length)],
  ['nlogn', lines => lines.slice().sort()],
  ['trie', solve],
  ['loops', solve_loop],
];

function main() {
  for (let n = 2; n < 100000; n *= 2) {
    console.log(`n=${n}`);
    for (let i = 0; i < methods.length; i += 1) {
      const [name, fn] = methods[i];
      let total = 0.0;
      for (let j = 0; j < 100; j += 1) {
        const lines = randomLines(n);
        const t0 = Date.now();
        fn(lines);
        total += (Date.now() - t0);
      }
      total /= 100;
      console.log(`${name} ${total}ms (${total / n}/str)`);
    }
  }
}

main();
