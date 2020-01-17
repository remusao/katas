/* eslint-disable no-continue */

const fs = require('fs');

function log(...args) {
  if (true) {
    console.log(...args);
  }
}

function shuffle(array) {
  let currentIndex = array.length;
  let temporaryValue;
  let randomIndex;

  // While there remain elements to shuffle...
  while (currentIndex !== 0) {
    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

const INTER_CACHE = new Map();

function intersection(s1, s2) {
  const k1 = `${s1} ${s2}`;
  const c1 = INTER_CACHE.get(k1);
  if (c1 !== undefined) {
    return c1;
  }

  const k2 = `${s2} ${s1}`;
  const c2 = INTER_CACHE.get(k2);
  if (c2 !== undefined) {
    return c2;
  }

  let r = 0;
  if (s1.size < s2.size) {
    s1.forEach((e) => {
      if (s2.has(e)) {
        r += 1;
      }
    });
    INTER_CACHE.set(k1, r);
  } else {
    s2.forEach((e) => {
      if (s1.has(e)) {
        r += 1;
      }
    });
    INTER_CACHE.set(k2, r);
  }

  return r;
}

const DIFF_CACHE = new Map();

// In s1 but not s2
function diff(s1, s2) {
  const k = `${s1} ${s2}`;
  const c = DIFF_CACHE.get(k);
  if (c !== undefined) {
    return c;
  }

  let r = 0;
  s1.forEach((e) => {
    if (!s2.has(e)) {
      r += 1;
    }
  });

  DIFF_CACHE.set(k, r);
  return r;
}

function sortTags(index) {
  return [...index.entries()].sort(([, t1], [, t2]) => {
    if (t1.size < t2.size) {
      return -1;
    }
    if (t1.size > t2.size) {
      return 1;
    }
    return 0;
  });
}

function assert(b, msg) {
  if (!b) {
    console.error(msg);
    throw new Error('Fail');
  }
}

function validate(slides, hPics, vPics) {
  const used = new Set();

  for (let i = 0; i < slides.length; i += 1) {
    const pics = slides[i]
      .trim()
      .split(/\s+/g)
      .map(id => parseInt(id, 10));

    assert(pics.length !== 0, 'needs at least one pic per slide');
    if (pics.length === 1) {
      assert(hPics[pics[0]] !== undefined, 'slide with one pic must be horizontal');
      assert(!used.has(pics[0]), 'pic already used');
      used.add(pics[0]);
    } else {
      assert(pics.length === 2, 'max 2 pics per slides');
      assert(vPics[pics[0]] !== undefined, 'slide with two pics must be vertical');
      assert(vPics[pics[1]] !== undefined, 'slide with two pics must be vertical');
      assert(!used.has(pics[0]), 'pic already used');
      assert(!used.has(pics[1]), 'pic already used');
      used.add(pics[0]);
      used.add(pics[2]);
    }
  }
}

async function main() {
  const file = process.argv[process.argv.length - 1];
  log(`Load ${file}`);
  const lines = fs.readFileSync(file, { encoding: 'utf-8' }).split(/\n/g);

  const n = parseInt(lines[0], 10);
  log(`N ${n}`);

  const hPics = new Map();
  const vIndex = new Map();
  const idToSlide = new Map();

  const hIndex = new Map();
  const vPics = new Map();

  const allTags = new Set();

  const updateIndex = (index, tags, id) => {
    // log(i, orientation, tags);
    for (let i = 0; i < tags.length; i += 1) {
      const tag = tags[i];
      allTags.add(tag);
      if (!index.has(tag)) {
        index.set(tag, new Set([id]));
      } else {
        index.get(tag).add(id);
      }
    }
  };

  for (let j = 1; j < lines.length; j += 1) {
    const line = lines[j];
    const parts = line.split(/\s+/g);
    const orientation = parts[0];
    const tags = parts.slice(2);
    const id = j - 1;
    if (orientation === 'H') {
      idToSlide.set(id, `${id}`);
      hPics.set(id, new Set(tags));
      updateIndex(hIndex, tags, id);
    } else {
      vPics.set(id, new Set(tags));
      updateIndex(vIndex, tags, id);
    }
  }

  // List of all tags
  const atags = [...allTags];

  // const sortedVTags = sortTags(vIndex);
  const sortedVTags = [['foo', [...vPics.keys()]]];
  const remaining = [];

  const vUse = (id) => {
    // console.log('USE', id);
    vPics.get(id).forEach((tag) => {
      // console.log('REMOVE', id, tags[i]);
      vIndex.get(tag).delete(id);
    });
  };

  for (let i = 0; i < sortedVTags.length; i += 1) {
    const elt = sortedVTags[i];
    // const tag = elt[0];
    const pics = [...elt[1]];
    // console.log('???', pics.length);

    while (pics.length >= 2) {
      const p1 = pics.pop();
      const p2 = pics.pop();
      vUse(p1);
      vUse(p2);
      idToSlide.set(p2, `${p1} ${p2}`);
      const newTags = new Set([...vPics.get(p1), ...vPics.get(p2)]);
      hPics.set(p2, newTags);
      updateIndex(hIndex, [...newTags], p2);
    }

    pics.forEach((id) => {
      remaining.push(id);
    });
  }
  console.log(remaining.length);

  log(`hPics ${hPics.size}`);
  log(`Tags ${hIndex.size}`);

  // const sortedHTags = [...hIndex.entries()];
  // const sortedVTags = sortTags(vIndex);
  const sortedHTags = sortTags(hIndex).reverse();
  const slides = []; // solution

  // console.log('hPICS', hPics);
  // console.log('vPICS', vPics);

  // Consume pic
  const hUse = (id) => {
    // console.log('USE', id);
    hPics.get(id).forEach((tag) => {
      // console.log('REMOVE', id, tags[i]);
      hIndex.get(tag).delete(id);
    });
  };

  console.log(sortedHTags);
  let total = 0;
  while (sortedHTags.length !== 0) {
    const elt = sortedHTags.pop();
    // const tag = elt[0];
    const ps = [...elt[1]];

    // for (let i = 0; i < 1; i += 1) {
    //   if (sortedHTags.length === 0) {
    //     break;
    //   }
    //   const elt2 = sortedHTags.pop();
    //   ps.push(...elt2[1]);
    // }

    const pics = [...new Set(ps)];

    console.log('>>>', sortedHTags.length, pics.length);

    if (pics.length === 0) {
      continue;
    }

    // console.log('>> TAGS', tag, pics);

    const pic = pics.pop();
    // console.log('>>>>1', pic);
    slides.push(idToSlide.get(pic));
    hUse(pic);
    let tags1 = hPics.get(pic);

    while (pics.length !== 0) {
      // console.log(pics.length);
      let bestTags = new Set();
      let bestS = 0;
      let bestP = pics[0];
      let bestJ = 0;
      let j = 0;
      for (; j < pics.length; j += 1) {
        const tags = hPics.get(pics[j]);
        const p1 = intersection(tags1, tags);
        if (p1 === 0) {
          continue;
        }
        const p2 = diff(tags1, tags);
        if (p2 === 0) {
          continue;
        }
        const p3 = diff(tags, tags1);
        if (p3 === 0) {
          continue;
        }
        const score = Math.min(p1, p2, p3);
        if (score >= bestS) {
          bestJ = j;
          bestS = score;
          bestP = pics[j];
          bestTags = tags;
        }
      }
      // console.log('SCORE', bestS);
      // console.log('>>>', bestS, bestP);

      tags1 = bestTags;
      pics.splice(bestJ, 1);
      // console.log('>>>>', bestP);
      slides.push(idToSlide.get(bestP));
      hUse(bestP);
      total += bestS;
    }

    // TODO - try to insert a slide with vertical pics
    // if (vIndex.has(tag) && vIndex.get(tag).size !== 0) {
    //   console.log('Insert v?');
    // }
  }

  // TODO - insert remaining vPics?
  // console.log(sortedVTags);
  // for (let i = 0; i < sortedVTags.length; i += 1) {
  //   const [tag, pics] = sortedVTags[i];
  // }
  console.log(total);

  // TODO
  const solution = `${slides.length}\n${slides.join('\n')}\n`;
  // validate(slides, hPics, vPics);
  fs.writeFileSync(`${file}.output`, solution, {
    encoding: 'utf-8',
  });
}

main();
