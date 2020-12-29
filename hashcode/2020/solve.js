const fs = require("fs");
const assert = require("assert").strict;

const debug = process.env.PRODUCTION === undefined;
function log(...args) {
  if (debug === true) {
    console.log(...args);
  }
}

/**
 * Shuffles array in place.
 */
function shuffle(a) {
  let j;
  let x;
  let i;
  for (i = a.length - 1; i > 0; i--) {
    j = Math.floor(Math.random() * (i + 1));
    x = a[i];
    a[i] = a[j];
    a[j] = x;
  }
  return a;
}

// ======================================================================== //
// PARSE INPUT
// ======================================================================== //

function parse(file) {
  const content = fs.readFileSync(file, { encoding: "utf-8" });
  const lines = content.split(/\n/g);

  // 1 <= X <= 10^5
  const [B, L, D] = lines[0].split(" ").map(d => parseInt(d, 10));

  // S0, ... SB
  const books = lines[1].split(" ").map(d => parseInt(d, 10));

  const libraries = [];
  let index = 2;
  let currentLibraryId = 0;
  while (index < lines.length - 2) {
    const [N, T, M] = lines[index++].split(" ").map(d => parseInt(d, 10));
    libraries.push({
      id: currentLibraryId++,

      N,
      numberOfBooks: N,

      T,
      numberOfDaysForSignup: T,

      M,
      numberOfBooksPerDay: M,

      // Sort books per decreasing score
      books: lines[index++]
        .split(" ")
        .map(d => parseInt(d, 10))
        .sort((b1, b2) => books[b2] - books[b1])
    });
  }

  // log({ B, L, D, books, libraries });
  // TODO
  const input = {
    B,
    numberOfBooks: B,

    L,
    numberOfLibraries: L,

    D,
    numberOfDays: D,

    books, // scores for each book
    libraries
  };

  log(JSON.stringify(input, null, 2));

  return input;
}

// ======================================================================== //
// FIND SOLUTION
// ======================================================================== //

function solve(input) {
  // Score libraries + sort
  // for (const library of input.libraries) {
  //   // Compute cumulative score given number of remaining days
  //   const cumul = [];
  // }

  const seenBooks = new Set();
  const libraries = [...input.libraries];
  // for (const library of libraries) {
  //   library.books = [...library.books];
  // }

  const counter = {};
  for (const library of libraries) {
    for (const book of library.books) {
      counter[book] = (counter[book] || 0) + 1;
    }
  }

  let remainingDays = input.numberOfDays;

  const computeCumul = () => {
    for (const library of libraries) {
      const budget = library.numberOfBooksPerDay * (remainingDays - library.numberOfDaysForSignup);
      let p = 0;
      let total = 0;
      let usedBooks = 0;
      for (const bookId of library.books) {
        if (seenBooks.has(bookId) === false) {
          total += input.books[bookId]; //  / counter[bookId];
          usedBooks += 1;
          p += counter[bookId];
          if (usedBooks >= budget) {
            break;
          }
        }
      }
      // p /= usedBooks;
      library.cumul = total / (library.numberOfDaysForSignup);
      // library.cumul = total / (library.numberOfDaysForSignup ** 0.8);
    }
  };

  // const score = (l1, l2) => l2.cumul - l1.cumul;
  //   l2.cumul /
  //     (l2.numberOfDaysForSignup + l2.numberOfBooks / l2.numberOfBooksPerDay) -
  //   l1.cumul /
  //     (l1.numberOfDaysForSignup + l1.numberOfBooks / l1.numberOfBooksPerDay);

  // const sortLibraries = () => {
  //   libraries.sort(score);
  // };

  computeCumul();
  // sortLibraries();

  const seenLibraries = new Set();
  let lastSort = 0;
  let pickedLibrarySinceLastSort = false;
  const librariesQueued = [];
  let currentLibraryCandidate = 0;
  let lastCumulSum = 0;
  while (remainingDays > 0) {
    const t0 = Date.now();
    if (lastCumulSum === 1) {
      // log('> remainingDays', remainingDays);
      computeCumul();
      // sortLibraries();
      lastCumulSum = 0;
    }

    lastCumulSum += 1;
    // const t1 = Date.now();
    // log('>>> remaining libraries', libraries.length);

    log('> remainingDays', remainingDays);
    let currentBestLibrary;
    let currentBestScore = 0;
    for (let i = 0; i < libraries.length; i += 1) {
      const l = libraries[i];
      const s = l.cumul; // / (l.numberOfDaysForSignup + l.numberOfBooks / l.numberOfBooksPerDay);
      if (s > currentBestScore) {
        // console.log(' + s', s);
        currentBestScore = s;
        currentBestLibrary = l;
      }
    }

    // const t2 = Date.now();
    // log('cumul time', t1 - t0);
    // log('lookup time', t2 - t1);

    // sortLibraries();
    // currentLibraryCandidate = 0;

    // log('? remaining', remainingDays);
    // if (lastSort >= 50) {
    //   lastSort = 0;
    //   computeCumul();
    //   sortLibraries();
    // }

    if (currentBestLibrary === undefined) {
      break;
    }

    if (currentLibraryCandidate >= libraries.length) {
      break; // no more libraries
    }

    const library = currentBestLibrary; // libraries[currentLibraryCandidate];
    if (remainingDays < library.numberOfDaysForSignup + 1) {
      log(">>> no more time for more libraries", remainingDays);
      break; // no more time for another library
    }

    // log(" + select", library.id, library.numberOfBooks, remainingDays);

    // Remove scanned books
    const remainingBooks = library.books.filter(b => {
      if (seenBooks.has(b)) {
        return false;
      }

      seenBooks.add(b);
      return true;
    });
    // console.log('!', { signup: library.numberOfDaysForSignup, books: remainingBooks.length });

    if (remainingBooks.length > 0) {
      librariesQueued.push([
        library.id,
        Math.min(
          remainingBooks.length,
          library.numberOfBooks, // best case we can scan all books
          (remainingDays - library.numberOfDaysForSignup) *
            library.numberOfBooksPerDay // or we send one book per day for remaining of simulation
        ),
        remainingBooks
      ]);
      // Subtract sign-up for next library from remaining time
      remainingDays -= library.numberOfDaysForSignup;
    }

    // log('splice', currentLibraryCandidate);
    if (currentLibraryCandidate === 0) {
      libraries.shift();
    } else {
      libraries.splice(currentLibraryCandidate, 1);
    }
  }

  // TODO

  const output = [];
  output.push([librariesQueued.length]);
  for (let i = 0; i < librariesQueued.length; i += 1) {
    const [id, numberOfBooksToSend, books] = librariesQueued[i];
    output.push([id, numberOfBooksToSend]);
    output.push(books.slice(0, numberOfBooksToSend));
  }
  return output;

  // return [
  //   [2], // number of libraries to sign up
  //   [1, 2], // sign-up of library 1 then send 3 books
  //   [5, 2, 3], // IDs of books to send to scanning from library 1
  //   [0, 5], // sign-up of library 0 then send 5 books
  //   [0, 1, 2, 3, 4], // books to send from library 0
  // ];
}

// ======================================================================== //
// VALIDATE SOLUTION
// ======================================================================== //

function validate(input, solution) {
  const [numberOfLibraries] = solution[0];
  const librariesQueued = [];
  let index = 1;
  while (index < solution.length) {
    const [libraryId, numberOfBooksToSend] = solution[index++];
    const booksToSend = solution[index++];
    librariesQueued.push({
      libraryId,
      numberOfBooksToSend,
      booksToSend,
      currentBookIndex: 0
    });
  }
  // log(librariesQueued);

  const seenBooks = new Set();

  if (librariesQueued.length === 0) {
    return 0;
  }

  let score = 0;

  // Keep track of signing up
  let signingUpIndex = 0;
  let signingUpEnds =
    input.libraries[librariesQueued[0].libraryId].numberOfDaysForSignup;

  // Keep track of signed-up libraries
  const signedUpLibraries = [];
  for (let i = 0; i < input.numberOfDays; i += 1) {
    // Library is signed-up and we can start another one
    // log(i, '>>>', 'lib signing-up', librariesQueued[signingUpIndex].libraryId);
    if (signingUpEnds === i) {
      // log(i, '!!! lib signed-up', librariesQueued[signingUpIndex].libraryId);

      // It will start sending a book this turn of the simulation
      signedUpLibraries.push(librariesQueued[signingUpIndex]);

      // Check if another library could start signing-up now
      signingUpIndex += 1;
      // log('????', signingUpIndex, librariesQueued.length);
      if (signingUpIndex < librariesQueued.length) {
        signingUpEnds +=
          input.libraries[librariesQueued[signingUpIndex].libraryId]
            .numberOfDaysForSignup;
        // console.log('Will end at', signingUpEnds)
      }
    }

    // For each signed-up library we can send a book
    for (let j = 0; j < signedUpLibraries.length; j += 1) {
      const {
        libraryId,
        numberOfBooksToSend,
        booksToSend,
        currentBookIndex
      } = signedUpLibraries[j];

      if (currentBookIndex < booksToSend.length) {
        const nextBook = booksToSend[currentBookIndex];
        signedUpLibraries[j].currentBookIndex += 1;
        if (seenBooks.has(nextBook) === false) {
          seenBooks.add(nextBook);
          score += input.books[nextBook];
          // log(i, '+ score', nextBook, input.books[nextBook]);
        }
      }
    }
  }

  // log('Final score', score);
  return score;
  // TODO - lots of asserts
}

// ======================================================================== //
// WRITE SOLUTION ON DISK
// ======================================================================== //

function writeSolution(file, solution) {
  let output = "";
  for (const line of solution) {
    output += line.join(" ");
    output += "\n";
  }
  fs.writeFileSync(`${file}.output`, output, {
    encoding: "utf-8"
  });
}

// ======================================================================== //
// ENTRY POINT
// ======================================================================== //

(() => {
  // > parse inputs from file
  const file = process.argv[process.argv.length - 1];
  log(`Load ${file}`);
  const input = parse(file);

  // > solve inputs
  const solution = solve(input);

  // > validate solution
  log(validate(input, solution));

  // If we reach this point then our solution is valid and we can dump it
  writeSolution(file, solution);
})();
