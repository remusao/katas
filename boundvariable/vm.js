#!/usr/bin/env node

/* eslint-disable no-console, no-bitwise, operator-linebreak */

const fs = require('fs');

// TODO - read one char
// TODO - write one char

function failure(...args) {
  console.error('Failure:', ...args);
  process.exit();
}

function log(...args) {
  console.log(...args);
}

class CopyOnWrite {
  constructor(array) {
    let buffer = array;
    this.byteLength = buffer.byteLength;
    this.get = n => buffer[n];

    // Copy-on-write `set`
    this.set = (...args) => {
      buffer = new Uint8Array(buffer);
      this.set = (n, val) => { buffer[n] = val; };
      this.set(...args);
    };
  }
}

function run(initialProgram) {
  // 8 registers
  const registers = new Uint32Array(8);

  // collection of arrays of platters
  const memory = new Map();

  // execution finger
  let pc = 0;
  let program = new CopyOnWrite(initialProgram);

  // let n = 0;
  while ((pc + 4) < program.byteLength) {
    // n += 1;
    // if (n % 10000000 === 0) {
    //   console.log('#', n);
    // }
    log(
      'Decode operator from 4 next bytes',
      pc,
      program.get(pc),
      program.get(pc + 1),
      program.get(pc + 2),
      program.get(pc + 3),
    );

    const op = (
      ((program.get(pc) << 24) >>> 0) +
      ((program.get(pc + 1) << 16) |
       (program.get(pc + 2) << 8) |
        program.get(pc + 3))
    ) >>> 0;
    // log('Found operator', op);

    // Registers
    const firstByte = program.get(pc);
    const a = (op & 448) >>> 6;
    const b = (op & 56) >>> 3;
    const c = op & 7;
    const specialReg = (firstByte & 14) >>> 1;

    // log('Exec', op, firstByte.toString(2), op.toString(2), a, b, c);
    pc += 4;

    switch (firstByte >>> 4) {
      case 0:
        log('move');
        if (registers[c] !== 0) {
          log('perform move');
          registers[a] = registers[b];
        }
        break;
      case 1:
        log('index');

        if (memory.has(registers[b])) {
          registers[a] = memory.get(registers[b])[registers[c]];
        } else {
          failure('array does not exist', registers[b]);
          return;
        }
        break;
      case 2:
        log('amendment');

        if (memory.has(registers[a])) {
          memory.get(registers[a])[registers[b]] = registers[c];
        } else {
          failure('array does not exist', registers[a]);
          return;
        }
        break;
      case 3:
        log('addition');
        registers[a] = (registers[b] + registers[c]) % 4294967296;
        break;
      case 4:
        log('multiplication');
        registers[a] = (registers[b] * registers[c]) % 4294967296;
        break;
      case 5:
        log('division');

        if (registers[c] === 0) {
          failure('division by 0');
          return;
        }

        registers[a] = Math.floor(registers[b] / registers[c]);
        break;
      case 6:
        log('no-and');
        registers[a] = !(registers[b] & registers[c]);
        break;
      case 7:
        log('halt');
        return;
      case 8: {
        log('allocation');
        let nextAvailable = 1;
        while (memory.has(nextAvailable)) {
          nextAvailable += 1;
        }
        memory.set(nextAvailable, new Uint8Array(registers[c]));
        registers[b] = nextAvailable;
        break;
      }
      case 9:
        log('abandonment');
        if (registers[c] === 0) {
          failure('abandoning 0 array');
          return;
        }
        memory.delete(registers[c]);
        break;
      case 10:
        log('output');
        if (registers[c] > 255) {
          failure('output invalid char', registers[c]);
          return;
        }

        process.stdout.write(String.fromCharCode(registers[c]));
        break;
      case 11:
        log('input');
        console.error('INPUT NOT IMPLEMENTED'); return;
        break;
      case 12: {
        log('load program');
        if (registers[b] === 0) { pc = registers[c]; break; }

        if (!memory.has(registers[b])) {
          failure('loading non-existant program', registers[b]);
          return;
        }

        program = new CopyOnWrite(memory.get(registers[b]).buffer);
        pc = registers[c];
        break;
      }
      case 13: {
        log('orthography');
        registers[specialReg] = op & 33554431;
        break;
      }
      default:
        failure('unknown operator', op);
        return;
    }
  }

  // Failue if `pc` is out of the program
  if ((pc + 4) >= program.byteLength) {
    console.error('Failue: out of bound', pc);
    return;
  }

  console.log('Done.');
}

function main() {
  if (process.argv.length !== 3) {
    console.error('Expected one argument: <program>');
  } else {
    const program = new Uint8Array(fs.readFileSync(process.argv[2]).buffer);
    // log('Program', program);
    run(program);
  }
}

main();
