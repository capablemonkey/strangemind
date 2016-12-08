# StrangeMind

[![Build Status](https://travis-ci.com/capablemonkey/mastermind.svg?token=565F651SdURFqsqwp3Jy&branch=master)](https://travis-ci.com/capablemonkey/mastermind)

StrangeMind is a genetic algorithm based AI player for the game of [Mastermind](https://en.wikipedia.org/wiki/Mastermind_(board_game)).  It was written in Common Lisp to solve different variations of the Mastermind game for an arbitrary number of pegs and up to 26 different colors.  It is also designed to account for biased codemaking patterns (referred to as SCSAs, or secret code selection algorithms).

Read our [paper](https://github.com/capablemonkey/strangemind/raw/paper/StrangeMind-paper.pdf) to learn more about the rationale behind the development of this player.

Provided tournament code for the course lives in `lib/game.lisp` and `lib/scsa.lisp`.

This repository also includes an implementation of Knuth's algorithm

We use the Common Lisp style guide: http://lisp-lang.org/style-guide/

## Getting started

We use Clozure Common Lisp.

The `main.lisp` file loads the provided tournament code in mastermind.lisp and plays a sample simple game.

```
ccl --load main.lisp
```

## Testing

We use [prove](https://github.com/fukamachi/prove).  Make sure you install `prove` using Quicklisp, a Common Lisp package manager.

Don't have Quicklisp?  Install it here: https://www.quicklisp.org/beta/

Once installed, remember to configure Quicklisp to auto-load itself:

```
(ql:add-to-init-file)
```

Then install prove:

```
(ql:quickload :prove)
```

To run tests, do this from the repo root directory:

```
ccl --noinform --load test/index.lisp --eval '(quit)'
```

## Benchmarking

To benchmark team parameters (like population size, mutation rate, etc.):

```
ccl --load benchmark/team-parameters.lisp
```

To measure how the team stacks up against different pegs and colors:

```
ccl --load benchmark/pegs-and-colors.lisp
```

To test performance against different SCSAs:

```
ccl --load benchmark/scsas.lisp
```

## Authors

- [Andres Quinones](https://github.com/AJ340)
- [Nishad Sharker](https://github.com/NSharker)
- [Iden Wantanabe](https://github.com/EyeDen)
- [Gordon Zheng](https://github.com/capablemonkey)