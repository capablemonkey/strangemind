# StrangeMind

[![Build Status](https://travis-ci.com/capablemonkey/mastermind.svg?token=565F651SdURFqsqwp3Jy&branch=master)](https://travis-ci.com/capablemonkey/mastermind)

Let's win this.

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