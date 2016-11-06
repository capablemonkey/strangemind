10 colors
10 pegs

answer
98765 43210
              b c
00000 00000 = 1 0
01111 11111 = 1 1
20222 22222 = 1 1
33033 33333 = 1 1
44404 44444 = 1 1
55550 55555 = 0 2
66665 06666 = 2 1
77775 70777 = 2 1
88885 88088 = 2 1
99995 99909 = 2 1
12225 22220 = 




- maybe worthwhile to initially enumerate all valid colors when there are many many color: 
  10colors / 10pegs: 10^10 and 100 moves vs 10P10 and (100-90) moves
  26colors / 10pegs: 26^10 and 100 moves vs 26P10 and (100-26) moves
- use regexes to represent SCSA patterns
- we can't assume we know the SCSA after one match.  We have to keep a confidence, which grows with each game.  Probabilistic.
- maybe we can use CSP and arc consistentcies for the genetic algorithm's fitness function
- fitness function is very important -- this is where we'll make the GA smarter
- we can generate initial population using an SCSA if we are confident that it is the SCSA.
- OR, generate the initial population with a distribution of probabilities of each SCSA
- need to find optimal population size
- buckets of SCSAs, tally answers against them to generate probability distribution
- use a number of turns relative to the board/color size to play guesses which we use to build a set of constraints which we can use to narrow the domains of answers

things to tune:
- mutation rate
- optimal population size
- crossover point

TODO for next week:
- write functions to detect the given SCSAs
- identify patterns for secret SCSAs
- build a simple genetic algorithm team