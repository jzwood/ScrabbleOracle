# Scrabble Oracle

### Problem
How do we algorithmically find the highest scoring word a Scrabble player can make on their turn?

### Facts
- Board - 255 playable squares
- Rack - 7 tiles
- There are 1,200 unique ways to play word
- English Dictionary contains over 300,000 words

### Naive Approach
For each 1,200 ways to play a word we could scan the entire dictionary and ask the question, given this play spot and this rack could we spell this entry in the dictionary?

*Example:*
```
Rack: A, A, E, G, R, W, Y
Spot: _R__
Dict Word: AREA

Can make word?
YES
```

```
Rack: A, A, E, G, R, W, Y
Spot: _R__
Dict Word: AARDVARK

Can make word?
NO
```

This approach would take on the order of ~360M operations just to produce the list of valid playable words. It does not include scoring or ranking.

There are optimizations one could apply to this strategy but none sufficiently powerful enough to justify the approach. The following attempts to sketch the basic steps in the algorithm used in Scrabble-Oracle which is performant enough to play and entire game in well under a minute.

## Algorithm
1. get coordinates of every legal place to play (i.e. physically legal, no dictionary checks yet)
1. pair each of these with their corresponding board string fragment (fragment example: `_R__`)
1. group these pairings by word fragment – now each word fragment will be associated with 1+ playspot coordinates
1. for each fragment and playspot coords find every possible string combo that the input rack can produce (implementation uses [trie](https://en.wikipedia.org/wiki/Trie))
1. remove from list of letter combo/playspot coords any pairing that, if played, produces a word not in the dictionary
1. score all remaining valid playable spots and order from highest point value to lowest


## Run locally
**Prereq:**
- [stack](https://docs.haskellstack.org/en/stable/README/)

To run Scrabble AI vs AI:
```
stack build
stack exec scrabble-oracle-exe
```
