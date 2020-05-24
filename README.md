# Scrabble Oracle
**Input**:
- Scrabble board part-way through a game
- player's rack

**Output**:
- list of every legal play ranked by point value (descending)

## Algorithm
1. get coordinates of every legal place to play (i.e. physically legal, no dictionary checks yet)
1. pair each of these with their corresponding board string fragment (fragment example: "_t__")
1. group these pairings by word fragment -- now each word fragment will be associated with 1+ playspot coordinates
1. for each fragment and playspot coords find every possible string combo that the input rack can produce
1. remove from list of letter combo/playspot coords any pairing that, if played, produces a word not in the dictionary
1. score all remaining valid playable spots and order from highest point value to lowest
