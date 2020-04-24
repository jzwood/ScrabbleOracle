
testBoard1 = fromLists
  [
    [Square (Nothing, Nothing), Square (Nothing, Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('k', 0)), Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('l', 0)), Nothing), Square (Nothing, Nothing)]
  ]


testBoard2 = fromLists
  [
    [Square (Nothing, Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('k', 0)), Nothing)]
  ]

partialWords =
  [ "AC__"
  , "A__"
  , "C_T"
  , "C__STS"
  ]



wordPlacements = [[Coordinate (0,0)]]
associationStrings = map (\w -> (w, wordPlacements)) partialWords
rack = "ACTSO"

--putStr . show $ calcAdjacencies testBoard1
--putStr . show $ exhaustivePlacements testBoard1

