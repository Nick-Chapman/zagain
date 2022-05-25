
-- | Text manipulation needed when running z-interaction afor Walkthough or Console
module TextDisplay (lineWrap) where

lineWrap :: Int -> String -> String
lineWrap max text =
  unlines [ lineWrapOneLine line | line <- lines text ]
  where
    lineWrapOneLine line = do
      let (wsIndent,remainder) = span (== ' ') line
      case words remainder of
        [] -> []
        w1:ws -> do
          let indentedW1 = wsIndent ++ w1
          loop [indentedW1] (length indentedW1) ws
          where
            loop :: [String] -> Int-> [String] -> String
            loop acc pos = \case
              [] -> concat (reverse acc)
              w:ws -> do
                let indentedW = wsIndent ++ w
                let posW = pos + length w + 1
                let makeSplit = posW > max
                let pos' = if makeSplit then length indentedW else posW
                let sep = if makeSplit then "\n"++wsIndent else " "
                loop (w : sep : acc) pos' ws
