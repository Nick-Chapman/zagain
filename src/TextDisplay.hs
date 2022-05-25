
-- | Text manipulation needed when running z-interaction afor Walkthough or Console
module TextDisplay (lineWrap) where

lineWrap :: Int -> String -> String
lineWrap max text =
  unlines [ layout max (tokenize line) | line <- lines text ]

data Tok = White String | Alpha String

tokenize :: String -> [Tok]
tokenize = \case
  "" -> []
  c:cs | c ==' ' -> loopW [c] cs
       | otherwise -> loopA [c] cs
  where
    loopA acc = \case
      "" -> [Alpha (reverse acc)]
      ' ':cs -> Alpha (reverse acc) : loopW [' '] cs
      '-':cs -> loopH ('-':acc) cs
      c:cs -> loopA (c:acc) cs
    loopH acc = \case
      "" -> [Alpha (reverse acc)]
      ' ':cs -> Alpha (reverse acc) : loopW [' '] cs
      '-':cs -> loopH ('-':acc) cs
      c:cs-> Alpha (reverse acc) : loopA [c] cs
    loopW acc = \case
      "" -> [White (reverse acc)]
      ' ':cs -> loopW (' ':acc) cs
      c:cs -> White (reverse acc) : loopA [c] cs

layout :: Int -> [Tok] -> String
layout max = \case
  [] -> ""
  Alpha s : toks -> s ++ loop (length s) toks
  [White _] -> ""
  toks -> loop 0 toks
  where
    loop pos = \case
      [] -> []
      [White _] -> ""
      White{} : White{} : _ -> error "layout: White/White"
      White w : Alpha s : toks -> do
        let pos' = pos + length w + length s
        if | pos' > max -> "\n" ++ s ++ loop (length s) toks
           | otherwise -> w ++ s ++ loop pos' toks
      Alpha s : toks -> do
        let pos' = pos + length s
        if | pos' > max -> "\n" ++ s ++ loop (length s) toks
           | otherwise -> s ++ loop pos' toks
