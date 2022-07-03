
-- | primitives for lexing
module Lex (tokenize,lookupInDict) where

import Data.List.Extra (lower)
import Dictionary (Dict(..))
import Numbers (Byte,Addr,Zversion(..))

data Tok = Tok Byte String

tokenize :: Dict -> String -> (Byte,[Byte],[String])
tokenize Dict{seps} s = do

  let toks = loop1 1 s
  let offsets = [ offset | Tok offset _ <- toks ]
  let words = [ tok | Tok _ tok <- toks ]
  let n = fromIntegral $ length toks

  (n,offsets,words)

  where
    isSep = (`elem` seps)

    loop1 n = \case
      "" -> []
      ' ':cs -> loopW (n+1) cs
      c:cs | isSep c -> Tok n [c] : loop1 (n+1) cs
      c:cs -> loopA (n+1) n [c] cs

    loopA n i acc = \case
      "" -> [Tok i (reverse acc)]
      ' ':cs -> Tok i (reverse acc) : loopW (n+1) cs
      c:cs | isSep c -> Tok i (reverse acc) : Tok n [c] : loop1 (n+1) cs
      c:cs -> loopA (n+1) i (c:acc) cs

    loopW n = \case
      "" -> []
      ' ':cs -> loopW (n+1) cs
      c:cs | isSep c -> Tok n [c] : loop1 (n+1) cs
      c:cs -> loopA (n+1) n [c] cs


lookupInDict :: Dict -> String -> Addr
lookupInDict dict word = do
  let Dict{zv,base,seps,entryLength,strings} = dict
  --let n = if zv <= Z3 then 6 else 6 -- THE BUG
  let n = if zv <= Z3 then 6 else 9 -- bugfix for "open wardrobe" failing in judo
  let key = lower (take n word)
  case [ i | (i,s) <- zip [1..] strings, s == key ] of
    [] -> 0
    xs@(_:_:_) -> error (show ("multi dict match!",word,xs))
    [i] -> do
      let offset = length seps + 4 -- +4 : #seps byte, entryLength byte, #entries word
      let baseEntries = fromIntegral base + offset
      fromIntegral (baseEntries + (i-1) * entryLength)
