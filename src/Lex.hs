
-- | primitives for lexing
module Lex (tokenize,lookupInDict) where

import Data.List.Extra (lower)
import Dictionary (Dict(..))
import Numbers (Byte,Addr,Zversion(..))

tokenize :: String -> (Byte,[Byte],[String],String) -- TODO: dont return canonicalized
tokenize s = do
  let better = xtokenize s -- TODO: inline
  let offsets = [ offset | Alpha offset _ <- better ]
  let toks = [ tok | Alpha _ tok <- better ]
  let n = length better
  let canonicalized = s ++ "\0" -- TODO: dont add null
  (fromIntegral n,offsets,toks,canonicalized)

data Tok = Alpha Byte String -- TODO: dont need

xtokenize :: String -> [Tok] -- TODO: inline into caller
xtokenize = loop1 (1::Byte) -- TODO: also handle double quotes properly
  where
    loop1 n = \case
      "" -> []
      ',':cs -> Alpha n "," : loop1 (n+1) cs
      ' ':cs -> loopW (n+1) cs
      c:cs -> loopA (n+1) n [c] cs
    loopA n i acc = \case
      "" -> [Alpha i (reverse acc)]
      ',':cs -> Alpha i (reverse acc) : Alpha n "," : loop1 (n+1) cs
      ' ':cs -> Alpha i (reverse acc) : loopW (n+1) cs
      c:cs -> loopA (n+1) i (c:acc) cs
    loopW n = \case
      "" -> []
      ',':cs -> Alpha n "," : loop1 (n+1) cs
      ' ':cs -> loopW (n+1) cs
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
