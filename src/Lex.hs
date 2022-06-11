
-- | primitives for lexing
module Lex (tokenize,lookupInDict) where

import Data.List (intercalate)
import Data.List.Extra (lower)
import Data.List.Split (splitOn)
import Dictionary (Dict(..))
import Numbers (Byte,Addr)

tokenize :: String -> (Byte,[(Byte,String)],String)
tokenize str = do
  let toks = [ w | w <- splitOn " " str, w /= "" ]
  let
    offsets :: [Byte] = do
      let lens :: [Byte] = [ fromIntegral (length w) | w <- toks ]
      let
        f :: (Byte,[Byte]) -> Byte -> (Byte,[Byte])
        f (off,xs) i = (off+i+1, off : xs) --
      let z = (1,[])
      let (_,offsetsR) = foldl f z lens
      reverse offsetsR
  -- TODO: Better if we didn't change inter-word whitespace
  let canonicalized = intercalate " " toks -- TODO: what needs this? ++ "\0"
  let positionedWords = zip offsets toks
  let n = fromIntegral (length positionedWords)
  (n, positionedWords, canonicalized)

lookupInDict :: Dict -> String -> Addr
lookupInDict dict word = do
  let Dict{base,seps,entryLength,strings} = dict
  let key = lower (take 6 word)
  case [ i | (i,s) <- zip [1..] strings, s == key ] of
    [] -> 0
    xs@(_:_:_) -> error (show ("multi dict match!",word,xs))
    [i] -> do
      let offset = length seps + 4 -- +4 : #seps byte, entryLength byte, #entries word
      let baseEntries = fromIntegral base + offset
      fromIntegral (baseEntries + (i-1) * entryLength)
