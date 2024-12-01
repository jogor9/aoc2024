import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe

chunk n [] = []
chunk n l = let (pref, suf) = splitAt n l in pref : chunk n suf

hist :: (Ord a) => [a] -> [(a, Int)]
hist = sort >>> group >>> map (\l -> (head l, length l))

main =
  getContents
    >>= ( words
            >>> map read
            >>> chunk 2
            >>> transpose
            >>> ( map sort
                    >>> \[a, b] -> zipWith (\l r -> abs $ l - r) a b
                )
              &&& ( \[a, b] ->
                      let dict = hist b
                       in map (\x -> dict & lookup x & fromMaybe 0 & (* x)) a
                  )
            >>> bimap sum sum
            >>> print
        )
