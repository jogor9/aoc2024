import Control.Arrow ((>>>))
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Function
import Data.List
import Debug.Trace

pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

between :: (Ord a) => a -> a -> a -> Bool
between a b x = a <= x && x <= b

main =
  getContents
    >>= ( lines
            >>> ( \m ->
                    let n = length m
                     in [ (x, i, j)
                          | i <- [0 .. n - 1],
                            j <- [0 .. n - 1],
                            let x = m !! j !! i,
                            x /= '.'
                        ]
                          & sortOn (\(x, _, _) -> x)
                          & groupBy ((==) `on` \(x, _, _) -> x)
                          & concatMap
                            ( pairs
                                >>> concatMap
                                  ( \((_, x0, y0), (_, x1, y1)) ->
                                      let dx = x1 - x0
                                          dy = y1 - y0
                                          filt =
                                            takeWhile (uncurry ((&&) `on` between 0 (n - 1)))
                                              . map (\k -> (x0 + dx * k, y0 + dy * k))
                                       in filt [-1, -2 ..] ++ filt [0 ..]
                                  )
                            )
                )
            >>> nubOrd
            >>> length
            >>> print
        )
