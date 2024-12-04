import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals [[x]] = [[x]]
diagonals (xs : ys) =
  let (zs, ms) = unzip $ map (fromJust . uncons) ys
      (zs', [z]) = splitAt (length zs - 1) zs
      (xs', [x]) = splitAt (length xs - 1) xs
   in [z] : zipWith (:) (reverse zs' ++ xs') (diagonals ms) ++ [[x]]

main =
  getContents
    >>= ( lines
            >>> ( let cr =
                        map
                          ( tails
                              >>> map
                                ( ("XMAS" `isPrefixOf`)
                                    &&& ("SAMX" `isPrefixOf`)
                                    >>> bimap fromEnum fromEnum
                                    >>> uncurry (+)
                                )
                              >>> sum
                          )
                          >>> sum
                   in cr
                        &&& (transpose >>> cr)
                        &&& (diagonals >>> cr)
                        &&& (reverse >>> diagonals >>> cr)
                        >>> (\(a, (b, (c, d))) -> a + b + c + d)
                )
              &&& ( \l ->
                      length
                        [ undefined
                          | i <- [1 .. length l - 2],
                            j <- [1 .. length l - 2],
                            l !! j !! i == 'A',
                            abs
                              ( fromEnum (l !! (j - 1) !! (i - 1))
                                  - fromEnum (l !! (j + 1) !! (i + 1))
                              )
                              == fromEnum 'S' - fromEnum 'M'
                              && abs
                                ( fromEnum (l !! (j + 1) !! (i - 1))
                                    - fromEnum (l !! (j - 1) !! (i + 1))
                                )
                                == fromEnum 'S' - fromEnum 'M'
                        ]
                  )
            >>> print
        )
