import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.List

between a b x = a <= x && x <= b

main =
  getContents
    >>= ( lines
            >>> (map . map) (ord >>> subtract (ord '0'))
            >>> zip [0 ..]
            >>> foldr
              ( \(j, l) ->
                  bimap
                    (l :)
                    ( map
                        ((,j) . fst)
                        (filter (snd >>> (== 0)) $ zip [0 ..] l)
                        ++
                    )
              )
              ([], [])
            >>> uncurry
              ( \field ->
                  let len = length field
                      dfs (i, j) = do
                        (i', j') <-
                          [ (i - 1, j),
                            (i + 1, j),
                            (i, j + 1),
                            (i, j - 1)
                            ]
                        guard $ between 0 (len - 1) i'
                        guard $ between 0 (len - 1) j'
                        let n = field !! j !! i
                        guard $ (field !! j' !! i') == n + 1
                        if n + 1 == 9
                          then [(i', j')]
                          else dfs (i', j')
                   in -- delete the nubOrd for part 2 lmao
                      foldr (dfs >>> nubOrd >>> length >>> (+)) 0
              )
            >>> print
        )
