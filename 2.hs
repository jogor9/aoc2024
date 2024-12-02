{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor
import Data.Function

main =
  getContents
    >>= ( lines
            >>> map
              ( words
                  >>> map (read :: String -> Int)
                  >>> let base =
                            ( zipWith (-) <*> drop 1
                                >>> zipWith
                                  ( \a b ->
                                      a * b > 0
                                        && abs a <= 3
                                        && abs b <= 3
                                  )
                                  <*> drop 1
                                >>> and
                            )
                       in base
                            &&& ( \l ->
                                    base l
                                      || ( [0 .. length l - 1]
                                             & any
                                               ( flip splitAt l
                                                   >>> second (drop 1)
                                                   >>> uncurry (++)
                                                   >>> base
                                               )
                                         )
                                )
                            >>> bimap fromEnum fromEnum
              )
            >>> unzip
            >>> bimap sum sum
            >>> print
        )
