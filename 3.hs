{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.Maybe

f <||> g = \x -> f x <|> g x

infixr 0 <||>

main =
  getContents
    >>= ( \input ->
            print . (\(s, _, _) -> s) $
              until
                (\(_, i, _) -> null i)
                ( \(acc, s, mul) ->
                    ( stripPrefix "mul("
                        >>> fmap
                          ( reads
                              >>> fmap
                                ( second $
                                    stripPrefix ","
                                      >>> fmap
                                        ( reads
                                            >>> fmap (second $ stripPrefix ")")
                                        )
                                )
                          )
                          <||> stripPrefix "do()"
                        >>> fmap (\r -> [(-1, Just [(1, Just r)])])
                          <||> stripPrefix "don't()"
                        >>> fmap (\r -> [(-1, Just [(-1, Just r)])])
                    )
                      >>> ( \case
                              Just [(-1, Just [(-1, Just r)])] -> (acc, r, False)
                              Just [(-1, Just [(_, Just r)])] -> (acc, r, True)
                              Just [(n, Just [(n', Just r)])] ->
                                (acc + n * n' * fromEnum mul, r, mul)
                              _ -> (acc, drop 1 s, mul)
                          )
                      $ s
                )
                (0, input, True)
        )
