import Control.Arrow ((>>>))
import Data.Function
import Data.List
import Debug.Trace

gen :: (Num a) => [a -> a -> a] -> Int -> [[a -> a -> a]]
gen ops 1 = foldr ((:) . pure) [] ops
gen ops n =
  let g = gen ops $ n - 1
   in concatMap (\op -> map (op :) g) ops

op :: [Int -> Int -> Int] -> [Int] -> Int
op _ [x] = x
op (f : fs) (x : y : xs) = op fs (f x y : xs)

digitCat :: Int -> Int -> Int
digitCat a b = show a ++ show b & read

main =
  getContents
    >>= ( lines
            >>> map
              ( words
                  >>> (\(t : xs) -> (read $ takeWhile (/= ':') t, map read xs))
                  >>> \(v :: Int, xs :: [Int]) ->
                    let cs = gen [(+), (*), digitCat] (length xs - 1)
                     in map (`op` xs) cs & (v `elem`) & fromEnum & (* v)
              )
            >>> sum
            >>> print
        )
