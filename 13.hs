import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.List
import Debug.Trace (traceShowId)

split _ [] = []
split delim xs = break (== delim) >>> second (drop 1 >>> split delim) >>> uncurry (:) $ xs

-- a * m + b * n = p
-- ax * m + bx * n = px
-- ay * m + by * n = py
-- m = (py - by * n) / ay
-- ax * (py - by * n) / ay + bx * n = px
-- (ax * py - ax * by * n + ay * bx * n - px * ay) / ay = 0
-- (ay * bx - ax * by) / ay * n = (ax * py - px * ay) / ay
-- n = (ax * py - px * ay) / (ay * bx - ax * by)
--
-- 1 / (ax * by - bx * ay)
--
-- x' = (by -
--
-- b * n + a * m = p
--
-- set q = b * min(p div b)
-- while (p - q) mod a /= 0 and p >= q decrease q by b
-- solution is q div b + 3 * (p - q) div a

main =
  getContents
    >>= ( lines
            >>> split ""
            >>> map
              ( map words >>> \[a, b, p] ->
                  (a, b)
                    & join
                      bimap
                      ( drop 2
                          >>> map (drop 2 >>> takeWhile isDigit >>> read)
                      )
                    & (,p)
                    & second
                      ( drop 1
                          >>> map (drop 2 >>> takeWhile isDigit >>> read >>> (+ 10000000000000))
                      )
                    & traceShowId
                    & \((a@[ax, ay], b@[bx, by]), p@[px, py]) ->
                      let -- [ax bx] [m] = [px]
                          -- [ay by] [n] = [py]
                          -- [m] = [ by -bx ] [px]
                          -- [n] = [ -ay ax ] [py]
                          d = ax * by - bx * ay
                          (m, m') = (by * px - bx * py) `divMod` d
                          (n, n') = (ax * py - ay * px) `divMod` d
                       in if n' == 0 && m' == 0
                            then n + 3 * m
                            else 0
                            -- let k0 = minimum (zipWith div p b)
                            --  in foldr
                            --       ( (\k -> map (* k) b) >>> \q@[qx, qy] ->
                            --           let n = qx `div` bx
                            --               m = (px - qx) `div` ax
                            --            in if (zipWith mod (zipWith (-) p q) a & all (== 0))
                            --                 && n + m <= 100
                            --                 then const $ n + 3 * m
                            --                 else id
                            --       )
                            --       0
                            --       [k0, k0 - 1 .. 0]
              )
            >>> sum
            >>> print
        )
