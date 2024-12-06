import Control.Arrow ((>>>))
import Control.Monad.ST
import Data.Array.ST
import Data.Bifunctor
import Data.Bits
import Data.Function
import Data.Functor
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import Data.List
import Data.Maybe
import Data.Traversable

between a b x = a <= x && x <= b

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p act s =
  if p s
    then pure s
    else act s >>= untilM p act

main =
  getContents
    >>= ( lines
            >>> (,) <*> fromJust . findIndex ('^' `elem`)
            >>> (,) <*> fromJust . elemIndex '^' . uncurry (!!)
            >>> \((field, y), x) ->
              let n = length field
                  key x y = x .|. y `shiftL` 10
                  dkey dx dy = dx + 1 .|. (dy + 1) `shiftL` 2
                  ins x y dx dy =
                    IMap.insertWith ISet.union (key x y) $
                      ISet.singleton $
                        dkey dx dy
               in print $ runST $ do
                    arr <- newListArray (0, n * n - 1) $ concat field :: ST s (STUArray s Int Char)
                    fmap sum $ for [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]] $ \(i, j) -> do
                      el <- readArray arr (j * n + i)
                      if el /= '.'
                        then return 0
                        else do
                          writeArray arr (j * n + i) '#'
                          loop <-
                            untilM
                              ( \(x, y, dx, dy, _, loop) ->
                                  not
                                    ( between 0 (n - 1) (x + dx)
                                        && between 0 (n - 1) (y + dy)
                                    )
                                    || loop
                              )
                              ( \(x, y, dx, dy, s, _) ->
                                  let nx = x + dx
                                      ny = y + dy
                                      ns = ins x y dx dy s
                                      loop =
                                        s
                                          & IMap.lookup (key x y)
                                          & maybe False (ISet.member $ dkey dx dy)
                                   in readArray arr (ny * n + nx) >>= \el ->
                                        if el == '#'
                                          then return (x, y, -dy, dx, ns, loop)
                                          else return (nx, ny, dx, dy, ns, loop)
                              )
                              (x, y, 0, -1, IMap.empty, False)
                              <&> \(_, _, _, _, _, loop) -> fromEnum loop
                          writeArray arr (j * n + i) '.'
                          return loop
        )
