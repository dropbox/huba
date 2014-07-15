
-- Binary adventures
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word)


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

largestFibUnder n = liftA2 (,) maximum (\x -> length x - 1) $ takeWhile (<= n) fibs

data Bit = One | Zero | Bad deriving Show

fibEncodeList :: Int -> [Bit]
fibEncodeList n = let largest = largestFibUnder n
                      bits = Prelude.replicate 5 Zero in
                  bits

fibEncode :: Int -> [Bit]
fibEncode n = One : recurse relevantFibs n where
    relevantFibs = reverse $ takeWhile (<= n) fibs -- reversing list inefficient
    recurse (f:fs) 0 = [Bad]
    recurse (f:fs) n = if f <= n then Zero : recurse fs n
                       else One : recurse fs (n - f)
    recurse [] _ = [One]
    recurse _ _ = [Bad]


-- fibEncode2 :: Int -> [Bit]
fibEncode2 n = let (largest, index) = largestFibUnder n in
               U.create $ do
                 v <- M.new index
                 M.set v 0 -- Initialize to zero
                 M.write v 0 (42 :: Word)
                 return v


-- deltaEncodeList :: (Num a) => [a] -> a
deltaEncodeList l@(x:xs) = x : zipWith (-) (tail l) l

main = do
    v <- M.new 10
    M.write v 0 (3 :: Int)
    x <- M.read v 0
    print x
    -- return x

a = U.create $ do
      v <- M.new 2
      M.write v 0 (42 :: Word)
      M.write v 1 33
      return v
