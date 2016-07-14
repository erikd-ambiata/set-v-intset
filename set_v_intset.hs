
import qualified Criterion.Main as C

import qualified Data.IntSet as IntSet
import qualified Data.List as DL
import qualified Data.Set as Set

import qualified Test.QuickCheck as QC




main :: IO ()
main = do
    dense <- generateInputData 10000 1000
    sparse <- generateInputData 10000 100000
    C.defaultMain
        [ C.bench "Set dense"     $ C.whnf setInsert (dense, Set.empty, Set.insert, Set.size)
        , C.bench "IntSet dense"  $ C.whnf setInsert (dense, IntSet.empty, IntSet.insert, IntSet.size)
        , C.bench "Set sparse"    $ C.whnf setInsert (sparse, Set.empty, Set.insert, Set.size)
        , C.bench "IntSet sparse" $ C.whnf setInsert (sparse, IntSet.empty, IntSet.insert, IntSet.size)
        ]

setInsert :: ([Int], a, Int -> a -> a, a -> Int) -> Int
setInsert (testdata, set, insert, sizeFunc) =
  sizeFunc $ DL.foldr insert set testdata

generateInputData :: Int -> Int -> IO [Int]
generateInputData len range =
  QC.generate $ QC.vectorOf len $ QC.choose (0, range)
