{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Data.Graph.AStar
import qualified Data.HashSet as HS

test_oneNode :: IO ()
test_oneNode =
    let solution = aStar (\() -> HS.singleton ())
                         (\() () -> 0)
                         (\() -> 0)
                         (\() -> True)
                         ()
     in assertEqual (Just []) solution

test_circle :: IO ()
test_circle =
    let solution = aStar (\n -> HS.singleton (if n < 10 then n+1 else 0))
                         (\a b -> abs (a - b))
                         (\n -> 10 - n)
                         (== 10)
                         (0::Int)
     in assertEqual (Just [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) solution

test_fullyMesh :: IO ()
test_fullyMesh =
    let solution = aStar (\n -> HS.fromList [0..10])
                         (\a b -> abs (a - b))
                         (\n -> 10 - n)
                         (== 10)
                         (0::Int)
     in assertEqual (Just [10]) solution

test_circleWithUselessHeuristic :: IO ()
test_circleWithUselessHeuristic =
    let solution = aStar (\n -> HS.singleton (if n < 10 then n+1 else 0))
                         (\a b -> abs (a - b))
                         (const 0)
                         (== 10)
                         (0::Int)
     in assertEqual (Just [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) solution

main = htfMain htf_thisModulesTests
