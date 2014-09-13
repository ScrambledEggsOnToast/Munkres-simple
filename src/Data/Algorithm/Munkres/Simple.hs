-- | A simple and typesafe layer over "Data.Algorithm.Munkres".

module Data.Algorithm.Munkres.Simple (
    -- * Problems
    Problem, 
    problem, 
    -- * Solutions
    Solution, solve, associatedA, associatedB, associationList)
    where

import qualified Data.Set as S (Set, size, toAscList)
import qualified Data.Bimap as B (Bimap, lookup, lookupR, toList, fromList)
import qualified Data.Array.IArray as IA (array)

import Data.Algorithm.Munkres (hungarianMethodDouble)

-- | An association problem, consisting of two sets of items, and a weight function between them. Construct with 'problem'.

data Problem a b = Problem {
    setA :: S.Set a
  , setB :: S.Set b
  , weightFunction :: a -> b -> Double
}

-- | Constructs an association problem, checking whether the sets of objects are the same size first.

problem :: (Ord a, Ord b) => S.Set a -> S.Set b 
                          -> (a -> b -> Double) 
                          -> Problem a b
problem as bs df = 
    if (S.size as) == (S.size bs) 
        then Problem as bs df 
        else error "Sets are of different size"

-- | The solution of an association problem.

data Solution a b = Solution {
    solutionBimap :: B.Bimap a b
  , solutionCost :: Double
}

-- | Solve an association problem.

solve :: (Ord a, Ord b) => Problem a b -> Solution a b
solve p = Solution solution cost
  where
    solution = B.fromList $ 
        map (\(m,n) -> (snd $ as !! (m-1), snd $ bs !! (n-1))) intSolution

    (intSolution, cost) = hungarianMethodDouble 
        $ IA.array ((1,1), (numA, numB)) assocs

    numA = S.size . setA $ p
    numB = S.size . setB $ p
    as = zip [1..] (S.toAscList . setA $ p)
    bs = zip [1..] (S.toAscList . setB $ p)
    
    assocs = [((m,n), weightFunction p a b) | (m,a) <- as, (n, b) <- bs]

-- | In a solution of type @Solution a b@, finds the @a@ that a given @b@ is paired with. Returns @Nothing@ if the given @b@ was not a part of the initial problem.

associatedA :: (Ord a, Ord b) => Solution a b -> b -> Maybe a
associatedA s b = B.lookupR b . solutionBimap $ s

-- | In a solution of type @Solution a b@, finds the @b@ that a given @a@ is paired with. Returns @Nothing@ if the given @a@ was not a part of the initial problem.

associatedB :: (Ord a, Ord b) => Solution a b -> a -> Maybe b
associatedB s a = B.lookup a . solutionBimap $ s

-- | A list of the pairs of items in a solution.

associationList :: Solution a b -> [(a,b)]
associationList = B.toList . solutionBimap
