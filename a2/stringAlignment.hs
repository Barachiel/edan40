import Data.List

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String, String)

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (x:xs) [] = scoreCheck x '-' + similarityScore xs []
similarityScore [] (y:ys) = scoreCheck '-' y + similarityScore [] ys
similarityScore (x:xs) (y:ys) = maximum [scoreCheck x y + similarityScore xs ys,
                                    scoreCheck x '-' + similarityScore xs (y:ys),
                                    scoreCheck '-' y + similarityScore (x:xs) ys]

scoreCheck :: Char -> Char -> Int
scoreCheck '-' c2 = scoreSpace
scoreCheck c1 '-' = scoreSpace
scoreCheck c1 c2
  | c1 == c2 = scoreMatch
  | c1 /= c2 = scoreMismatch

score :: String -> String -> Int
score [] [] = 0
score (x:xs) (y:ys) = scoreCheck x y + score xs ys

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f [] = []
maximaBy f [x] = [x]
maximaBy f (x:xs)
  | (f x) > maximum (map f xs) = x:[]
  | (f x) == maximum (map f xs) = x : maximaBy f xs
  | (f x) < maximum (map f xs) = maximaBy f xs

-- maximaBy :: Ord b => (a -> b) -> [a] -> [a]
-- maximaBy f xs = extractMax (map f xs) xs
--
-- extractMax :: Ord b => [b] -> [a] -> [a]
-- extractMax mxs xs = genMax (elemIndices (maximum mxs) mxs) xs
--
-- genMax :: [Int] -> [a] -> [a]
-- genMax [] xs = []
-- genMax (i:indexes) xs = (xs !! i) : genMax indexes

attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs,ys) <- aList]

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) list
                              where list = concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y:ys)), attachHeads '-' y (optAlignments (x:xs) ys)]

optAlignmentsOpt :: String -> String -> [AlignmentType]
optAlignmentsOpt xs ys = optLen (length xs) (length ys)
  where
    optLen i j = optTable!!i!!j
    optTable = [ [optEntry i j | j<-[0..]] | i<-[0..] ]

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry _ 0 = 0
    optEntry 0 _ = 0
    optEntry i j
      | x == y = 1 + optLen (i-1) (j-1)
      | otherwise = max (optLen i (j-1))
                        (optLen (i-1) j)
      where
        x = xs!!(i-1)
        y = ys!!(j-1)

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  let s = optAlignments string1 string2
  putStrLn ("There are " ++ show (length s) ++ " optimal alignments:")
  mapM_ (putStrLn.showTuple) s
    where
      showTuple (s1, s2) = "\n" ++ insertSpace s1 ++ "\n" ++ insertSpace s2

insertSpace :: String -> String
insertSpace [] = []
insertSpace (x:xs) = x:' ':(insertSpace xs)
