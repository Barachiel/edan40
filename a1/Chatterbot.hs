module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------



stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return (rulesApply (pairExtract brain r))

pairExtract:: BotBrain -> Float-> [PhrasePair]
pairExtract [] _ = []
pairExtract (brainBit:brainBits) r = ((fst brainBit), (pick r (snd brainBit))):pairExtract brainBits r


rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply l c
  | transformationsApply "*" reflect l c == Nothing = []
  | otherwise = try (transformationsApply "*" reflect l) c

reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs)
  | lookup x reflections /= Nothing = (fromJust( (lookup x) reflections)) : reflect xs
  | otherwise = x:reflect xs


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile p = (map . map2) (lowerWords, (map lowerWords)) p
                where lowerWords = words . map toLower


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply pp p
  | transformationsApply "*" id pp p /= Nothing = fix (try (transformationsApply "*" id pp)) p
  | otherwise = p


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a [] b = []
substitute a (x:xs) b
  | a == x = b ++ substitute a xs b
  | otherwise = x : substitute a xs b



-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match a (x:xs) (y:ys)
  | x == a = orElse (singleWildcardMatch (x:xs) (y:ys)) (longerWildcardMatch (x:xs) (y:ys))
  | x == y = match a xs ys
  | otherwise = Nothing


-- Helper function to match --
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:wc_rest) (s:s_rest)
  | match wc wc_rest s_rest /= Nothing = Just [s]
  | otherwise = Nothing

longerWildcardMatch (wc:ps) (x:xs)
  | match wc (wc:ps) xs /= Nothing = mmap ([x] ++) (match wc (wc:ps) xs)
  | otherwise = Nothing


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply a f x (y,z) = mmap (substitute a z)  (mmap f (match a y x))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply a b (z:zs) x
  | transformationApply a b x z == Nothing = (transformationsApply a b zs x)
  | otherwise = (transformationApply a b x z)
