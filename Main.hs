{-# LANGUAGE BangPatterns #-}
module Main where
{-
Just having some fun seeing what Dave's "most powerful" words are on twitter

All code is stream of concious so do not expect optimal, tidy or commented
functions etc.
-}

-- Imports ----------------------------------------------------------------------
import           Control.Monad
import           Data.Char
import           Data.Hashable
import           Data.HashMap.Strict as HM
import           Data.List           as L
import           System.Directory
import           Text.Printf


-- Functions --------------------------------------------------------------------

-- > readTwitter
readTwitter :: Int -> IO [String]
readTwitter num = do
  let filename = printf "./data/tweet%04d.txt" num :: String
  !filecontents <- readFile filename
  return (lines filecontents)


-- > get a subsequence of words
subseq :: Int -> Int -> [a] -> [a]
subseq start len = take len . drop start


-- > get the n-grams from a list of text
ngrams :: [String] -> Int -> [String]
ngrams listwords n = L.map (\index -> unwords (subseq index n listwords))
                           [0..(length listwords - n)]


wordify :: String -> [String]
wordify l = words $ L.filter (\c -> isAsciiLower c ||
                                    c == ' ' ||
                                    c == '@')
                             (L.map toLower l)


-- > turn a document into a map
docToMap :: Int -> Int -> IO (HashMap String Double)
docToMap doc n = do
  files <- liftM (L.filter (isPrefixOf "tweet")) (getDirectoryContents "./data")
  let lenFiles = genericLength files
  document <- L.map readTwitter [0.. (lenFiles - 1 )] !! doc
  let content = unwords document
  let myWords = wordify content
  let myng = ngrams myWords n
  return $ L.foldl (\mymap ngram -> insertWith (+) ngram 1.0 mymap) empty myng


-- > sum of squares
sumSquares :: (Num a, Eq k, Hashable k)
           => HashMap k a
           -> HashMap k a
           -> a
sumSquares a b = sum (elems (intersectionWith (*) a b))


-- > cosine similarity
cossim :: HashMap String Double -> HashMap String Double -> Double
cossim a b = sab / (sqrt saa * sqrt sbb)
  where
    sab = sumSquares a b
    saa = sumSquares a a
    sbb = sumSquares b b


-- > term frequency
tf :: (Eq k, Fractional a, Hashable k, Ord a) => k -> HashMap k a -> a
tf term freq = 0.5 + 0.5 * lookupDefault 0 term freq / maximum (elems freq)


-- > inverse document frequency
idf :: (Floating a1, Eq k, Hashable k) => k -> [HashMap k a] -> a1
idf term corpuses = log (genericLength corpuses /
                       (1 + genericLength (L.filter (member term) corpuses))
                  )


-- > tf-idf score
tfidf :: (Eq k, Hashable k, Ord a1, Floating a1)
      => k
      -> HashMap k a1
      -> [HashMap k a]
      -> a1
tfidf term freq corpuses = tf term freq * idf term corpuses


-- Main -------------------------------------------------------------------------
main :: IO ()
main = do
  files <- liftM (L.filter (isPrefixOf "tweet")) (getDirectoryContents "./data")
  let lenFiles = genericLength files


  corpusMaps <- traverse (`docToMap` 1) [0..(lenFiles-1)]

  putStr "IDF score of 'campus' across the entire set: "
  print $ idf "campus" corpusMaps

  -- filler
  putStrLn " "


  latest <- docToMap (lenFiles-1) 1

  putStrLn "Your latest tweet is:"
  lasttweet <- readTwitter (lenFiles-1)
  print $ unwords lasttweet

  putStrLn " "

  let pairs = L.map (\term -> (term, tfidf term latest corpusMaps))
                    (keys latest)
  let scores = sort [ y | (_,y) <- pairs]
  let biggest = drop (genericLength pairs-2) scores

  let highscores = [ (x, y) | (x,y) <- pairs,
                              y `elem` biggest]

  putStrLn "The highest (tf-idf) word scores in your latest tweet are: "
  print highscores

