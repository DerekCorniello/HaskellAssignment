module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst needle haystack = findHelper needle haystack 0
  where
    findHelper _ [] _ = NoMatch
    findHelper needle (hay:stack) index =
      if needle hay
        then Match index
        else findHelper needle stack (index + 1)
------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome [] = True
palindrome [x] = True  -- Single character is a palindrome
palindrome candidate =
  if head candidate == last candidate
    then palindrome (init (tail candidate))
    else False