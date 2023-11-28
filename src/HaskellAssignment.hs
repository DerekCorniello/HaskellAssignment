module HaskellAssignment where

data Found = Match Int | NoMatch deriving Eq

-- make a type class for Show for the ADT Found to use in findFirst
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

------------------------------------------------
-- findFirst: finds the first index in a list 
--            that meets the function condition
--
--    inputs: a function with an input type a &
--            return type of Bool, and a list of
--            type a. Type a must be of type 
--            class Eq.
--
--   outputs: a Found datatype
------------------------------------------------

findFirst :: Eq a => (a -> Bool) -> [a] -> Found
-- make a helper function to keep track of the index, def value zero
findFirst needle haystack = findFirstHelper needle haystack 0 where

    -- list is empty? return no match
    findFirstHelper _ [] _ = NoMatch
    -- if not, 
    findFirstHelper needle (hay:stack) index =
      -- if the first element in the list fulfills the condition, return a match with the index
      if needle hay
        then Match index
        -- if not, make recursive call and increase the index.
        else findFirstHelper needle stack (index + 1)

------------------------------------------------
-- palindrome: determines if a string is a 
--                             palindrome.
-- 
--     inputs: A list of characters (or string)
-- 
--    outputs: A bool value
------------------------------------------------

palindrome :: [Char] -> Bool

-- per docs, empty string is a palindrome
palindrome [] = True 

-- single character is a palindrome
palindrome [x] = True  

palindrome candidate =
  -- if the first and last char in a candidate match
  if head candidate == last candidate

    -- make recursive call to ensure the rest of the string is
    -- init returns all chars but the last 
    -- tail returns all chars but the first

    then palindrome (init (tail candidate))

    -- otherwise, return False
    else False