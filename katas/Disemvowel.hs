module Disemvowel where

import Data.Char (toLower)

disemvowel :: String -> String
disemvowel = filter (flip notElem "aeiou" . toLower)
