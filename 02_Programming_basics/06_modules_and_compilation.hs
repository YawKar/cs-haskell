module Main where

import Data.Char as DChar (); -- import nothing under the name 'DChar'
import Data.List (union, sort); -- import only `sort` and `union`
import Data.Set;
-- `qualified` means that every name from this module should be prefixed with `Data.Array`
import qualified Data.Array as DArray hiding (elems); -- import everything except `elems`
import Prelude hiding (not) -- hiding default implementation of `not` function from Prelude

main :: IO ()
main = putStrLn "Main module"

not :: Bool -> Bool
not bool = if bool then False else True

nowCanUseMyOwnNot :: Bool
nowCanUseMyOwnNot = not True

-- ambiguousUnion = union  -- cannot use it like that because `union` is ambiguous
dataListUnion :: String -> String -> String
dataListUnion = Data.List.union
dataSetUnion :: Set Int -> Set Int -> Set Int
dataSetUnion = Data.Set.union

arrayMustBeQualified :: (Int, Int) -> [(Int, String)] -> DArray.Array Int String
arrayMustBeQualified = DArray.array

{-
- Lexical analysis :: Text -> Tokens
- Syntactic analysis :: Tokens -> Syntax tree -- All tokens are organized into a tree
- Name resolution :: Syntax tree -> Well-Scoped Syntax Tree -- All names become qualified
- Type checking :: Well-Scoped Syntax Tree -> Well-Typed Syntax Tree -- Check and infer the types
- Desugaring :: Well-Typed Syntax Tree -> Core Language Text -- translation
- STG (Spineless Tagless G-machine (Reduction machine)) :: Core Language Text -> Reductioned Graph (?)
- Cmm :: Reductioned Graph (?) -> C-- Language Text
-}
