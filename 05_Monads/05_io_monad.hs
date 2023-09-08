module Main where
import System.Directory (getDirectoryContents, removeFile)

main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Nice to meet you, " ++ name ++ "!"

mainDesugared :: IO ()
mainDesugared =
    putStrLn "What is your name?" >>
    getLine >>= (\name ->
    putStrLn $ "Nice to meet you, " ++ name ++ "!")

main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if null name
        then main'
        else putStrLn $ "Hi, " ++ name ++ "!"

type RealWorld' = Integer

newtype IO' a = IO' (RealWorld' -> (RealWorld', a))

instance Functor IO' where
    fmap f (IO' rl) = IO' newIO
        where
            newIO w = (rl', f val)
                where
                    (rl', val) = rl w

instance Applicative IO' where
    pure :: a -> IO' a
    pure x = IO' (, x)
    (<*>) :: IO' (a -> b) -> IO' a -> IO' b
    IO' transform <*> IO' io = IO' newIO
        where
            newIO w = (rl', (snd . transform) w val)
                where
                    (rl', val) = io w

instance Monad IO' where
    (>>=) :: IO' a -> (a -> IO' b) -> IO' b
    IO' io >>= k = IO' $ \w -> case io w of (w', a) -> undefined

lolWtfIsGoingOn :: IO' Integer
lolWtfIsGoingOn = do
    return 123 >>= \x ->
        return $ x + 1

getLine' :: IO String
getLine' = do
    cur <- getChar
    if cur == '\n'
        then return []
        else do
            rest <- getLine'
            return $ cur : rest

putStr' :: String -> IO ()
-- putStr' = foldr ((>>) . putChar) (return ())
putStr' [] = return ()
putStr' (c : cs) = putChar c >> putStr' cs

sequenceWithJusts :: Maybe ()
sequenceWithJusts = sequence_ [Just 1, Just 2] -- Just ()
sequenceWithJusts' :: Maybe ()
sequenceWithJusts' = sequence_ [Just 1, Nothing] -- Nothing

sequenceWithLists :: [()]
sequenceWithLists = sequence_ [[1, 2], [3, 4, 5]] -- [(), (), (), (), (), ()] -- bc 2 x 3 = 6

sequenceWithIO :: IO ()
sequenceWithIO = sequence_ [putStrLn "Hello", putStrLn "World"] -- IO ()

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar
-- putStr'' = mapM_ putChar

eightCalculations :: [()]
eightCalculations = mapM_ (\x -> [x,x]) "ABC" -- [(), (), (), (), (), (), (), ()] -- 8 bc ["AA", "BB", "CC"] -> 2 x 2 x 2 = 8

sequentiallyGetLines :: IO [String]
sequentiallyGetLines = sequence [
    putStrLn "Please enter your name:" >> return "out1",
    getLine,
    putStrLn "Please enter your age:" >> return "out2",
    getLine,
    putStrLn "Please enter your surname:" >> return "out3",
    getLine
    ]

resultUnitList :: IO [()]
resultUnitList = mapM putChar "Abc" -- [(), (), ()]


-- task
mainTask :: IO ()
mainTask = do
    putStr "Substring: "
    substr <- getLine
    if null substr
        then putStrLn "Canceled"
        else do
            contents <- getDirectoryContents "."
            let filteredContents = filter (`hasSubstr` substr) contents
            mapM_ deleteFileToy filteredContents
            where
                _ `hasSubstr` [] = True
                [] `hasSubstr` _ = False
                g'@(g:gx) `hasSubstr` s
                    | g' `prefixHas` s = True
                    | gx `hasSubstr` s = True
                    | otherwise = False

                _ `prefixHas` [] = True
                [] `prefixHas` _ = False
                p'@(p:px) `prefixHas` s'@(s:sx)
                    | p == s = px `prefixHas` sx
                    | otherwise = False

                deleteFileToy :: FilePath -> IO ()
                deleteFileToy = putStrLn . (++) "Removing file: "

                deleteFileDanger :: FilePath -> IO ()
                deleteFileDanger filePath = do
                    putStrLn $ "Removing file: " ++ filePath
                    removeFile filePath
