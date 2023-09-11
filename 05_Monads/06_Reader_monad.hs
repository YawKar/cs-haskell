module Main (main) where

import Debug.Trace (trace)
import Control.Monad.Reader (Reader, MonadReader (reader), runReader)

main :: IO ()
main = putStrLn "Hello"

safeHead :: [a] -> Maybe a
safeHead = do
    b <- null
    if b
        then return Nothing
        else do
            Just <$> head

k :: [b]
k = (\x -> [x, x]) =<< []

safeHeadDesugared :: [a] -> Maybe a
safeHeadDesugared =
    null >>= (\b ->
        if b
            then return Nothing
            else Just . head
        )

_24 :: Integer
_24 = return 2 >>= (+) >>= (*) $ 4

newtype ReaderM e a = ReaderM { runReader' :: e -> a }

instance Functor (ReaderM e) where
    fmap :: (a -> b) -> ReaderM e a -> ReaderM e b
    fmap f (ReaderM getter) = ReaderM $ \e -> f $ getter e

instance Applicative (ReaderM e) where
    pure :: a -> ReaderM e a
    pure value = ReaderM $ const value
    (<*>) :: ReaderM e (a -> b) -> ReaderM e a -> ReaderM e b
    ReaderM getter1 <*> ReaderM getter2 = ReaderM newReader
        where
            newReader e = getter1 e $ getter2 e

instance Monad (ReaderM e) where
    -- m1 >> m2 = m1 >>= const m2
    (>>=) :: ReaderM e a -> (a -> ReaderM e b) -> ReaderM e b
    ReaderM m1 >>= k = ReaderM $ \e -> runReader' (k (m1 e)) e

ask :: ReaderM e e
ask = ReaderM id

test :: Int
test = length $
    runReader' (
        ask >>= (
            \x -> ReaderM $ \_ -> trace "dfd" $ length x) >>= ( -- no output
                    \_ -> ask) >>= (                            -- because of laziness
                        \x -> ReaderM $ \e -> trace (show x) x) -- [1,2]
    )
    $ [1, 2]

type User = String
type Password = String
type UsersTable = [(User, Password)]

pwds :: UsersTable
pwds = [("Bill", "123"),
        ("Ann", "qwerty"),
        ("John", "2sRq8P")]

firstUser :: ReaderM UsersTable User
firstUser = do
    table <- ask
    return $ fst . head $ table

asks :: (e -> a) -> ReaderM e a
asks = ReaderM

firstUserPwd :: ReaderM UsersTable Password
firstUserPwd = asks (snd . head)
    -- do
    --     pwd <- asks (snd . head)
    --     return pwd

usersCount :: ReaderM UsersTable Int
usersCount = asks length

-- Local environment change
local' :: (e -> e) -> ReaderM e a -> ReaderM e a
local' f (ReaderM prev) = ReaderM $ \e -> prev (f e)

local'' :: (e -> e') -> ReaderM e' a -> ReaderM e a
local'' f (ReaderM prev) = ReaderM $ \e -> prev (f e)

localTest :: ReaderM UsersTable (Int, Int, Int)
localTest = do
    count1 <- usersCount
    count2 <- local' (("Mike", "1"):) usersCount
    count3 <- usersCount
    return (count1, count2, count3) -- [k, k+1, k]

reader' :: (e -> a) -> ReaderM e a
reader' = ReaderM -- this is how it's implemented in standard library, you can't access Reader constructor directly

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = reader helper
    where
        helper :: UsersTable -> [User]
        helper table = case table of
            [] -> []
            (user'@(user, password) : xs)
                | password == "123456" -> user : helper xs
                | otherwise -> helper xs
