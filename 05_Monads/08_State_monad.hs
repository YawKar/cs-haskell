module Main (main) where
import Control.Monad.Reader (Reader, runReader, replicateM_)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.Monad.State (runState, evalState, MonadState (state, get, put), State)

main :: IO ()
main = putStrLn "State Monad"

newtype State' s a = State' { runState' :: s -> (a, s) }

-- runState' :: State' s a -> s -> (a, s)

instance Functor (State' s) where
    fmap :: (a -> b) -> State' s a -> State' s b
    fmap f (State' runSt) = State' newRunSt
        where
            newRunSt s = (f value, state)
                where
                    (value, state) = runSt s

instance Applicative (State' s) where
    pure :: a -> State' s a
    pure x = State' (x, )
    (<*>) :: State' s (a -> b) -> State' s a -> State' s b
    -- this is broken!!!
    State' runStTr <*> State' runSt = State' newRunSt
        where
            newRunSt s = (transform a, s')
                where
                    (a, s') = runSt s
                    (transform, _) = runStTr s

instance Monad (State' s) where
    (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    State' runStA >>= k = State' newRunSt
        where
            newRunSt s = runStB s'
                where
                    (a, s') = runStA s
                    State' runStB = k a

execState' :: State' s a -> s -> s
execState' m st = snd $ runState' m st

evalState' :: State' s a -> s -> a
evalState' m st = fst $ runState' m st

get' :: State' s s
get' = State' $ \s -> (s, s)

put' :: s -> State' s ()
put' s = State' $ const ((), s)

example :: State' Integer Integer
example = do
    put' 5 -- set state to 5, and the whole structure to (\_ -> ((), 5))
    n <- get' -- get state as a value, structure is (\s -> (s, s))
    put' 4 -- set state to 4, and the whole structure to (\_ -> ((), 4))
    return (n + 1) -- return value 5 + 1 ~= 6 and the state is (6, 4)

tick' :: State' Integer Integer
tick' = do
    n <- get'
    put' (n + 1)
    return n

_7 :: Integer
_7 = evalState' (tick' >> tick' >> tick') 5

_8 :: Integer
_8 = execState' (tick' >> tick' >> tick') 5

modify :: (s -> s) -> State' s ()
-- modify f = State' $ \s -> ((), f s)
modify f = do
    s <- get'
    put' $ f s

_14 :: Integer
_14 = evalState' (tick' >> tick' >> modify (*2) >> get') 5

_14' :: State' Integer Integer
_14' = do
    tick'
    tick'
    modify (*2)
    get'

readerToState :: Reader r a -> State' r a
readerToState m = State' $ \r -> (runReader m r, r)

writerToState :: Monoid w => Writer w a -> State' w a
writerToState m = State' $ \w -> (a, w <> w')
    where
        (a, w') = runWriter m

succ' :: Integer -> Integer
succ' = execState' tick'

plus' :: Integer -> Integer -> Integer
plus' n = execState' (sequence_ $ replicate (fromIntegral n) tick')

fibStep :: State' (Integer, Integer) ()
fibStep = State' $ \(x0, x1) -> ((), (x1, x0 + x1))

execStateN :: Int -> State' s a -> s -> s
execStateN n m = execState' $ sequence_ $ replicate n m

data Tree a
    = Leaf a
    | Fork (Tree a) a (Tree a)
    deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (monadicNumbering tree) 1

tick :: State Integer Integer
tick = do
    n <- get
    put (n + 1)
    return n

monadicNumbering :: Tree () -> State Integer (Tree Integer)
monadicNumbering (Leaf _) = do
    Leaf <$> tick
monadicNumbering (Fork left val right) = do
    left' <- monadicNumbering left
    n <- tick
    right' <- monadicNumbering right
    return (Fork left' n right')
