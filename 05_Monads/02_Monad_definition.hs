
data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f logLabel a = Log [logLabel] (f a)

add1Log :: Integer -> Log Integer
add1Log = toLogger (+1) "added one"

mult2Log :: Integer -> Log Integer
mult2Log = toLogger (*2) "multiplied by 2"

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x l1 l2 = Log (logs1 ++ logs2) val2
    where
        (Log logs1 val1) = l1 x
        (Log logs2 val2) = l2 val1

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f a = return $ f a

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log logs val) f = Log (logs ++ logs') val'
    where
        (Log logs' val') = f val

instance Functor Log where
    fmap f (Log logs val) = Log logs $ f val

instance Applicative Log where
    pure = returnLog
    (<*>) :: Log (a -> b) -> Log a -> Log b
    (<*>) (Log logs f) (Log logsA valA) = Log (logs ++ logsA) $ f valA

instance Monad Log where
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList val = foldl (>>=) (return val)
