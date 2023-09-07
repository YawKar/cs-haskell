
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity value) = Identity $ f value

instance Applicative Identity where
    pure = Identity
    (Identity func) <*> (Identity a) = Identity $ func a

instance Monad Identity where
    Identity val >>= f = f val

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

data SomeType a = SomeType a

instance Applicative SomeType where
    pure = SomeType
    SomeType transform <*> SomeType v = SomeType (transform v)

instance Monad SomeType where
    (>>=) :: SomeType a -> (a -> SomeType b) -> SomeType b
    SomeType v1 >>= f = f v1

instance Functor SomeType where
    fmap f x = x >>= \x' -> return $ f x'

{-
Monad laws:
1. return a >>= k === k a
2. m >>= return === m
3. (m >>= k) >>= k' === m >>= k >>= k'
-}

-- These all are equvivalent
iteration1 = runIdentity $ wrap'n'succ 3 >>= wrap'n'succ >>= wrap'n'succ
iteration2 = runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= wrap'n'succ)
iteration3 = runIdentity $ wrap'n'succ 3 >>= (\x -> wrap'n'succ x >>= (\y -> wrap'n'succ y))

goWrap0 =
    wrap'n'succ 3 >>=
    wrap'n'succ >>=
    wrap'n'succ

goWrap1 =
    wrap'n'succ 3 >>= (\x ->
    wrap'n'succ x >>= (\y ->
    wrap'n'succ y >>= (\z ->
    return (x, y, z))))

goWrap2 =
    wrap'n'succ 3 >>= \x ->
    wrap'n'succ x >>= \y ->
    wrap'n'succ y >>= \z ->
    return (x, y, z)

goWrap3 =
    wrap'n'succ 3 >>= \x ->
    wrap'n'succ x >>= \y ->
    wrap'n'succ y >> -- doesn't advance monadic value (:: m a -> m b -> m b) instead of Kleisli arrow (:: m a -> (a -> m b) -> m b)
    return (x + y)

goWrap4 =
    let z = fromIntegral $ length [1, 2, 3] in
    wrap'n'succ 3 >>= \x ->
    wrap'n'succ x >>= \y ->
    wrap'n'succ y >>
    return (x + y + z)

{-
do notation (syntax sugar that desugars to goWraps)
1. do {e1 ; e2} === e1 >> e2
2. do {p <- e1; e2} === e1 >>= \p -> e2 -- `p` in lambda can be a pattern like `Just x`
-- if pattern match fails then monadic calculation will end according to MonadFail instance's fail implementation
3. do {let v = e1; e2} === let v = e1 in do e2 -- non-monadic calculation used inside of monadic calculation
-}

goWrap0' = do
    x <- wrap'n'succ 3
    x <- wrap'n'succ x
    wrap'n'succ x

goWrap1' = do
    x <- wrap'n'succ 3
    y <- wrap'n'succ x
    z <- wrap'n'succ y
    return (x, y, z)

goWrap2' = goWrap1'

goWrap3' = do
    x <- wrap'n'succ 3
    y <- wrap'n'succ x
    wrap'n'succ y
    return (x + y)

-- every line has type of monad in which calculations are being performed
goWrap4' = do
    let z = fromIntegral $ length [1, 2, 3] -- non-monadic calculation assignment
    x <- wrap'n'succ 3 -- monadic calculation assignment
    y <- wrap'n'succ x
    wrap'n'succ y
    return (x + y + z)
