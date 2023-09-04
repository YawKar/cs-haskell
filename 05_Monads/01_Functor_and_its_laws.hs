class Functor' f where
    fmap' :: (a -> b) -> f a -> f b -- (a -> b) 'lifts up to' (f a -> f b)

instance Functor' [] where
    fmap' = map

instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just val) = Just (f val)

nothing1 :: Maybe Integer
nothing1 = fmap' (*2) Nothing
_42 :: Maybe Integer
_42 = fmap' (*2) $ Just 21

data Point3D a = Point3D a a a deriving Show

instance Functor' Point3D where
    fmap' :: (a -> b) -> Point3D a -> Point3D b
    fmap' f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a
    = Point (Point3D a)
    | LineSegment (Point3D a) (Point3D a)
    deriving Show

instance Functor' GeomPrimitive where
    fmap' f (Point p) = Point (fmap' f p)
    fmap' f (LineSegment p1 p2) = LineSegment (fmap' f p1) (fmap' f p2)

data Tree a
    = Leaf a
    | Branch (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left val right) = Branch (fmap f left) (f val) (fmap f right)

testTree :: Tree Integer
testTree = Branch (Leaf 2) 3 (Leaf 4)
squaredTree :: Tree Integer
squaredTree = fmap (^2) testTree -- Branch (Leaf 4) 9 (Leaf 16)
squaredTreeUsingLtDollarGt :: Tree Integer
squaredTreeUsingLtDollarGt = (^2) <$> testTree
updateWithConstant = 42 <$ testTree -- Branch (Leaf 42) 42 (Leaf 42)

data Tree' a = Leaf' (Maybe a) | Branch' (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree' where
    fmap g (Leaf' Nothing) = Leaf' Nothing
    fmap g (Leaf' (Just val)) = Leaf' $ Just $ g val
    fmap g (Branch' left val right) = Branch' (fmap g left) val' (fmap g right)
        where
            val' = case val of
                Nothing -> Nothing
                Just x -> Just $ g x

instance Functor' ((,) s) where -- Functor expects * -> * kind for type parameter, in this case it's: (s,) :: * -> *
    fmap' :: (a -> b) -> (s, a) -> (s, b) -- change only the second element
    fmap' g (x, y) = (x, g y)

instance Functor' (Either err) where
    fmap' :: (a -> b) -> Either err a -> Either err b
    fmap' _ (Left err) = Left err
    fmap' g (Right value) = Right $ g value

instance Functor' ((->) e) where
    fmap' :: (a -> b) -> (e -> a) -> (e -> b)
    -- fmap' f g = (\x -> f $ g x)
    -- fmap' f g x = f $ g x -- function composition
    fmap' = (.)

lengthOfTail :: [a] -> Int
lengthOfTail = fmap' length tail -- the same as: length . tail

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap :: (a -> b) -> Entry k1 k2 a -> Entry k1 k2 b
    fmap g (Entry (k1, k2) val) = Entry (k1, k2) (g val)

instance Functor (Map k1 k2) where
    fmap :: (a -> b) -> Map k1 k2 a -> Map k1 k2 b
    fmap g (Map entries) = Map $ map (fmap g) entries

{-
Functor laws:
(1)     fmap id === id
(2)     fmap (f . g) === fmap f . fmap g -- left part's more effective
-}
