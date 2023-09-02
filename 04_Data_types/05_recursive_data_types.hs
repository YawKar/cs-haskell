data List a = Nil | Cons a (List a)
    deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons head rest) = head : fromList rest

toList :: [a] -> List a
toList = foldr Cons Nil

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc pred) = 1 + fromNat pred

add :: Nat -> Nat -> Nat
add Zero y = y
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Suc Zero) y = y
mul x (Suc Zero) = x
mul (Suc x) y = mul' y x (add Zero y)
    where
        -- mul' addition repeats acc
        mul' _ Zero acc = acc
        mul' Zero _ acc = acc
        mul' addition (Suc repeats) acc = mul' addition repeats (add acc addition)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac x@(Suc x') = mul x (fac x')

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left right) = max (height left) (height right) + 1

size :: Tree a -> Int
size (Leaf _) = 1
size (Node left right) = size left + size right + 1

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf value) = (1, value)
    go (Node left right) = (lc + rc, ls + rs)
        where
            (lc, ls) = go left
            (rc, rs) = go right

-- Let's try to code domain for arithmetic expressions

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expr1 :: Expr
expr1 = Val 2 :+: Val 3 :*: Val 4
expr2 :: Expr
expr2 = (Val 2 :+: Val 3) :*: Val 4

eval :: Expr -> Int
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand e@(e1 :*: e2) =
    case (e1 == e1', e2 == e2') of
        (True, True) -> e
        _ -> expand (e1' :*: e2')
    where
        e1' = expand e1
        e2' = expand e2
expand e = e

expand' :: Expr -> Expr
expand' = foldr1 (:+:) . expandList
    where
        expandList :: Expr -> [Expr]
        expandList v@(Val _) = [v]
        expandList (e1 :+: e2) = expandList e1 ++ expandList e2
        expandList (e1 :*: e2) = [e1' :*: e2' | e1' <- expandList e1, e2' <- expandList e2]
