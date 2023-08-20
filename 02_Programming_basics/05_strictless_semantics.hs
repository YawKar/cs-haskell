
{-
seq :: a -> b -> b
seq _|_ b = _|_ -- otherwise it will return a divergent computation result (actually bottom type, e.g. undefined/error)
seq a b = b -- forces `a` to be evaluated to WHNF and then returns `b` if `a` is a non-divergent computation

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
-}

main :: IO ()
main = do
  print seqForcesOnlyConstruction -- 4
  print constThatDoesn'tFail -- 42
  print constThatDoesFail -- Boom
  print seqForcesToBoom -- Boom

seqForcesToBoom :: Integer
seqForcesToBoom = seq undefined 4

seqForcesOnlyConstruction :: Integer
seqForcesOnlyConstruction = seq (undefined, undefined) 4

calcArgBeforeApplication :: (a -> b) -> a -> b
calcArgBeforeApplication f x = f $! x

constThatDoesn'tFail :: Integer
constThatDoesn'tFail = const 42 $ undefined

constThatDoesFail :: Integer
constThatDoesFail = const 42 $! undefined
