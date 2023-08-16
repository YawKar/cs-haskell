main = do
  print anotherPi
  print (max 5 42)
  print ((max 5) 42) -- partially applied function
  {-
  A function with N variables can be expressed as a function with 1 variable,
  yielding a function with N-1 variables:
  `A[1] -> A[2] -> ... -> A[N] -> Output` ~> `A[1] -> (A[2] -> ... -> A[N] -> Output)`
  -}

anotherPi = acos (cos pi)

sumSquares x y = x ^ 2 + y ^ 2

rock'n'roll = 42

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

whatIsTheNumber x = if x < 0 then "negative" else "non-negative"

prettyWhatIsTheNumber x = "Number is " ++ whatIsTheNumber x

sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

max5 = max 5

discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount = discount 1000 5
