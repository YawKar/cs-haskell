
usingFunctionAsInfixOperator = 5 `max` 9
usingOperatorAsPrefixFunction = (+) 4 8

infixl 6 *+*
-- either way works
a *+* b = a ^ 2 + b ^ 2
(*+*) a b = a ^ 2 + b ^ 2

infix 6 |-|
-- either way works
a |-| b = abs (a - b)
(|-|) a b = abs (a - b)

divide2 = (2 /)
divideBy2 = (/ 2)

infixr 0 $$$
($$$) f = f
k = sin $$$ cos $$$ pi / 2 -- instead of `sin (cos (pi / 2))`
normalFromBase = sin $ cos $ pi / 2

ex = logBase 4 $ min 20 $ 9 + 7
