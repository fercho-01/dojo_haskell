--Punto 1

--Reversar un numero 1234 -> [4 3 2 1]
toDigitsRev :: Int -> [Int]
toDigitsRev x
	|x<=0 = []
	|otherwise = [mod x 10]++toDigitsRev (div x 10)

toDigits :: Int -> [Int]
toDigits x = reverse (toDigitsRev x)

intListLength::[Int]->Int
intListLength []=0
intListLength (x:zs) = 1+intListLength zs

doubleEveryOther :: [Int]->[Int]
doubleEveryOther [] = []
doubleEveryOther n
    |(intListLength n `mod` 2) == 0 = pair n
    |otherwise                      = nonpair n

pair :: [Int]->[Int]
pair []         = []
pair (x:xs:r)   = x * 2 : xs : doubleEveryOther r

nonpair :: [Int]->[Int]
nonpair (x:[])      = x:[]
nonpair (x:xs:r)    = x: xs * 2 : doubleEveryOther r

sumDigits::[Int]->Int
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigitsRev x) + sumDigits xs

validate :: Int -> Bool
validate n 
	|(sumDigits(doubleEveryOther(toDigits n))) `mod` 10 == 0    = True
    |otherwise                                                  = False  