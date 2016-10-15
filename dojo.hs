x::Int
x=5

lucky::(Integral a)=> a ->String
lucky 7 = "El siete es tu numero de suerte"
lucky x = "hoy no es tu dia de suerte"

factorial::Integer->Integer
factorial 0=1
factorial x = x*factorial(x-1)

suma::Int->Int
suma 0=0
suma x = x+suma(x-1)

sumEveryTwo::[Integer]->[Integer]
sumEveryTwo[]=[]
sumEveryTwo(x:[]) = [x]
sumEveryTwo(x:y:zs) = (x+y):sumEveryTwo zs

intListLength::[Integer]->Int
intListLength []=0
intListLength (x:zs) = 1+intListLength zs

pesoTest::Int -> [Char]
pesoTest peso
	|peso<=40 = "Estas muy flaco"
	|peso<=60 = "Estas bien de peso"
	|peso<=80 = "Estas gordo"
	|otherwise = "Estas super gordo"

mayor :: Int->Int->Int
mayor a b
	|a>b = a
	|otherwise = b

-- Listas intensivas
duplicarPares xs = [x*2|x<-xs,(mod x 2)==0]

