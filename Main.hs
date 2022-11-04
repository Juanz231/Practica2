import Data.List

solveRPN :: String -> Float 
solveRPN = head.foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:xs) "raiz2" = sqrt(x):xs
            foldingFunction (x:xs) "negate" =  -x:xs
            foldingFunction (x:xs) "condnumb" = condnumb x:xs
            foldingFunction(x:y:ys) "sum" = sum(x:y:ys):ys
            foldingFunction(x:y:ys) "product" = product(x:y:ys):ys
            foldingFunction xs "promedio" = (sum xs /fromIntegral(length xs)):xs 
            foldingFunction xs numberString = read numberString:xs  
condnumb:: Float -> Float
condnumb num
   | num == 3 = 100
   | num == 5 = 25
   | otherwise = 0
main = do
  print "RPN : 10 67 negate"
  print (solveRPN "10 67 negate")
  print "/////"
  print "RPN : 10 67 negate +"
  print (solveRPN "10 67 negate +")
  print "/////"
  print "RPN : 10 67 negate *"
  print (solveRPN "10 67 negate *")
  print "/////"
  print "RPN : 10 16 raiz2"
  print (solveRPN "10 16 raiz2")
  print "/////"
  print "RPN : 10 16 raiz2 +"
  print (solveRPN "10 16 raiz2 +")
  print "/////"
  print "RPN : 10 5 condnumb" 
  print (solveRPN "10 5 condnumb")
  print "/////"
  print "RPN : 10 5 condnumb -"
  print (solveRPN "10 5 condnumb -")
  print "/////"
  print "RPN : 10 3 condnumb +"
  print (solveRPN "10 3 condnumb +")
  print "/////"
  print "RPN : 10 2 condnumb +"
  print (solveRPN "10 2 condnumb +")
  print "/////"
  print "RPN : 10 67 15 sum"
  print (solveRPN "10 67 15 sum")
  print "/////"
  print "RPN : 10 16 10 product"
  print (solveRPN "10 16 10 product")
  print "/////"
  print "RPN : 120 20 40 120 promedio"
  print (solveRPN "120 20 40 120 promedio")

