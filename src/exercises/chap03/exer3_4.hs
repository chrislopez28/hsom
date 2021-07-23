simple :: Integer -> Integer -> Integer -> Integer
simple x y z = x * (y + z)

applyEach :: [b -> b] -> b -> [b]
applyEach [] y = []
applyEach (x:xs) y = [x y] ++ applyEach xs y

main :: IO()
main = print (applyEach [simple 2 2, (+3)] 5)