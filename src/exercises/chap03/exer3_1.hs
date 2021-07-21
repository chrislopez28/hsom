import Euterpea

f1 :: Int -> [Pitch] -> [Pitch]
f1 i p = let f p1 = trans i p1
         in map f p

f2 :: [Dur] -> [Music a]

f3 :: [Music Pitch] -> [Music Pitch]

g1 :: [Pitch] -> [Music Pitch]
g1 p = let g p1 = note en p1
       in map g p 

main :: IO ()
main = play $ line (g1 (f1 2 [(A, 4), (B, 5), (C, 4), (D, 4), (E, 4), (F, 4), (C, 5)]))