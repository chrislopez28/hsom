import Euterpea

f1 :: Int -> [Pitch] -> [Pitch]
f1 i p = let f p1 = trans i p1
         in map f p

f2 :: [Dur] -> [Music a]
f2 d = map rest d

f3 :: [Music Pitch] -> [Music Pitch]
f3 mps = let f (Prim (Note d p)) = note (d/2) p :+: rest (d/2)
       in map f mps

f4 :: [Music Pitch] -> [Music Pitch]
f4 mps = let f (Prim (Note d p)) = note (d/2) p
       in map f mps

g1 :: [Pitch] -> [Music Pitch]
g1 p = let g p1 = note en p1
       in map g p 

notes :: [Pitch]
notes = [(A, 4), (B, 5), (C, 4), (D, 4), (E, 4), (F, 4), (C, 5), (B, 4)]

melody :: [Music Pitch]
melody = g1 ((f1 0 notes) ++ (f1 2 notes) ++ (f1 4 notes) ++ (f1 (-2) notes)) 

melody2 :: [Music Pitch]
melody2 = g1 ((f1 4 notes) ++ (f1 6 notes) ++ (f1 (0) notes) ++ (f1 2 notes)) 

melody3 :: [Music Pitch]
melody3 = g1 ((f1 8 notes) ++ (f1 10 notes) ++ (f1 (-4) notes) ++ (f1 6 notes)) 

main :: IO ()
main = play $ chord [line (melody ++ (f4 melody) ++ (f4 (f4 melody)) ++ (f4 (f4 melody)) ++ melody ++ [Prim (Note hn (B, 4)), Prim (Note hn (C, 5))]), 
                     line (melody ++ melody ++ melody ++ [Prim (Note hn (D, 5)), Prim (Note hn (E, 5))]),
                     line (melody2 ++ melody2 ++ melody2 ++ [Prim (Note hn (F, 5)), Prim (Note hn (G, 5))]),
                     line (melody3 ++ melody3 ++ melody3 ++ [Prim (Note hn (A, 5)), Prim (Note hn (C, 4))])
                     ]
