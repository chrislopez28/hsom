import Euterpea

changeOctave :: Int -> Music Pitch -> Music Pitch 
changeOctave 0 m = m
changeOctave n (Prim (Note d (pc, oct))) = (Prim (Note d (pc, oct + n)))

mapF :: [Music Pitch] -> Int -> [Music Pitch]
mapF xs y = map (\x -> changeOctave y x) xs

kyrie =  line [a 3 dqn, a 3 en, f 3 qn, bf 3 qn, 
           cs 3 dqn, cs 3 en, d 3 qn, enr, d 3 en, 
           e 3 dqn, d 3 sn, e 3 sn, f 3 en, e 3 sn, f 3 sn, g 3 en, f 3 sn, g 3 sn,
           a 3 en, g 3 en, f 3 en, e 3 en, d 3 en, c 3 en, b 2 en, e 3 en,
           a 2 en, f 3 sn, e 3 sn, d 3 sn, b 2 sn, c 3 sn, d 3 sn, e 3 qn, qnr]

kyrie2 = line [qnr, a 3 hn, g 3 sn, f 3 sn, c 3 sn, bf 2 sn, 
               c 3 qn, b 2 en, f 3 en, c 3 den, fs 3 sn, gs 3 sn, a 3 sn, f 3 sn, g 3 sn, 
               a 3 en, a 2 en, b 2 en, cs 3 en, d 3 qn, qnr]

m1 = line [kyrie, kyrie2]

m2 = line [wnr, wnr, wnr, hnr, kyrie]

m3 = line [wnr, christe]

m4 = line [wnr, wnr, wnr, wnr, hnr, christe]

christe = line [enr, e 3 en, e 3 en, e 3 en, f 3 sn, g 3 sn, f 3 sn, e 3 sn, f 3 sn, g 3 sn, e 3 sn, f 3 sn,
                g 3 sn, a 3 sn, g 3 sn, f 3 sn,   g 3 sn, a 3 sn, f 3 sn, g 3 sn,   a 3 en, bf 3 sn, a 3 sn,   g 3 sn, f 3 sn, e 3 sn, d 3 sn,
                cs 3 qn, e 3 qn, a 3 qn, g 3 qn,   f 3 qn, e 3 sn, f 3 sn,   d 3 sn, e 3 sn, fs 3 sn, gs 3 sn,
                a 3 qn, dhnr]

main :: IO()
main = play $ (m1 :=: m2 :=: m3 :=: m4);