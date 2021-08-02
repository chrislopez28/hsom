import Euterpea

changeOctave :: Int -> Music a -> Music a
changeOctave 0 m = m
changeOctave n m = Modify (Transpose (12 * n)) m

mapF :: [Music Pitch] -> Int -> [Music Pitch]
mapF xs y = map (\x -> changeOctave y x) xs

kyrie =  line [a 3 dqn, a 3 en, f 3 qn, bf 3 qn, 
           cs 3 dqn, cs 3 en, d 3 qn, enr, d 3 en, 
           e 3 dqn, d 3 sn, e 3 sn, f 3 en, e 3 sn, f 3 sn, g 3 en, f 3 sn, g 3 sn]

kyrie_bass = line [a 3 en, g 3 en, f 3 en, e 3 en, d 3 en, c 3 en, b 2 en, e 3 en,
              a 2 en, f 3 sn, e 3 sn, d 3 sn, b 2 sn, c 3 sn, d 3 sn, e 3 qn, qnr]

kyrie_alto = line [a 3 en, g 3 en, f 3 en, e 3 en, d 3 den, e 3 sn, fs 3 en, gs 3 en, 
             a 3 en, e 3 qn, d 3 sn, c 3 sn, b 2 qn, qnr,
             enr, e 3 en, a 3 qn, a 3 sn, g 3 sn, f 3 sn, e 3 sn, f 3 en, g 3 sn, f 3 sn,
             e 3 qn, qnr, enr, e 3 en, fs 3 en, gs 3 en, 
             a 3 qn, qnr, enr, a 3 en, d 3 en, c 3 en,
             bf 3 sn, a 3 sn, g 3 en, g 3 sn, a 3 sn, f 3 sn, g 3 sn, e 3 qn, f 3 en, g 3 en,
             c 3 qn, qnr]

kyrie2 = line [enr, c 3 qn, bf 2 en, 
               a 2 en, f 3 en, d 3 en, bf 3 en, enr, g 3 en, e 3 en, c 3 en,
               enr, e 3 sn, f 3 sn, g 3 en, f 3 sn, g 3 sn, a 3 en, e 3 en, f 3 en, cs 3 en,
               d 3 qn, e 3 en, fs 3 en, g 3 qn, qnr,
               enr, ef 3 en, d 3 en, c 3 en, bf 2 en, a 2 en, e 3 qn,
               ef 3 en, c 3 en, f 3 hn, ef 3 qn, 
               d 3 en, fs 3 en, g 3 en, a 3 en, d 3 en, af 3 en, g 3 qn]

bass1 = line [qnr, a 3 hn, g 3 sn, f 3 sn, c 3 sn, bf 2 sn, 
               c 3 qn, b 2 en, f 3 en, c 3 den, fs 3 sn, gs 3 sn, a 3 sn, f 3 sn, g 3 sn, 
               a 3 en, a 2 en, b 2 en, cs 3 en, d 3 qn, qnr]

christe = line [enr, e 3 en, e 3 en, e 3 en,   f 3 sn, g 3 sn, f 3 sn, e 3 sn,   f 3 sn, g 3 sn, e 3 sn, f 3 sn,
                g 3 sn, a 3 sn, g 3 sn, f 3 sn,   g 3 sn, a 3 sn, f 3 sn, g 3 sn,   a 3 en, bf 3 sn, a 3 sn,   g 3 sn, f 3 sn, e 3 sn, d 3 sn,
                cs 3 en, e 3 en, a 3 en, g 3 en]
                
christe_var1 = line [f 3 en, e 3 sn, f 3 sn,   d 3 sn, e 3 sn, fs 3 sn, gs 3 sn,
                a 3 qn, qnr, hnr]

christe_var2 = line [f 3 sn, e 3 sn, d 3 sn, c 3 sn,  b 2 qn,
                     c 3 qn, d 3 qn, e 3 qn, enr, e 3 en,
                     a 3 qn, a 3 sn, g 3 sn, f 3 sn, e 3 sn, f 3 en, d 3 en, g 3 qn, 
                     c 3 en, a 2 en, f 3 den, f 3 sn, e 3 qn, qnr,
                     enr, a 3 sn, g 3 sn, f 3 en, ef 3 en, d 3 qn, qnr,
                     enr, g 3 sn, f 3 sn, e 3 en, d 3 en, c 3 qn, f 3 en, e 3 en, 
                     f 3 qn, qnr]

christe2 = line [enr, c 3 en, c 3 en, c 3 en,
                 d 3 sn, e 3 sn, d 3 sn, c 3 sn, d 3 sn, e 3 sn, c 3 sn, d 3 sn, e 3 sn, f 3 sn, e 3 sn, d 3 sn, e 3 sn, f 3 sn, d 3 sn, e 3 sn,
                 f 3 en, g 3 sn, f 3 sn, e 3 sn, d 3 sn, c 3 sn, bf 2 sn, a 2 en, c 3 en, f 3 en, ef 3 en, 
                 d 3 en, c 4 qn, bf 3 sn, a 3 sn, g 3 sn, f 3 sn, ef 3 sn, d 3 sn, c 3 sn, d 3 sn, bf 2 sn, c 3 sn, 
                 d 3 dqn, d 3 en, g 2 qn, qnr]


soprano = line [wnr, wnr, wnr, hnr, changeOctave 2 kyrie]
                
alto = line [wnr, changeOctave 1 christe, changeOctave 1 christe_var1, wnr, wnr, 
             changeOctave 1 kyrie, changeOctave 1 kyrie_alto, changeOctave 1 kyrie,
             changeOctave 1 kyrie2]
tenor = line [wnr, wnr, wnr, wnr, hnr, changeOctave (1) christe]
bass = line [kyrie, kyrie_bass, bass1, changeOctave (-1) christe, changeOctave (-1) christe_var2, changeOctave (-1) christe, changeOctave (-1) christe2]

main :: IO()
main = play $ (tenor :=: alto :=: bass :=: soprano);
-- main = play $ (alto :=: bass)
-- main = play $ (alto)