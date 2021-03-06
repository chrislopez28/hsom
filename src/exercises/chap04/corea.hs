import Euterpea

times :: Int -> Music a -> Music a
times 0 m = rest 0 
times n m = m :+: Main.times (n - 1) m

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d/8) (trans n p) :+: note (7 * d/8) p
graceNote n _ = error "Can only add a grace note to a note"

b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
b2 = addDur dqn [b 3, es 4, fs 4, es 4]
b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

bassLine = Main.times 3 b1 :+: Main.times 2 b2 :+: Main.times 4 b3 :+: Main.times 5 b1


v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b
v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
v1b = addDur en [cs 5, b 4]

v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

v2a = line [cs 5 (dhn + dhn), d 5 dhn,
            f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en]
v2b = addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+:
      addDur en [as 4, cs 5, fs 5, e 5, fs 5]
v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en] :+:
      e 5 en :+: enr :+:
      line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]
v2d = addDur en [fs 5, cs 5, e 5, cs 5,
                 a 4, as 4, d 5, e 5, fs 5]
v2e = line [graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
            graceNote 1 (cs 5 qn), b 4 (en + hn), cs 5 en, b 4 en]
v2f = line [fs 5 en, a 5 en, b 5 (hn + qn), a 5 en, fs 5 en, e 5 qn,
            d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]
v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+:
      b 4 (3 * dhn + hn)

childSong6 :: Music Pitch
childSong6 = let t = (dhn/qn) * (69/120)
             in instrument RhodesPiano
                           (tempo t (bassLine :=: mainVoice))

mainVoice = Main.times 3 v1 :+: v2

main :: IO()
main = play childSong6