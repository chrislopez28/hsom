import Euterpea

main :: IO ()
main = play (twoFiveOne (C, 4) wn)

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne (pc, oct) dur = let root = note dur (pc, oct)
                               root2 = note (dur * 2) (pc, oct)
                               minorTwo = transpose 2 root :=: transpose 5 root :=: transpose 9 root
                               majorFive = transpose 7 root :=: transpose 11 root :=: transpose 14 root
                               majorOne = root2 :=: transpose 4 root2 :=: transpose 7 root2
                           in minorTwo :+: majorFive :+: majorOne
