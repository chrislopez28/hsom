import Euterpea

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p + ap))
        in map f [0, 2, 4, 6, 8]

main :: IO ()
main = play $ line (wts (A, 4))