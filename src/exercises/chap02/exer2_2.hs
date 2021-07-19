import Euterpea

data BluesPitchClass = Ro | MT | Fo | Fi | MS

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (Ro, o))) = Prim (Note d (C, o))
fromBlues (Prim (Note d (MT, o))) = Prim (Note d (Ef, o))
fromBlues (Prim (Note d (Fo, o))) = Prim (Note d (F, o))
fromBlues (Prim (Note d (Fi, o))) = Prim (Note d (G, o))
fromBlues (Prim (Note d (MS, o))) = Prim (Note d (Bf, o))
fromBlues (Prim (Rest d)) = Prim (Rest d)
fromBlues (m1 :+: m2) = fromBlues(m1) :+: fromBlues(m2)
fromBlues (m1 :=: m2) = fromBlues(m1) :=: fromBlues(m2)
fromBlues (Modify cntrl m1) = Modify cntrl (fromBlues(m1))

bassLine = fromBlues((ro 3 en) :+: (ro 3 en) :+: (fo 3 en) :+: (fo 3 en))
bassLine2 = fromBlues((ro 3 en) :+: (ro 3 en) :+: (mt 3 en) :+: (mt 3 en))
melody = fromBlues ((ro 4 qn) :+: (mt 4 qn) :+: (fo 4 qn) :+: (fi 4 qn) :+: (ms 4 qn) :+: (mt 4 qn) :+: (fo 4 qn) :+: (ms 3 qn))

main :: IO ()
main = play ( (melody :+: melody) :=: (bassLine :+: bassLine2 :+: bassLine :+: bassLine2 :+: bassLine :+: bassLine2 :+: bassLine :+: bassLine2))