open MiniKanren
open MiniKanrenStd

type 'a0 gnat = Z | S of 'a0

let rec fmap fa0 = function Z -> Z | S a0 -> S (fa0 a0)

module For_gnat = Fmap (struct
  let rec fmap fa0 = function Z -> Z | S a0 -> S (fa0 a0)

  type 'a0 t = 'a0 gnat
end)

let rec z () = inj (For_gnat.distrib Z)

and s x__0 = inj (For_gnat.distrib (S x__0))

let topLevel x0 x1 =
  let rec isPath y0 y1 = fresh (q1 q2 q3 q4) (y0 === nil () ||| (y0 === q1 % nil ()) ||| (y0 === q2 % (q3 % q4) &&& elemIsPath y1 q2 q3 q4))
  and elemIsPath y2 y3 y4 y5 =
    fresh (q1 q2 q3 q4)
      ( y2
      === pair q1 q2 % q3
      &&& (___eqNat y3 q1 !!true &&& ___eqNat y4 q2 !!true &&& isPath (y4 % y5) (pair q1 q2 % q3))
      ||| (y2 === q4 % q3 &&& (eqPairElem y3 y4 q4 q3 &&& isPath (y4 % y5) (q4 % q3))) )
  and eqPairElem y10 y11 y12 y13 =
    fresh (q1 q2 q3)
      ( y12 === pair q1 q2
      &&& (eqNatElem y13 y10 y11 q1 &&& ___eqNat y11 q2 q3)
      ||| (y12 === pair q1 q2 &&& (___eqNat y10 q1 !!true &&& ___eqNat y11 q2 !!false &&& ____elem y13 y11 y10)) )
  and eqNatElem y14 y15 y16 y17 =
    fresh (q1 q2 q3 q4)
      ( y15 === s q1
      &&& (y17 === z ())
      &&& ____elem y14 y16 (s q1)
      ||| (y15 === z () &&& (y17 === s q2) &&& ____elem y14 y16 (z ()))
      ||| (y15 === s q3 &&& (y17 === s q4) &&& _eqNatElem y14 y16 q3 q4) )
  and _eqNatElem y26 y27 y28 y29 =
    fresh (q1 q2 q3 q4)
      ( y28 === s q1
      &&& (y29 === z ())
      &&& ____elem y26 y27 (s (s q1))
      ||| (y28 === z () &&& (y29 === s q2) &&& ____elem y26 y27 (s (z ())))
      ||| (y28 === s q3 &&& (y29 === s q4) &&& (___eqNat q3 q4 !!false &&& ____elem y26 y27 (s (s q3)))) )
  and ____elem y39 y40 y41 =
    fresh (q1 q2 q3 q4) (y39 === pair q1 q2 % q3 &&& (___eqNat y41 q1 !!true &&& ___eqNat y40 q2 !!true) ||| (y39 === q4 % q3 &&& eqPairElem y41 y40 q4 q3))
  and ___eqNat y42 y43 y44 =
    fresh (q1 q2 q3 q4)
      ( y42 === z ()
      &&& (y43 === z ())
      &&& (y44 === !!true)
      ||| (y42 === s q1 &&& (y43 === z ()) &&& (y44 === !!false))
      ||| (y42 === z () &&& (y43 === s q2) &&& (y44 === !!false))
      ||| (y42 === s q3 &&& (y43 === s q4) &&& ___eqNat q3 q4 y44) )
  in
  isPath x0 x1
