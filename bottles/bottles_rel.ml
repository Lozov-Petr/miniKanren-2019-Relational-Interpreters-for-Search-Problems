open MiniKanren
open MiniKanrenStd

type bottle =
  | Fst 
  | Snd 

let fst () = !! Fst
let snd () = !! Snd

type stepType =
  | Fill 
  | Empty 
  | Pour 

let fill () = !! Fill
let empty () = !! Empty
let pour () = !! Pour

type 'a0 gnat =
  | O 
  | S of 'a0 

module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)

let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))

let rec add a b q94 = ((a === (o ())) &&& (b === q94)) ||| (fresh (x) (a === (s x)) (add x (s b) q94))

let rec greater a b q90 =
  ((a === (o ())) &&& (q90 === (!! false))) ||| (fresh (x) (a === (s x)) (((b === (o ())) &&& (q90 === (!! true))) ||| (fresh (y) (b === (s y)) (greater x y q90))))

let rec sub a b q86 = ((b === (o ())) &&& (a === q86)) ||| (fresh (y) (b === (s y)) (((a === (o ())) &&& (q86 === (o ()))) ||| (fresh (x) (a === (s x)) (sub x y q86))))

let anotherBottle b q83 = ((b === (fst ())) &&& (q83 === (snd ()))) ||| ((b === (snd ())) &&& (q83 === (fst ())))

let createState bottle lvl1 lvl2 q80 = ((bottle === (fst ())) &&& (q80 === (pair lvl1 lvl2))) ||| ((bottle === (snd ())) &&& (q80 === (pair lvl2 lvl1)))

let checkStep state0 step0 capacities q42 =
  fresh (f s t b lvl1 lvl2) (state0 === (pair f s)) (step0 === (pair t b)) (
    ((b === (fst ())) &&& (f === lvl1)) ||| ((b === (snd ())) &&& (s === lvl1))) (
    ((b === (fst ())) &&& (s === lvl2)) ||| ((b === (snd ())) &&& (f === lvl2)))
    (conde
       [(t === (fill ())) &&& (conde [(lvl1 === (o ())) &&& (q42 === (!! true)); (q42 === (!! false)) &&& (lvl1 =/= (o ()))]);
       fresh (q56) (t === (empty ())) (capacities b q56) (conde [(lvl1 === q56) &&& (q42 === (!! true)); (q42 === (!! false)) &&& (lvl1 =/= q56)]);
       fresh (b' q62 q66 q67 q75) (t === (pour ())) (anotherBottle b b') (
         conde [(lvl1 === (o ())) &&& (q66 === (!! true)); (q66 === (!! false)) &&& (lvl1 =/= (o ()))]) (
         capacities b' q75) (conde [(lvl2 === q75) &&& (q67 === (!! true)); (q67 === (!! false)) &&& (lvl2 =/= q75)])
         (conde [(q66 === (!! true)) &&& (q62 === (!! true)); (q66 === (!! false)) &&& (q62 === q67)])
         (conde [(q62 === (!! true)) &&& (q42 === (!! false)); (q62 === (!! false)) &&& (q42 === (!! true))])])

let doStep state0 step0 capacities q25 =
  fresh (f s t b lvl2) (state0 === (pair f s)) (step0 === (pair t b)) (
    ((b === (fst ())) &&& (s === lvl2)) ||| ((b === (snd ())) &&& (f === lvl2)))
    (conde
       [fresh (q32) (t === (fill ())) (capacities b q32) (createState b q32 lvl2 q25);
       (t === (empty ())) &&& (createState b (o ()) lvl2 q25);
       fresh (sum cap2 q35 q39) (t === (pour ())) (add f s sum) (anotherBottle b q35) (
         capacities q35 cap2) (greater sum cap2 q39)
         (conde [fresh (q40) (q39 === (!! true)) (sub sum cap2 q40) (createState b q40 cap2 q25); (q39 === (!! false)) &&& (createState b (o ()) sum q25)])])

let isFinishState state0 reqLvl q12 =
  fresh (f s q13 q14) (state0 === (pair f s)) (conde [(f === reqLvl) &&& (q13 === (!! true)); (q13 === (!! false)) &&& (f =/= reqLvl)])
    (conde [(s === reqLvl) &&& (q14 === (!! true)); (q14 === (!! false)) &&& (s =/= reqLvl)])
    (conde [(q13 === (!! true)) &&& (q12 === (!! true)); (q13 === (!! false)) &&& (q12 === q14)])

let checkAnswer answer capacities reqLvl q11 =
  let rec checkAnswer state0 answer q3 =
    ((answer === (nil ())) &&& (isFinishState state0 reqLvl q3)) |||
      (fresh (x xs q5) (answer === (x % xs)) (checkStep state0 x capacities q5)
         (conde [fresh (q6) (q5 === (!! true)) (doStep state0 x capacities q6) (checkAnswer q6 xs q3); (q5 === (!! false)) &&& (q3 === (!! false))])) in
  fresh (startState) (startState === (pair (o ()) (o ()))) (checkAnswer startState answer q11)

let capacities1 b q0 = ((b === (fst ())) &&& (q0 === (s (s (s (s (o ()))))))) ||| ((b === (snd ())) &&& (q0 === (s (s (s (s (s (s (s (s (s (o ()))))))))))))
