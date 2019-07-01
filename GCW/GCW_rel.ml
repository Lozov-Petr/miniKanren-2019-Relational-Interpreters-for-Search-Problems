open MiniKanren
open MiniKanrenStd

type person =
  | G 
  | C 
  | W 
  | N 

let g () = !! G
let c () = !! C
let w () = !! W
let n () = !! N

type 'a0 gstate =
  | St of 'a0 * 'a0 * 'a0 * 'a0 

module For_gstate = (Fmap)(struct let rec fmap fa0 = function | St (a0_0, a0_1, a0_2, a0_3) -> St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3))
                                  type 'a0 t = 'a0 gstate end)

let rec st x__0 x__1 x__2 x__3 = inj (For_gstate.distrib (St (x__0, x__1, x__2, x__3)))

let checkState s q55 =
  fresh (i0 g0 c0 w0 q57) (s === (st i0 g0 c0 w0)) (conde [(i0 === g0) &&& (q57 === (!! true)); (q57 === (!! false)) &&& (i0 =/= g0)])
    (conde
       [(q57 === (!! true)) &&& (q55 === (!! true));
       fresh (q60) (q57 === (!! false)) (conde [(i0 === c0) &&& (q60 === (!! true)); (q60 === (!! false)) &&& (i0 =/= c0)])
         (conde [(q60 === (!! true)) &&& (conde [(i0 === w0) &&& (q55 === (!! true)); (q55 === (!! false)) &&& (i0 =/= w0)]); (q60 === (!! false)) &&& (q55 === (!! false))])])

let checkStep state step q43 =
  fresh (i0 g0 c0 w0) (state === (st i0 g0 c0 w0))
    (conde
       [(step === (n ())) &&& (q43 === (!! true));
       (step === (g ())) &&& (conde [(i0 === g0) &&& (q43 === (!! true)); (q43 === (!! false)) &&& (i0 =/= g0)]);
       (step === (c ())) &&& (conde [(i0 === c0) &&& (q43 === (!! true)); (q43 === (!! false)) &&& (i0 =/= c0)]);
       (step === (w ())) &&& (conde [(i0 === w0) &&& (q43 === (!! true)); (q43 === (!! false)) &&& (i0 =/= w0)])])

let step s p q16 =
  fresh (i0 g0 c0 w0) (s === (st i0 g0 c0 w0))
    (conde
       [fresh (q18 q19) (p === (g ())) (q16 === (st q18 q19 c0 w0)) (
          conde [(i0 === (!! true)) &&& (q18 === (!! false)); (i0 === (!! false)) &&& (q18 === (!! true))])
          (conde [(g0 === (!! true)) &&& (q19 === (!! false)); (g0 === (!! false)) &&& (q19 === (!! true))]);
       fresh (q25 q26) (p === (c ())) (q16 === (st q25 g0 q26 w0)) (conde [(i0 === (!! true)) &&& (q25 === (!! false)); (i0 === (!! false)) &&& (q25 === (!! true))])
         (conde [(c0 === (!! true)) &&& (q26 === (!! false)); (c0 === (!! false)) &&& (q26 === (!! true))]);
       fresh (q32 q33) (p === (w ())) (q16 === (st q32 g0 c0 q33)) (conde [(i0 === (!! true)) &&& (q32 === (!! false)); (i0 === (!! false)) &&& (q32 === (!! true))])
         (conde [(w0 === (!! true)) &&& (q33 === (!! false)); (w0 === (!! false)) &&& (q33 === (!! true))]);
       fresh (q39) (p === (n ())) (q16 === (st q39 g0 c0 w0)) (conde [(i0 === (!! true)) &&& (q39 === (!! false)); (i0 === (!! false)) &&& (q39 === (!! true))])])

let checkAnswer a q1 =
  fresh (startState finishState) (startState === (st (!! true) (!! true) (!! true) (!! true))) (
    finishState === (st (!! false) (!! false) (!! false) (!! false)))
    (let rec checkAnswer a state q4 =
       ((a === (nil ())) &&& (conde [(state === finishState) &&& (q4 === (!! true)); (q4 === (!! false)) &&& (state =/= finishState)])) |||
         (fresh (x xs q9) (a === (x % xs)) (checkStep state x q9)
            (conde
               [fresh (newState q12) (q9 === (!! true)) (step state x newState) (
                  checkState newState q12) (conde [(q12 === (!! true)) &&& (checkAnswer xs newState q4); (q12 === (!! false)) &&& (q4 === (!! false))]);
               (q9 === (!! false)) &&& (q4 === (!! false))])) in
     checkAnswer a startState q1)
