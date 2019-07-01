type nat = Z | S of nat

let rec eqNat a b =
  match a, b with
  | Z  , Z   -> true
  | S _, Z   -> false
  | Z  , S _ -> false
  | S x, S y -> eqNat x y

let eqPair a b =
  match a, b with
  | (a1, a2), (b1, b2) -> eqNat a1 b1 && eqNat a2 b2

let rec elem x g =
  match g with
  | []      -> false
  | y :: ys -> if eqPair x y then true
                             else elem x ys

let rec isPath c g =
  match c with
  | [_]            -> true
  | x1 :: x2 :: xs -> elem (x1, x2) g && isPath (x2 :: xs) g
