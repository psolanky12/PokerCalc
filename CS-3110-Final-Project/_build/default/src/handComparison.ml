open HandRater
open Card
open Hand

let int_of_rank = function
  | Num x -> x
  | Face Jack -> 11
  | Face Queen -> 12
  | Face King -> 13
  | Face Ace -> 14

let rec actual_card_comparator l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | _, [] | [], _ -> failwith "lists are not the same length"
  | r1 :: t1, r2 :: t2 ->
      let dif = int_of_rank r1 - int_of_rank r2 in
      if dif = 0 then actual_card_comparator t1 t2 else dif

let score_type_to_number x =
  match x with
  | StraightFlush -> 8
  | FourOfAKind -> 7
  | FullHouse -> 6
  | Flush -> 5
  | Straight -> 4
  | ThreeOfAKind -> 3
  | TwoPair -> 2
  | OnePair -> 1
  | HighCard -> 0

let score_type_comparator x y =
  let v1 = score_type_to_number x in
  let v2 = score_type_to_number y in
  v1 - v2

let score_comparator (s1 : (hand_results * rank list) * int)
    (s2 : (hand_results * rank list) * int) =
  let w = score_type_comparator (fst (fst s1)) (fst (fst s2)) in
  if w <> 0 then w else actual_card_comparator (snd (fst s1)) (snd (fst s2))

let order_scores l = List.sort score_comparator l
