open Card
open Hand
open Odds

type hand_results =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush

type hand_value = hand_results * rank list

let compare_card_value c1 c2 = if c1 > c2 then 1 else if c1 = c2 then 0 else -1
let value_sorter l f = List.sort f l

let count lst v =
  List.fold_left (fun acc x -> if v = x then acc + 1 else acc) 0 lst

let get_kind (n : int) ?(except : rank option = None) (hand : card list) =
  let vals = List.map (fun x -> x.rank) hand in
  List.find_opt
    (fun v ->
      match except with
      | Some v' when v = v' -> false
      | _ -> count vals v = n)
    vals

let hand_results_to_string rst =
  match rst with
  | HighCard -> "HighCard"
  | OnePair -> "OnePair"
  | TwoPair -> "TwoPair"
  | ThreeOfAKind -> "ThreeOfAKind"
  | Straight -> "Straight"
  | Flush -> "Flush"
  | FullHouse -> "FullHouse"
  | FourOfAKind -> "FourOfAKind"
  | StraightFlush -> "StraightFlush"

let get_number_for_rank_straight (input : rank) : int =
  match input with
  | Num x -> x
  | Face x -> (
      match x with
      | Ace -> 1
      | King -> 13
      | Queen -> 12
      | Jack -> 11)

let rec rank_to_string (ranks : rank list) =
  let str_of_rnk h =
    match h with
    | Num x -> string_of_int x ^ "; "
    | Face f -> (
        match f with
        | Jack -> "Jack; "
        | Queen -> "Queen; "
        | King -> "King; "
        | Ace -> "Ace; ")
  in
  match ranks with
  | [] -> "[]"
  | _ -> "[" ^ String.concat "; " (List.map str_of_rnk ranks) ^ "]"

let string_of_hand_score (x, y) =
  "(" ^ hand_results_to_string x ^ ", " ^ rank_to_string y ^ ")"

let rec check_straight_helper set previous_val count =
  match set with
  | h :: t ->
      let v = get_number_for_rank_straight h.rank in
      let adjusted_v = if v = 1 && previous_val = 13 then 14 else v in
      if (adjusted_v - previous_val) mod 11 = 1 then
        check_straight_helper t adjusted_v (count + 1)
      else if count > 4 then true
      else check_straight_helper t adjusted_v 1
  | [] -> count > 4

let check_straight (set : card list) =
  if List.length set = 0 then raise (Invalid_argument "Not a valid list length")
  else if (List.hd (List.rev set)).rank = Face Ace then
    let set = create Spades (Face Ace) :: set in
    check_straight_helper set (-1) 0
  else check_straight_helper set (-1) 0

let check_flush_of_suit set suit =
  if
    List.fold_left (fun acc x -> if x.suit = suit then acc + 1 else acc) 0 set
    > 4
  then true
  else false

let check_flush set =
  check_flush_of_suit set Spades
  || check_flush_of_suit set Diamonds
  || check_flush_of_suit set Hearts
  || check_flush_of_suit set Clubs

let rec firstk k xs =
  match xs with
  | [] -> failwith "list not long enough for application of this function"
  | h :: t -> if k = 1 then [ h ] else h :: firstk (k - 1) t

let get_score deck =
  let values = List.map (fun x -> x.rank) deck in
  let ordered_values =
    value_sorter values (fun x y -> -compare_card_value x y)
  in
  let is_straight = check_straight deck in
  let is_flush = check_flush deck in
  if is_straight && is_flush then (StraightFlush, ordered_values)
  else
    match get_kind 4 deck with
    | Some v ->
        ( FourOfAKind,
          v :: v :: v :: v
          :: firstk 1 (List.filter (fun x -> x <> v) ordered_values) )
    | _ -> begin
        let three_of_a_kind = get_kind 3 deck in
        let pair = get_kind 2 deck in
        match (three_of_a_kind, pair) with
        | Some v1, Some v2 -> (FullHouse, [ v1; v2 ])
        | _ when is_flush -> (Flush, ordered_values)
        | _ when is_straight -> (Straight, ordered_values)
        | Some v, None ->
            ( ThreeOfAKind,
              v :: v :: v
              :: firstk 2 (List.filter (fun x -> x <> v) ordered_values) )
        | None, Some v -> begin
            match get_kind 2 ~except:(Some v) deck with
            | Some v' ->
                ( TwoPair,
                  value_sorter [ v; v' ] compare_card_value
                  @ firstk 1
                      (List.filter (fun x -> x <> v && x <> v') ordered_values)
                )
            | None ->
                ( OnePair,
                  v :: firstk 3 (List.filter (fun x -> x <> v) ordered_values)
                )
          end
        | None, None -> (HighCard, firstk 5 ordered_values)
      end
