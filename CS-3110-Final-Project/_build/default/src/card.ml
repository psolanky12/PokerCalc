type face =
  | Jack
  | Queen
  | King
  | Ace

type rank =
  | Num of int
  | Face of face

type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

type card = {
  suit : suit;
  rank : rank;
}

let create suit rank = { suit; rank }

let compare_cards (card1 : card) (card2 : card) : int =
  match (card1.rank, card2.rank) with
  | Num x, Num y -> if x > y then 1 else if x < y then -1 else 0
  | Face _, Num _ -> 1
  | Num _, Face _ -> -1
  | Face x, Face y -> (
      match (x, y) with
      | Ace, Ace | King, King | Queen, Queen | Jack, Jack -> 0
      | Ace, _ -> 1
      | King, y -> if y <> Ace then 1 else -1
      | Queen, y -> if y == Jack then 1 else -1
      | Jack, _ -> -1)

let size_of_deck = List.length

let equal_suit c1 c2 =
  match (c1.suit, c2.suit) with
  | Spades, Spades | Clubs, Clubs | Hearts, Hearts | Diamonds, Diamonds -> true
  | _ -> false

let equal_rank c1 c2 =
  match (c1.rank, c2.rank) with
  | Num 2, Num 2
  | Num 3, Num 3
  | Num 4, Num 4
  | Num 5, Num 5
  | Num 6, Num 6
  | Num 7, Num 7
  | Num 8, Num 8
  | Num 9, Num 9
  | Num 10, Num 10 -> true
  | Face Jack, Face Jack
  | Face Queen, Face Queen
  | Face King, Face King
  | Face Ace, Face Ace -> true
  | _ -> false

let equal_cards c1 c2 = equal_suit c1 c2 && equal_rank c1 c2

let insert c d =
  let rec assert_insert c d =
    match d with
    | [] -> c
    | h :: t ->
        if equal_cards h c then
          raise (Invalid_argument "this card already exists in this deck")
        else assert_insert c t
  in
  let c' = assert_insert c d in
  c' :: d

let empty = []

let rank_of_int n =
  match n with
  | 0 -> Face Ace
  | 1 -> Num 2
  | 2 -> Num 3
  | 3 -> Num 4
  | 4 -> Num 5
  | 5 -> Num 6
  | 6 -> Num 7
  | 7 -> Num 8
  | 8 -> Num 9
  | 9 -> Num 10
  | 10 -> Face Jack
  | 11 -> Face Queen
  | 12 -> Face King
  | _ -> raise (Invalid_argument "Not a valid value for the card ")

let suit_of_int n =
  match n with
  | 0 -> Clubs
  | 1 -> Spades
  | 2 -> Hearts
  | 3 -> Diamonds
  | _ -> raise (Invalid_argument "not a valid suit")

let pick_a_card d spot = List.nth d spot

let any_card_int d =
  let () = Random.self_init () in
  Random.int (List.length d)

let any_card d = pick_a_card d (any_card_int d)

let take_a_card_spot d spot =
  let rec rc_inner (n : int) (left : card list) (right : card list) :
      card * card list =
    if spot == n then (List.hd right, left @ List.tl right)
    else rc_inner (n + 1) (left @ [ List.hd right ]) (List.tl right)
  in
  if spot < List.length d then rc_inner 0 [] d
  else raise (Invalid_argument "card is not in the deck")

let take_a_card d c =
  let rec helper (card : card) (left : card list) (right : card list) =
    match right with
    | h :: t ->
        if card = h then (card, left @ t) else helper card (left @ [ h ]) t
    | [] -> failwith "card is not in deck"
  in
  helper c [] d

let take_any_amount_of_cards d spot =
  let rec helper (c : card list) (d' : card list) (y : int) =
    let a_card, new_deck = take_a_card_spot d' 0 in
    let hand = a_card :: c in
    if y - 1 == 0 then (hand, new_deck) else helper hand new_deck (y - 1)
  in
  helper [] d spot

let deck () =
  let rec deck_inner (deck : card list) (card_n : int) : card list =
    let suit_n = card_n / 13 in
    let face_n = card_n mod 13 in
    if card_n == 52 || suit_n > 3 then deck
    else
      deck_inner
        (create (suit_of_int suit_n) (rank_of_int face_n) :: deck)
        (card_n + 1)
  in
  deck_inner [] 0

let shuffle d =
  let rec helper (shuffled_deck : card list) (old_deck : card list) =
    match old_deck with
    | [] -> shuffled_deck
    | card :: rest ->
        let next_card, rest_of_deck =
          take_a_card_spot old_deck (any_card_int old_deck)
        in
        helper (next_card :: shuffled_deck) rest_of_deck
  in
  helper [] d

let int_of_face card =
  match card.rank with
  | Face Jack -> 11
  | Face Queen -> 12
  | Face King -> 13
  | Face Ace -> 14
  | _ -> failwith "not a face card"
