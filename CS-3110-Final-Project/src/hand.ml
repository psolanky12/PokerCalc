open Card

type hand = {
  card1 : card;
  card2 : card;
}

let create_hand card1 card2 = { card1; card2 }

let string_rank card =
  match card.rank with
  | Face King -> "K"
  | Face Ace -> "A"
  | Face Queen -> "Q"
  | Face Jack -> "J"
  | Num x -> string_of_int x

let string_suit card =
  match card.suit with
  | Clubs -> "Clubs"
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"

let hand_to_list hand =
  match hand with
  | { card1; card2 } -> [ card1; card2 ]

let to_string hand1 =
  let string_card1 = string_rank hand1.card1 ^ string_suit hand1.card1 in
  let string_card2 = string_rank hand1.card2 ^ string_suit hand1.card2 in
  string_card1 ^ string_card2
