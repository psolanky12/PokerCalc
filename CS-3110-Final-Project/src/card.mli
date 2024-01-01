(*types of ranks of cards that do not have numbers on them*)
type face =
  | Jack
  | Queen
  | King
  | Ace

(*The value of a card. Can either be a face card or a card with numbers on it.*)
type rank =
  | Num of int
  | Face of face

(*The suit of a given card.*)
type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

(*The card itself. Each card has a suit and a rank.*)
type card = {
  suit : suit;
  rank : rank;
}

(*Compares the values of two cards. For this sake, we say that a 2 is the lowest
  value of card, an ace is the highest value of card, and the suits do not
  matter. A 0 is returned if the cards have equal value, a 1 is returned if the
  first card is of greater value, and a -1 is returned if the second card is of
  greater value*)
val compare_cards : card -> card -> int

(*The empty deck, which contains no cards.*)
val empty : card list

(*[create s r] creates a card with suit s and rank r*)
val create : suit -> rank -> card

(*[size_of_deck d] returns the size of the list d, which immitates the size of a
  given deck or group of cards.*)
val size_of_deck : card list -> int

(* [insert c d] inserts a card c into the end of the deck d. a card that is
   already inserted into the deck cannot be inserted again*)
val insert : card -> card list -> card list

(*Displays the rank of a card that is associated with a given integer. Requires
  that 0 <= n <= 12, where n is the integer input into the function.*)
val rank_of_int : int -> rank

(*Displays the suit of a card that is associated with a given integer. The order
  from least to greatest of suits is Clubs, Spades, Hearts, Diamonds. Requires
  that 0 <= n <= 3, where n is the integer input into the function.*)
val suit_of_int : int -> suit

(*[equal_suit c1 c2] determines whether two suits c1 and c2 are equal. Two suits
  are equal if they have the same suit.*)
val equal_suit : card -> card -> bool

(*[equal_rank c1 c2] determines whether two ranks c1 and c2 are equal. Two ranks
  are equal if they have the same rank.*)
val equal_rank : card -> card -> bool

(*[equal_cards c1 c2] determines whether two cards c1 and c2 are equal. Two
  cards are equal if they have the same suit and the same rank.*)
val equal_cards : card -> card -> bool

(*[pick_a_card d n] picks a card from a deck of cards d given the position that
  that card is at in the deck n. Requires that n is not negative and is less
  than the length of the list - 1.*)
val pick_a_card : card list -> int -> card

(*[take_a_card_spot d n] returns a given card taken from the deck d given the
  position the card is at in the deck n, and the rest of the deck without the
  just taken card. Raises an error if n is greater than the size of the deck.*)
val take_a_card_spot : card list -> int -> card * card list

(*[take_a_card d c] returns a given card taken from the deck d given card c, and
  the rest of the deck without the just taken card. Raises an error if c is not
  in the deck.*)
val take_a_card : card list -> card -> card * card list

(* takes n amount of cards out of the deck d. Returns the list of cards taken
   and the remaining cards not taken. Requires that at least one card is taken
   from the deck*)
val take_any_amount_of_cards : card list -> int -> card list * card list

(*Creates a deck of cards, which contains a card that has every comination of a
  suit and a rank. 52 cards will be put into the deck.*)
val deck : unit -> card list

(*[shuffle d] shuffles the deck of cards d. *)
val shuffle : card list -> card list
