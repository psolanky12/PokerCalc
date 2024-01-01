open Card
open Hand
open Odds

(*the types of hands you can get once the community cards are put on board*)
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

(*a tuple containing a hand result and a list of all the ranks of each card in a
  player's list of cards containing the community cards. The rank list is used
  in case the hand_results of two players are the same. *)
type hand_value = hand_results * rank list

(* Takes two cards (card1 and card2) as inputs and returns 1 if card1 is greater
   than card 2, 0 if card1 is equal to card2, and -1 if card1 is less than
   card2. *)
val compare_card_value : 'a -> 'a -> int

(* Takes a list and comparator as inputs and sorts the list in increasing order
   as per the comparator. *)
val value_sorter : 'a list -> ('a -> 'a -> int) -> 'a list

(* Takes a list and value as inputs and finds how many values in the list are
   present.*)
val count : 'a list -> 'a -> int

(* Takes an int to represent "x of a kind" and will return the rank of which
   there are x of a kind. Takes an int, card list, and optional paramter of rank
   option with default value None as parameters. *)
val get_kind : int -> ?except:rank option -> card list -> rank option

(* Takes in a hand result as input and converts it to a string. *)
val hand_results_to_string : hand_results -> string

(* Converts the rank to an integer. Ace returns 1. Takes a rank as input. *)
val get_number_for_rank_straight : rank -> int

(* Takes a rank list as input and converts it to a string. *)
val rank_to_string : rank list -> string

(* Converts string_of_hand into a string. Takes an input of tuple of hand
   results and rank list. *)
val string_of_hand_score : hand_results * rank list -> string

(* Helper function for check_straight. Takes a card list, the previous value,
   and the count as input. Returns whether the set is a straight. *)
val check_straight_helper : card list -> int -> int -> bool

(* Takes a card list as an input and calls check_straight_helper on the set
   list. Returns a bool stating if it is a straight. *)
val check_straight : card list -> bool

(* Helper function for check_flush. Takes a card list and suit as inputs and
   checks if each card matches the inputted suit. *)
val check_flush_of_suit : card list -> suit -> bool

(* Takes a card list as input and calls check_flush_of_suit for every suit.
   Returns true if it is a flush. *)
val check_flush : card list -> bool

(* Takes an integer (k) and list as input and returns the first k elements of
   the list. *)
val firstk : int -> 'a list -> 'a list

(* Takes a card list as input and finds what result the hand gives. It returns a
   tuple of the hand result and list of ranks. *)
val get_score : card list -> hand_results * rank list
