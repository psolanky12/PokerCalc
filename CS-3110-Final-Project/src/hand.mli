open Card

(* identifies a player's hand, which consists of two cards*)
type hand

(* Creates a hand by taking in the first and second as inputs and storing them
   in a tuple. *)
val create_hand : card -> card -> hand

(* Converts the rank of the card into a string. Jack, Queen, King, and Ace are
   represented by J, Q, K, A, respectively, and the integers are represented as
   string of the int. Takes card as input. *)
val string_rank : card -> string

(* Converts the suit of the card into a string. Takes card as input. *)
val string_suit : card -> string

(* Converts the hand, which is a tuple of card1 and card2 and turns it into a
   list. *)
val hand_to_list : hand -> card list

(* converts the hand to a string, where each card is represented by a letter.
   For example, if a player has two aces, the function would return "AA"*)
val to_string : hand -> string
