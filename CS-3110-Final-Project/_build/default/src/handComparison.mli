open HandRater
open Card
open Hand

(*Converts the rank of a card to an integer. The face values are assigned 11,
  12, 13, and 14 for Jack, Queen, King, and Ace respectively. *)
val int_of_rank : rank -> int

(* Returns the difference in values between two cards. The face cards are
   represented by 11, 12, 13, and 14 for Jack, Queen, King, and Ace
   respectively. Takes two rank lists as inputs. *)
val actual_card_comparator : rank list -> rank list -> int

(* Converts the different scores to numbers. Takes input of type hand_result. *)
val score_type_to_number : hand_results -> int

(* Returns the difference of the two scores by converting the scores to a number
   then subtracting them. Takes score1 and score2 as inputs. *)
val score_type_comparator : hand_results -> hand_results -> int

(* Takes two tuple of tuple as input with hand results and a list of ranks in
   one tuple with an integer and evaluates which scores is higher. *)
val score_comparator :
  (hand_results * rank list) * int -> (hand_results * rank list) * int -> int

(* Sorts the scores in increasing order. Takes a list of tuple of a tuple with
   hand results and rank list in one tuple and int in the other tuple.*)
val order_scores :
  ((hand_results * rank list) * int) list ->
  ((hand_results * rank list) * int) list
