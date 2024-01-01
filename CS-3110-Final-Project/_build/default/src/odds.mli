(* Takes a string list as input and the number of players and finds the column
   in the string list corresponding to the number of players and returns the
   string. *)
val check_for_players : string list -> int -> string

(* Takes a string list list and string as input and checks if the string is in
   the list. If it is, it returns the string list. Otherwise it fails with
   "Invalid hand selection". *)
val check_for_hand : string list list -> string -> string list

(* Takes the hand (as a string) and the number of players as input and calls
   check_for_players and check_for_hand to read from the CSV file "HandOdds" and
   return the value as a string.*)
val get_odds : string -> int -> string
