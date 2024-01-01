open Poker
open Card
open Hand
open Odds
open Game
open HandComparison
open HandRater

exception InvalidSuit
exception InvalidRank
exception InvalidFormat
exception InvalidNumOfPeopleAtTable

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

(* Reading the user's input *)
let find_suit str =
  match String.trim str with
  | "Hearts" -> Hearts
  | "Diamonds" -> Diamonds
  | "Clubs" -> Clubs
  | "Spades" -> Spades
  | _ -> raise InvalidSuit

let is_int = function
  | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "10" -> true
  | _ -> false

(*Checks the rank of a card from a string to an actual rank from the Card
  module. Throws an exception if the string implemented doesn't match either a
  number 2-10 or a face card with the first letter capatalized.*)
let find_rank str =
  match String.trim str with
  | "King" -> Face King
  | "Queen" -> Face Queen
  | "Jack" -> Face Jack
  | "Ace" -> Face Ace
  | x -> if is_int x then Num (int_of_string x) else raise InvalidRank

(*transformed the inputted strings into an actual card using pattern
  matching.suit If the input isn't correct (is not two strings that match a rank
  and a suit), the function throws an error.*)
let card1 card1_transform : card option =
  match card1_transform with
  | [ suit1; rank1 ] -> Some { suit = find_suit suit1; rank = find_rank rank1 }
  | _ -> raise InvalidFormat

(* checks if the inputted number for the amount of people playing at the poker
   table is valid. If it isn't the function throws an error*)
let verify_number (num : string) =
  if int_of_string num < 1 || int_of_string num > 9 then
    raise InvalidNumOfPeopleAtTable

(* converts a list with at least two elements into a tuple. This is used when
   converting the list consisting of a suit and a rank into a tuple. This allows
   for easy access to pull the individual strings.*)
let list_to_tuple lst : string * string =
  match lst with
  | h1 :: h2 :: _ -> (h2, h1)
  | _ -> ("1", "1")
(* will never happen *)

let string_card card =
  match card.rank with
  | Face King -> "K"
  | Face Ace -> "A"
  | Face Queen -> "Q"
  | Face Jack -> "J"
  | Num x -> if x = 10 then "T" else string_of_int x

let hand_to_string hand1 hand2 =
  let string_card1 = string_card hand1 in
  let string_card2 =
    string_card hand2
    (* in let offsuit = is_suited (hand1.card1) (hand1.card2) in *)
  in
  string_card1 ^ string_card2

let suit_to_string (suit : Card.suit) =
  match suit with
  | Clubs -> "Clubs"
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"

let rank_to_string (rank : Card.rank) =
  match rank with
  | Face Ace -> "Ace"
  | Face King -> "King"
  | Face Queen -> "Queen"
  | Face Jack -> "Jack"
  | Num x -> string_of_int x

let rec print_community_cards = function
  | [] -> ""
  | h :: t ->
      print_endline
        ("The " ^ rank_to_string h.rank ^ " of " ^ suit_to_string h.suit);
      print_community_cards t

let get_flop (comm : card list) =
  let rec helper comm n =
    match n with
    | 3 -> []
    | _ -> (
        match comm with
        | h :: t -> h :: helper t (n + 1)
        | [] -> [])
  in
  helper comm 0

let take_out_option = function
  | Some x -> x
  | None -> failwith "nothing to take out"

let determine_winner (s : (hand_results * rank list) * int) =
  match snd s with
  | 0 -> print_endline "you win"
  | _ -> print_endline "you lost"

let rec simulate_games our_hand rem_deck players wins games_left =
  if games_left = 0 then wins
  else
    let comm_cards, rem_deck' = Game.get_community_cards [] rem_deck 5 in
    let rec add_other_players list deck players_to_add =
      let x, y = Card.take_a_card_spot deck (Random.int (List.length deck)) in
      let a, b = Card.take_a_card_spot y (Random.int (List.length y)) in
      if players_to_add = 1 then [ x; a ] :: list
      else add_other_players ([ x; a ] :: list) b (players_to_add - 1)
    in
    let hands_list = our_hand :: [] in
    let player_hands = add_other_players hands_list rem_deck' (players - 1) in
    let score_list =
      Game.evaluate_hands (List.map (fun x -> x @ comm_cards) player_hands)
    in
    let sorted_scores = order_scores score_list in
    let s, p = List.hd sorted_scores in
    let new_wins = if p = 0 then wins + 1 else wins in
    simulate_games our_hand rem_deck players new_wins (games_left - 1)

let check_swap c1 c2 =
  if HandComparison.int_of_rank c1.rank < HandComparison.int_of_rank c2.rank
  then
    let temp = c2 in
    let c2 = c1 in
    let c1 = temp in
    (c1, c2)
  else (c1, c2)

(*********** command line interface ***********)
let () =
  print_endline
    "\n\n\
     Welcome to Poker trainer. Would you like to learn how to play? (enter Y \
     or N)\n";
  let answer = read_line () in
  if answer = "Y" then
    print_endline
      "Hello whoever is using this poker simulation interface, this is how to \
       play. The overarching goal of poker is to create the best possible hand \
       of 5 cards. There are a minimum of 2 players are a maximum of 9 players \
       allowed at a poker table. Each seat at the table is given a certain \
       name, depending on the position of the dealer button. At the beginning \
       of each hand, the big blind and small blind are obligated to put in a \
       certain amount of money into the pot. Then, each player is given two \
       cards. These cards should not be shown to the rest of the people you \
       are playing with. After this, the person in the high jack position (the \
       person sitting to the right of the big blind) is up to play. This \
       player can either call, fold, or raise. Calling means they match the \
       biggest bet on the table. Folding means to throw your cards away and \
       not play this round. This is recommended for when your cards are not \
       good, and your chances of winning are low. Finally, raising is betting \
       an amount that is more than the biggest current bet. This happens for \
       every player. After this, three community cards are put onto the table, \
       for which all players can use. Once this happens, the betting round \
       happens again, only now players have the options to check, which means \
       they do not want to bet, but also do not want to fold. A player can \
       check if it is there turn and no one has bet yet. Once this happens, \
       another community card is revealed, and the betting round happens \
       again. Finally, a fifth community card is shown, and the betting round \
       happens one more time. A player can win if all the others players fold, \
       or if multiple players are still playing after the final betting round, \
       and you have the best hand. The possible suites are: Diamonds, Hearts, \
       Spades, and Clubs. \n\
       The possible ranks are integers from 2 to 10, or the face cards \n\
      \  (Jack, Queen, King, Ace). Everything is case sensitive. ";
  let guard1 = ref true in
  let card1_input = ref "" in
  let card1_transform = ref [] in
  while !guard1 do
    print_endline
      " \nPlease enter the first card for player 1 in the form [suit rank]. ";
    card1_input := read_line ();
    let card1_final : card option =
      try
        card1_transform := String.split_on_char ' ' !card1_input;
        card1 !card1_transform
      with
      | InvalidSuit -> None
      | InvalidRank -> None
      | InvalidFormat -> None
    in
    match card1_final with
    | Some _ -> guard1 := false
    | None ->
        print_endline
          "Either the suit, rank, or format in incorrect. Please read the \
           instructions more carefully and try again."
  done;
  let card1_tuple = list_to_tuple !card1_transform in
  let card1_final = card1 !card1_transform in
  let _ =
    print_endline
      ("Successfully saved Player 1's first card: The " ^ fst card1_tuple
     ^ " of " ^ snd card1_tuple)
  in

  let guard2 = ref true in
  let card2_input = ref "" in
  let card2_transform = ref [] in
  while !guard2 do
    print_endline
      "\nPlease enter the second card for player 1 in the form [suit rank]. ";
    card2_input := read_line ();
    let card2_final : card option =
      try
        card2_transform := String.split_on_char ' ' !card2_input;
        card1 !card2_transform
      with
      | InvalidSuit -> None
      | InvalidRank -> None
      | InvalidFormat -> None
    in
    match card2_final with
    | Some _ -> guard2 := false
    | None ->
        print_endline
          "Either the suit, rank, or format in incorrect. Please read the \
           instructions more carefully and try again."
  done;
  let card2_tuple = list_to_tuple !card2_transform in
  let card2_final = card1 !card2_transform in
  let _ =
    if
      Card.equal_cards
        (take_out_option card1_final)
        (take_out_option card2_final)
    then
      failwith "Cannot input the same card twice. Please restart the program."
    else ()
  in
  let _ =
    print_endline
      ("Successfully saved Player 1's second card: The " ^ fst card2_tuple
     ^ " of " ^ snd card2_tuple)
  in
  let _ =
    print_endline "\nPlease enter how many players are sitting at the table:"
  in
  let number_of_players = read_line () in
  let _ =
    print_endline
      "\nPlease enter how many times you would like to run the simulation:"
  in
  let num_of_sim = read_line () in
  let _ = verify_number number_of_players in
  let _ = print_endline "\nShuffling the deck..." in
  let deck = Card.deck () in
  let shuffled_deck = Card.shuffle deck in
  let _ =
    print_endline
      "Giving you and your opponents their cards and putting out community \
       cards ..."
  in
  let _, rest = Card.take_a_card shuffled_deck (take_out_option card1_final) in
  let _, rest' = Card.take_a_card rest (take_out_option card2_final) in

  if int_of_string num_of_sim == 1 then
    let comm, rest'' = Game.get_community_cards [] rest' 5 in
    let finished_hands =
      Game.finished_game
        (int_of_string number_of_players)
        [ take_out_option card1_final; take_out_option card2_final ]
        comm rest'' []
    in
    let _ = print_endline "The flop is ..." in
    let flop = get_flop comm in
    let _ = print_community_cards flop in
    let _ = print_endline "\nThe turn is\n     ..." in
    let _ = print_community_cards [ List.nth comm 3 ] in
    let _ = print_endline "\nThe river is ..." in
    let _ = print_community_cards [ List.nth comm 4 ] in
    let scores = Game.evaluate_hands finished_hands in
    let sorted_scores = HandComparison.order_scores scores in
    determine_winner (List.hd sorted_scores)
  else
    let full_game =
      simulate_games
        [ take_out_option card1_final; take_out_option card2_final ]
        rest'
        (int_of_string number_of_players)
        0 (int_of_string num_of_sim)
    in
    let _ =
      print_endline
        ("\nYou won " ^ string_of_int full_game ^ " games! " ^ "That's a "
        ^ string_of_float
            (100. *. (float_of_int full_game /. float_of_string num_of_sim))
        ^ "% win rate.")
    in
    let c1, c2 =
      check_swap (take_out_option card1_final) (take_out_option card2_final)
    in
    let calculated_odds =
      Odds.get_odds (hand_to_string c1 c2) (int_of_string number_of_players)
    in
    print_endline
      ("\nThe GTO chance of your hand winning against " ^ number_of_players
     ^ " is " ^ calculated_odds)
