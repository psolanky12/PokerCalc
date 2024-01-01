let check_for_players (data : string list) (players : int) : string =
  List.nth data (players - 1)

let rec check_for_hand (data : string list list) (hand : string) : string list =
  match data with
  | h :: t -> if List.hd h = hand then h else check_for_hand t hand
  | [] -> failwith "Invalid Hand Selection"

let get_odds (hand : string) (players : int) : string =
  let hand_data = Csv.load "data/HandOdds.csv" in
  check_for_players (check_for_hand hand_data hand) players
