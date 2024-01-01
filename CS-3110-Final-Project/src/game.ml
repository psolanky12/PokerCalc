open Card
open Hand

let get_hero_hand card1 card2 = Hand.create_hand card1 card2

let rec get_community_cards (comm : card list) (remaining_deck : card list)
    (num : int) =
  match num with
  | 0 -> (comm, remaining_deck)
  | _ ->
      let c, r =
        Card.take_a_card_spot remaining_deck
          (Random.int (List.length remaining_deck))
      in
      get_community_cards (c :: comm) r (num - 1)

let hand_and_community (player_hand : card list) (comm_cards : card list) =
  player_hand @ comm_cards

let get_two_cards (deck : card list) = get_community_cards [] deck 2

let rec finished_game (num_of_players : int) (hero_hand : card list)
    (comm_cards : card list) (remaining_cards : card list) (r : card list list)
    =
  let r' = hand_and_community hero_hand comm_cards in
  let r_final = r' :: r in
  match num_of_players with
  | 1 -> r_final
  | _ ->
      let rp1, rest = get_community_cards [] remaining_cards 2 in
      finished_game (num_of_players - 1) hero_hand comm_cards rest
        (hand_and_community rp1 comm_cards :: r_final)

let rec get_scores (hands : card list list) =
  match hands with
  | [] -> []
  | h :: t -> HandRater.get_score h :: get_scores t

and assign_players (hands : card list list) =
  let rec helper (hands' : card list list) (num : int) =
    match hands' with
    | [] -> []
    | h :: t -> num :: helper t (num + 1)
  in
  helper hands 0

let rec combine_two list1 list2 =
  if List.length list1 <> List.length list2 then
    failwith "lists are not the same length"
  else
    match (list1, list2) with
    | _, [] | [], _ -> []
    | h1 :: t1, h2 :: t2 -> (h1, h2) :: combine_two t1 t2

let evaluate_hands (hands : card list list) =
  let t1 = get_scores hands in
  let t2 = assign_players hands in
  combine_two t1 t2
