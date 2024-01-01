open Card
open Hand
open HandRater

(*[get_hero_score c1 c2] returns the hand contianing the two cards c1 and c2
  which are given by the user in the interface*)
val get_hero_hand : card -> card -> hand

(*[get_community_cards d1 d2 n] returns n random cards from d2 and inputs them
  into d1. This function returns a tuple of d1 and the remaining cards in d2.*)
val get_community_cards : card list -> card list -> int -> card list * card list

(*[hand_and_community d1 d2] appends two decks d1 and d2 together. This is used
  when determining what type of a hand a player has with their two cards and the
  community cards.*)
val hand_and_community : card list -> card list -> card list

(*[get_two_cards d1] gets two random cards and returns those two cards in a
  tuple with the remiaing cards in d1. Uses get_community_cards.*)
val get_two_cards : card list -> card list * card list

(*[finished_game n d1 comm rem cll] returns a list of list of cards, where each
  list consists of 7 cards, where 5 of them are the community cards comm, and 2
  are the cards each player is holding. The user's cards are contained in d1,
  and the remainder of the deck is rem. The function gives each of the n players
  cards until no more cards need to be dealt.*)
val finished_game :
  int -> card list -> card list -> card list -> card list list -> card list list

(*[get_scores cll] returns a list of each player's hand result and the ranking
  of each of their cards, including the community cards. For example, if a
  specific players card list was [4H, 4D, 4S, KH, 8S, 4C, 5C], this player would
  have a four of a kind.*)
val get_scores : card list list -> (hand_results * rank list) list

(*[assign_players cll] matches each player with an integer. This is merely to
  seperate each player from one another.*)
val assign_players : card list list -> int list

(*[combine_two l1 l2] merges two lists, where the first element in l1 and the
  first element in l2 become (h1, h2), and so on. Requires that l1 and l2 are
  the same length.*)
val combine_two : 'a list -> 'b list -> ('a * 'b) list

(*[combine cll] takes the hand_result of a certain player, their designated
  player int, and the ranks of all of their cards, and merges them just as in
  combine.*)
val evaluate_hands : card list list -> ((hand_results * rank list) * int) list
