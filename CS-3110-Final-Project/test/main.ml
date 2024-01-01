open OUnit2
open Poker
open Card
open Game
open HandRater
open Hand

(*Our test plan is pretty similar to testing strategies in previous projects.
  This project uses test-driven development in order to ensure the correct
  functionality of each function. All of the functions that were not in our
  interface file were tested using OUnit, as this was the easiest way to ensure
  correctness. However, our interface and helper functions in the interface file
  were manually tested. We did this by typing many different combinations of
  hand and players sitting at the table, and ensured the proportion of times the
  user wins is within the ballpark. For interface testing, our tests involved
  trying to make the interface crash and testing that it does not unless
  intended to. Before actually implementing the function, we wrote test cases to
  fulfill full or mostly-full coverage of the function before building the
  implementation of the function. For this coverages, we implemented a black box
  testing strategy, which covers all possibilities for outputs based on
  reasoning, not based on the physical LOC. The reason some testing suits are
  mostly-full coverage is because for functions like shuffle(), the order of the
  cards in a deck are changed at random, so there is not a set answer to the
  exact order. However, we tested other constants about these types of
  functions, such as that the size of the deck inputted is equal to the output.
  The testing approach demonstrates the correctness of the system because it
  tests individual functions that lead to up building the core of the game,
  which is used by the interface. So, if the building blocks are correct, then
  the game is correct.*)

let card1 = create Spades (Num 9)
let card2 = create Hearts (Num 9)
let card3 = create Diamonds (Face King)
let card4 = create Clubs (Num 3)
let card5 = create Hearts (Face Ace)
let card6 = create Clubs (Face Ace)
let deck1 = [ card1; card2; card3; card4; card5 ]

let compare_card_tests =
  [
    ( "compare two equal cards" >:: fun _ ->
      assert_equal 0 (Card.compare_cards card1 card2) );
    ( "compare two cards where the first is greater than the second" >:: fun _ ->
      assert_equal 1 (Card.compare_cards card3 card1) );
    ( "comapre two cards where the first is weaker than the second" >:: fun _ ->
      assert_equal (-1) (Card.compare_cards card4 card5) );
    ( "compare two cards where the first is greater and the suits are the same"
    >:: fun _ -> assert_equal 1 (Card.compare_cards card5 card1) );
    ( "compare two cards where the first is greater and the suits are the same"
    >:: fun _ -> assert_equal (-1) (Card.compare_cards card1 card5) );
  ]

let insert_invalid_arg_test in1 in2 =
  let exn = Invalid_argument "..." in
  assert_raises exn (fun () ->
      try Card.insert in1 in2 with Invalid_argument _ -> raise exn)

let size_of_deck_test =
  [
    ("size of an empty deck" >:: fun _ -> assert_equal 0 (empty |> size_of_deck));
    ( "size of a non empty deck" >:: fun _ ->
      assert_equal 3
        (empty |> insert card1 |> insert card2 |> insert card3 |> size_of_deck)
    );
    ( "Insert raises exception when card in the deck" >:: fun _ ->
      insert_invalid_arg_test card1 deck1 );
  ]

let rank_invalid_arg_test input =
  let exn = Invalid_argument "..." in
  assert_raises exn (fun () ->
      try Card.rank_of_int input with Invalid_argument _ -> raise exn)

let rank_of_int_tests =
  [
    ("edge case int" >:: fun _ -> assert_equal (Face Ace) (Card.rank_of_int 0));
    ("non edge int case" >:: fun _ -> assert_equal (Num 9) (Card.rank_of_int 8));
    ("invalid int case" >:: fun _ -> rank_invalid_arg_test 16);
    ( "invalid int case with a negative number" >:: fun _ ->
      rank_invalid_arg_test (-1) );
  ]

let suit_invalid_arg_test input =
  let exn = Invalid_argument "..." in
  assert_raises exn (fun () ->
      try Card.suit_of_int input with Invalid_argument _ -> raise exn)

let suit_of_int_test =
  [
    ( "getting the suit clubs for a card" >:: fun _ ->
      assert_equal Clubs (Card.suit_of_int 0) );
    ( "getting the suit spades for a card" >:: fun _ ->
      assert_equal Spades (Card.suit_of_int 1) );
    ( "getting the suit hearts for a card" >:: fun _ ->
      assert_equal Hearts (Card.suit_of_int 2) );
    ( "getting the suit diamonds for a card" >:: fun _ ->
      assert_equal Diamonds (Card.suit_of_int 3) );
    ( "invalid argument for the suite of a card" >:: fun _ ->
      suit_invalid_arg_test 8 );
    ( "invalid argument for the suite of a card with a negative int" >:: fun _ ->
      suit_invalid_arg_test (-2) );
  ]

let pick_a_card_test =
  [
    ( "picking the first card in a deck of cards" >:: fun _ ->
      assert_equal card1 (Card.pick_a_card deck1 0) );
    ( "pick the last card in the deck of cards" >:: fun _ ->
      assert_equal card5 (Card.pick_a_card deck1 (List.length deck1 - 1)) );
    ( "picking a non-edge case card in a deck of more than 3 cards" >:: fun _ ->
      assert_equal card4 (Card.pick_a_card deck1 3) );
  ]

let takecard_invalid_arg_test in1 in2 =
  let exn = Invalid_argument "..." in
  assert_raises exn (fun () ->
      try Card.take_a_card_spot in1 in2 with Invalid_argument _ -> raise exn)

let take_a_card_spot_test =
  [
    ( "taking the first card from a deck, which won't change the rest of the \
       deck"
    >:: fun _ ->
      assert_equal
        (card1, [ card2; card3; card4; card5 ])
        (Card.take_a_card_spot deck1 0) );
    ( "taking the last card from a deck, not chaning the orientation of \
       neighboring cards"
    >:: fun _ ->
      assert_equal
        (card5, [ card1; card2; card3; card4 ])
        (Card.take_a_card_spot deck1 (List.length deck1 - 1)) );
    ( "taking a card from the middle of the deck, changing the neighboring of \
       more cards"
    >:: fun _ ->
      assert_equal
        (card3, [ card1; card2; card4; card5 ])
        (Card.take_a_card_spot deck1 2) );
    ( "invalid card input with empty deck" >:: fun _ ->
      takecard_invalid_arg_test [] 5 );
    ( "invalid card input with a non-empty deck" >:: fun _ ->
      takecard_invalid_arg_test [ card2; card3 ] 3 );
  ]

let take_a_card_test =
  [
    ( "taking a card from the middle of a list of cards" >:: fun _ ->
      assert_equal
        (card3, [ card1; card2; card4; card5 ])
        (Card.take_a_card deck1 card3) );
    ( "taking the last card in the deck" >:: fun _ ->
      assert_equal
        (card5, [ card1; card2; card3; card4 ])
        (Card.take_a_card deck1 card5) );
  ]

let take_any_amount_of_cards_test =
  [
    ( "take the minimum amount of  cards from the deck" >:: fun _ ->
      assert_equal
        ([ card1 ], snd (take_a_card_spot deck1 0))
        (Card.take_any_amount_of_cards deck1 1) );
    ( "taking all of the cards from a deck" >:: fun _ ->
      assert_equal
        (List.rev deck1, [])
        (Card.take_any_amount_of_cards deck1 (List.length deck1)) );
  ]

let deck_test =
  [
    ( "testing the size of a proper deck" >:: fun _ ->
      assert_equal 52 Card.(deck () |> size_of_deck) );
    ( "testing the expected first card added in a proper deck" >:: fun _ ->
      assert_equal card6 (List.hd (List.rev (Card.deck ()))) );
    ( "testing the expected last card added in a proper deck" >:: fun _ ->
      assert_equal card3 (List.hd (Card.deck ())) );
  ]

let shuffle_test =
  [
    ( "test that shuffle keeps the same number of cards in the deck as before"
    >:: fun _ -> assert_equal 52 Card.(deck () |> shuffle |> size_of_deck) );
    ( "test that shuffle keeps the same number of cards for a not full deck"
    >:: fun _ -> assert_equal 5 (Card.shuffle deck1 |> size_of_deck) );
  ]

let get_number_for_rank_test =
  [
    ( "testing getting the number of an int" >:: fun _ ->
      assert_equal 3 (HandRater.get_number_for_rank_straight (Num 3)) );
    ( "testing getting the number for a face card" >:: fun _ ->
      assert_equal 13 (HandRater.get_number_for_rank_straight (Face King)) );
  ]

let cs1 = create Hearts (Num 2)
let cs2 = create Diamonds (Num 3)
let cs3 = create Spades (Num 4)
let cs4 = create Spades (Num 5)
let cs5 = create Clubs (Num 6)
let cs6 = create Hearts (Face Ace)
let cs7 = create Hearts (Num 10)
let cs8 = create Hearts (Face Jack)
let cs9 = create Clubs (Face Queen)
let cs10 = create Diamonds (Face King)

let checkstr_invalid_arg_test in1 =
  let exn = Invalid_argument "..." in
  assert_raises exn (fun () ->
      try HandRater.check_straight in1 with Invalid_argument _ -> raise exn)

let check_straight_tests =
  [
    ( "testing a basic straight" >:: fun _ ->
      assert_equal true (HandRater.check_straight [ cs1; cs2; cs3; cs4; cs5 ])
    );
    ( "testing straight with ace as low card" >:: fun _ ->
      assert_equal true (HandRater.check_straight [ cs6; cs1; cs2; cs3; cs4 ])
    );
    ( "testing a non-straight seqeunce" >:: fun _ ->
      assert_equal false (HandRater.check_straight [ cs1; cs3; cs4; cs5; cs6 ])
    );
    ("testing an empty seqeunce" >:: fun _ -> checkstr_invalid_arg_test []);
    ( "tetsting straight with ace as high card" >:: fun _ ->
      assert_equal true (HandRater.check_straight [ cs7; cs8; cs9; cs10; cs6 ])
    );
    ( "testing straight with non-consec" >:: fun _ ->
      assert_equal false (HandRater.check_straight [ cs8; cs9; cs10; cs6; cs1 ])
    );
  ]

let check_flush_tests =
  [
    ( "testing a basic flush" >:: fun _ ->
      assert_equal true (HandRater.check_flush [ cs1; cs6; cs7; cs8; card2 ]) );
    ( "testing a set of cards that are not flush" >:: fun _ ->
      assert_equal false (HandRater.check_flush [ cs1; cs2; cs3; cs4; cs5 ]) );
  ]

let hand_to_list_test =
  [
    ( "hand_to_list for where the rank of both the cards has type Num"
    >:: fun _ ->
      assert_equal [ card1; card2 ]
        (Hand.hand_to_list (Hand.create_hand card1 card2)) );
    ( "hand_to_list for where the rank of both the cards has type Face"
    >:: fun _ ->
      assert_equal [ card3; card5 ]
        (Hand.hand_to_list (Hand.create_hand card3 card5)) );
    ( "hand_to_list for where the one of the cards has type Num and the other \
       has type Face "
    >:: fun _ ->
      assert_equal [ card1; card5 ]
        (Hand.hand_to_list (Hand.create_hand card1 card5)) );
  ]

let lst1 =
  [
    create Hearts (Num 9);
    create Hearts (Num 10);
    create Hearts (Face Jack);
    create Hearts (Face Queen);
    create Hearts (Face King);
  ]

let lst2 =
  [
    create Clubs (Num 8);
    create Hearts (Num 8);
    create Spades (Num 8);
    create Diamonds (Num 8);
    create Clubs (Face Jack);
  ]

let lst3 =
  [
    create Clubs (Num 7);
    create Hearts (Num 7);
    create Diamonds (Num 7);
    create Spades (Num 5);
    create Diamonds (Num 5);
  ]

let lst4 =
  [
    create Hearts (Face Ace);
    create Diamonds (Num 5);
    create Clubs (Num 5);
    create Spades (Num 5);
    create Hearts (Num 7);
  ]

let lst5 =
  [
    create Hearts (Num 2);
    create Spades (Num 2);
    create Clubs (Num 9);
    create Clubs (Num 9);
    create Hearts (Num 8);
  ]

let lst6 =
  [
    create Diamonds (Face King);
    create Hearts (Face King);
    create Spades (Face Jack);
    create Clubs (Num 2);
    create Spades (Num 3);
  ]

let lst7 =
  [
    create Diamonds (Face Ace);
    create Diamonds (Num 8);
    create Diamonds (Num 10);
    create Diamonds (Num 5);
    create Diamonds (Num 2);
  ]

let string_of_result (result : hand_value) : string =
  match result with
  | x, y ->
      Printf.sprintf "Score: %s, Values: %s"
        (string_of_hand_score (x, y))
        (String.concat "; " (List.map rank_to_string [ y ]))

let get_score_tests =
  [
    ( "testing straight flush" >:: fun _ ->
      let result = HandRater.get_score lst1 in
      assert_equal
        (StraightFlush, [ Face King; Face Queen; Face Jack; Num 10; Num 9 ])
        result );
    ( "testing four of a kind" >:: fun _ ->
      assert_equal
        (FourOfAKind, [ Num 8; Num 8; Num 8; Num 8; Face Jack ])
        (HandRater.get_score lst2) );
    ( "testing full house" >:: fun _ ->
      assert_equal (FullHouse, [ Num 7; Num 5 ]) (HandRater.get_score lst3) );
    ( "testing  straight" >:: fun _ ->
      assert_equal
        (Straight, [ Num 6; Num 5; Num 4; Num 3; Num 2 ])
        (HandRater.get_score [ cs1; cs2; cs3; cs4; cs5 ]) );
    ( "testing flush" >:: fun _ ->
      assert_equal
        (Flush, [ Face Ace; Face Jack; Num 10; Num 9; Num 2 ])
        (HandRater.get_score [ cs1; cs6; cs7; cs8; card2 ]) );
    ( "testing three of a kind" >:: fun _ ->
      assert_equal
        (ThreeOfAKind, [ Num 5; Num 5; Num 5; Face Ace; Num 7 ])
        (HandRater.get_score lst4) );
    ( "testing two pair" >:: fun _ ->
      let expected = (TwoPair, [ Num 2; Num 9; Num 8 ]) in
      let actual = HandRater.get_score lst5 in
      assert_equal expected actual );
    ( "testing one pair" >:: fun _ ->
      assert_equal
        (OnePair, [ Face King; Face Jack; Num 3; Num 2 ])
        (HandRater.get_score lst6) );
    ( "testing high card" >:: fun _ ->
      assert_equal
        (HighCard, [ Num 10; Num 6; Num 5; Num 3; Num 2 ])
        (HandRater.get_score [ cs1; cs2; cs4; cs5; cs7 ]) );
  ]

let actual_card_comparator_test =
  [
    ( "testing for when first player's highest rank is higher" >:: fun _ ->
      assert_equal 1
        (HandComparison.actual_card_comparator [ Face King; Num 8 ]
           [ Face Queen; Num 5; Num 7 ]) );
    ( "testing for when the second player's highest card is higher" >:: fun _ ->
      assert_equal (-3)
        (HandComparison.actual_card_comparator [ Num 5; Num 4; Num 3 ]
           [ Num 8; Num 2 ]) );
    ( "testing for when the first card's of both players are the same v1"
    >:: fun _ ->
      assert_equal 3
        (HandComparison.actual_card_comparator
           [ Face Ace; Face Ace; Num 9 ]
           [ Face Ace; Face Jack; Num 3; Num 2 ]) );
    ( "testing for when the first card's of both players are the same v2"
    >:: fun _ ->
      assert_equal (-6)
        (HandComparison.actual_card_comparator [ Face Queen; Num 2 ]
           [ Face Queen; Num 8 ]) );
  ]

let assign_players_test =
  [
    ( "assign_players for 1 player" >:: fun _ ->
      assert_equal [ 0 ] (Game.assign_players [ lst5 ]) );
    ( "assign_players for more than 1 player" >:: fun _ ->
      assert_equal [ 0; 1; 2 ] (Game.assign_players [ lst5; lst4; lst3 ]) );
  ]

let get_scores_test =
  [
    ( "get_scores for four of a kind" >:: fun _ ->
      assert_equal
        [ (FourOfAKind, [ Num 8; Num 8; Num 8; Num 8; Face Jack ]) ]
        (Game.get_scores [ lst2 ]) );
    ( "get_scores for a straight flush" >:: fun _ ->
      assert_equal
        [ (StraightFlush, [ Face King; Face Queen; Face Jack; Num 10; Num 9 ]) ]
        (Game.get_scores [ lst1 ]) );
    ( "get_scores for a full house" >:: fun _ ->
      assert_equal [ (FullHouse, [ Num 7; Num 5 ]) ] (Game.get_scores [ lst3 ])
    );
    ( "get_scores for a flush" >:: fun _ ->
      assert_equal
        [ (Flush, [ Face Ace; Num 10; Num 8; Num 5; Num 2 ]) ]
        (Game.get_scores [ lst7 ]) );
    ( "get_scores for multiple players" >:: fun _ ->
      assert_equal
        [
          (TwoPair, [ Num 2; Num 9; Num 8 ]);
          (ThreeOfAKind, [ Num 5; Num 5; Num 5; Face Ace; Num 7 ]);
        ]
        (Game.get_scores [ lst5; lst4 ]) );
  ]

let firstk_test =
  [
    ( "put 5 values" >:: fun _ ->
      assert_equal
        [ Face Ace; Num 10; Num 8; Num 5; Num 2 ]
        (HandRater.firstk 5 [ Face Ace; Num 10; Num 8; Num 5; Num 2 ]) );
    ( "firstk for 1 value, which is less than the size of the list" >:: fun _ ->
      assert_equal [ Face Ace ]
        (HandRater.firstk 1 [ Face Ace; Num 10; Num 8; Num 5; Num 2 ]) );
  ]

let score_type_tests =
  [
    ( "Testing score_type_to_number " >:: fun _ ->
      assert_equal 8 (HandComparison.score_type_to_number StraightFlush) );
    ( "Testing score_type_to_number " >:: fun _ ->
      assert_equal 0 (HandComparison.score_type_to_number HighCard) );
  ]

let score_type_comparator_tests =
  [
    ( "testing score type comparator on two different values" >:: fun _ ->
      assert_equal 2 (HandComparison.score_type_comparator FourOfAKind Flush) );
    ( "testing score type comparator on two same values" >:: fun _ ->
      assert_equal 0 (HandComparison.score_type_comparator TwoPair TwoPair) );
    ( "testing score for when the result is negative" >:: fun _ ->
      assert_equal (-1)
        (HandComparison.score_type_comparator FourOfAKind StraightFlush) );
  ]

let score_comparator_tests =
  [
    ( "testing score_comparator on different hand results" >:: fun _ ->
      assert_equal 5
        (HandComparison.score_comparator
           ((FullHouse, [ Num 2; Num 2 ]), 2)
           ((OnePair, [ Num 2; Num 3 ]), 3)) );
    ( "testing score_comparator on same hand results " >:: fun _ ->
      assert_equal 1
        (HandComparison.score_comparator
           ((FourOfAKind, [ Num 3; Num 2 ]), 2)
           ((FourOfAKind, [ Num 2; Num 4 ]), 3)) );
  ]

let compare_card_value_test =
  [
    ( "testing for two card values that have the same value" >:: fun _ ->
      assert_equal 0
        (HandRater.compare_card_value (create Spades (Face King)).rank
           (create Hearts (Face King)).rank) );
    ( "testing two cards where the first card has a greater value than the \
       second card"
    >:: fun _ ->
      assert_equal 1
        (HandRater.compare_card_value (create Diamonds (Face King)).rank
           (create Hearts (Face Jack)).rank) );
    ( "testing two cards where the first card has a lesser value than the \
       second card"
    >:: fun _ ->
      assert_equal (-1)
        (HandRater.compare_card_value (create Diamonds (Num 2)).rank
           (create Hearts (Face Jack)).rank) );
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           compare_card_tests;
           rank_of_int_tests;
           size_of_deck_test;
           rank_of_int_tests;
           suit_of_int_test;
           pick_a_card_test;
           take_a_card_spot_test;
           take_a_card_test;
           take_any_amount_of_cards_test;
           deck_test;
           get_number_for_rank_test;
           check_straight_tests;
           check_flush_tests;
           actual_card_comparator_test;
           assign_players_test;
           get_scores_test;
           firstk_test;
           get_score_tests;
           score_type_tests;
           score_type_comparator_tests;
           score_comparator_tests;
           compare_card_value_test;
           hand_to_list_test;
           shuffle_test;
         ]

let () = run_test_tt_main suite
