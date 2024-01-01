module type SeatType = sig
  type seat
end

module Seat : SeatType = struct
  type seat =
    | BIG_BLIND
    | SMALL_BLIND
    | BUTTON
    | CUTOFF
    | HIGH_JACK
    | LOW_JACK
    | MIDDLE_POSITION
    | UTG
end
