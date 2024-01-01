module type SeatType = sig
  (*describes the posiiton at the table that your opponent is sitting at. This
    is used to determine his general "range" that he may be playing. For example
    a player sitting at the BIG_BLIND position will have a large range than
    someone sitting in the CUTOFF position.*)
  type seat
end
