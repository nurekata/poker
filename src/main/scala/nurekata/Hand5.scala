package nurekata

object Hand5:
   import Hand.occurs

   def eval(cs: List[Card]): Hand =
      val ranks = cs.map(c => c.rank).sortedDesc
      (isFlush(cs), isStraight(ranks)) match
         case (true, true)  => Hand(StraightFlush, ranks)
         case (true, false) => Hand(Flush, ranks)
         case (false, true) => Hand(Straight, ranks)
         case _ =>
            val (os, rs) = occurs(ranks).sortedDesc.unzip
            val hc = os match
               case 4 :: _      => Quads
               case 3 :: 2 :: _ => FullHouse
               case 3 :: _      => Trips
               case 2 :: 2 :: _ => TwoPair
               case 2 :: _      => Pair
               case _           => HighCard
            Hand(hc, rs)

   def isFlush(cs: List[Card]): Boolean =
      cs.forall(p => p.suit == cs.head.suit)

   def isStraight(sorted: List[Rank]): Boolean =
      sorted.view
         .zip(sorted.tail)
         .forall((h, l) => h.value == l.value + 1) ||
         isLowStraight(sorted)

   def isLowStraight(sorted: List[Rank]): Boolean =
      sorted == Ace :: Five :: Four :: Three :: Two :: Nil
