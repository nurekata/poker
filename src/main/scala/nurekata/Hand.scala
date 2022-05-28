package nurekata

import nurekata.std.*

enum HandCategory:
   case HighCard
   case Pair
   case TwoPair
   case Trips
   case Straight
   case Flush
   case FullHouse
   case Quads
   case StraightFlush

export HandCategory.*

final case class Hand(category: HandCategory, ranks: List[Rank])

object Hand:
   given rankOrdDesc: Ordering[Rank] = Rank.ordering.reverse
   given cardOrdDesc: Ordering[Card] = Card.ordering.reverse

   def eval5(cs: List[Card]): Hand =
      val ranks = cs.map(c => c.rank).sorted
      (isFlush(cs), isStraight(ranks)) match
         case (true, true)  => Hand(StraightFlush, ranks)
         case (true, false) => Hand(Flush, ranks)
         case (false, true) => Hand(Straight, ranks)
         case _ =>
            val (rs, os) = occurs(ranks)
               .sortBy((_, n) => -n)
               .unzip
            os match
               case 4 :: _      => Hand(Quads, rs)
               case 3 :: 2 :: _ => Hand(FullHouse, rs)
               case 3 :: _      => Hand(Trips, rs)
               case 2 :: 2 :: _ => Hand(TwoPair, rs)
               case 2 :: _      => Hand(Pair, rs)
               case _           => Hand(HighCard, ranks)

   def isFlush(cs: List[Card]): Boolean =
      cs.forall(p => p.suit == cs.head.suit)

   def isStraight(sorted: List[Rank]): Boolean =
      sorted
         .zip(sorted.tail)
         .forall((h, l) => h.value == l.value + 1) ||
         isLowStraight(sorted)

   def isLowStraight(sorted: List[Rank]): Boolean =
      sorted == Ace :: Five :: Four :: Three :: Two :: Nil

   def occurs(sorted: List[Rank]): List[(Rank, Int)] =
      sorted match
         case Nil => Nil
         case x :: _ =>
            val (same, rest) = sorted.span(_ == x)
            (x, same.length + 1) :: occurs(rest)
