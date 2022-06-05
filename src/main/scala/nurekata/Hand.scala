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

object HandCategory:
   given ordering: Ordering[HandCategory] = 
      Ordering.by(_.ordinal)

export HandCategory.*

final case class Hand(category: HandCategory, ranks: List[Rank])

object Hand:

   given ordering: Ordering[Hand] = 
      Ordering.by(h => (h.category, h.ranks))

   def eval5(cs: List[Card]): Hand =
      val ranks = cs.map(c => c.rank).sortedDesc
      (isFlush(cs), isStraight(ranks)) match
         case (true, true)  => Hand(StraightFlush, ranks)
         case (true, false) => Hand(Flush, ranks)
         case (false, true) => Hand(Straight, ranks)
         case _ =>
            val (os, rs) = occurs(ranks)
               .sortedDesc
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

   def occurs(sorted: List[Rank]): List[(Int, Rank)] =
      sorted match
         case Nil => Nil
         case x :: _ =>
            val (same, rest) = sorted.span(_ == x)
            (same.length, x) :: occurs(rest)

extension [A](xs: List[A])
   def sortedDesc(using ord: Ordering[A]): List[A] = 
      xs.sorted(using ord.reverse)
