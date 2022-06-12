package nurekata

import scala.Ordering.Implicits.seqOrdering

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

   def eval(cs: List[Card]): Hand =
      val ranks = cs.map(c => c.rank).sortedDesc
      flush(cs)
         .map(rs =>
            straight(rs)
               .map(r => Hand(StraightFlush, List(r)))
               .getOrElse(Hand(Flush, rs.take(5)))
         )
         .orElse(
            straight(ranks)
               .map(r => Hand(Straight, List(r)))
         )
         .getOrElse(repeats(ranks))

   def repeats(sorted: List[Rank]): Hand =
      val (os, rs) = occurs(sorted).sortedDesc.unzip
      os match
         case 4 :: _      => Hand(Quads, List(rs.head, rs.tail.max))
         case 3 :: 2 :: _ => Hand(FullHouse, rs.take(2))
         case 3 :: _      => Hand(Trips, rs.take(3))
         case 2 :: 2 :: _ => Hand(TwoPair, rs.take(2) :+ rs.drop(2).max)
         case 2 :: _      => Hand(Pair, rs.take(4))
         case _           => Hand(HighCard, sorted.take(5))

   def flush(cs: List[Card]): Option[List[Rank]] =
      cs.groupBy(_.suit)
         .values
         .collectFirst { case cs if cs.size >= 5 => cs.map(_.rank).sortedDesc }

   def straight(sorted: List[Rank]): Option[Rank] =
      def lowStraight(rs: List[Rank]): Option[Rank] =
         Option.when(
            rs.head == Ace &&
               rs.endsWith(List(Five, Four, Three, Two))
         )(Five)

      val distinct =
         sorted
            .zip(sorted.tail)
            .collect { case (h, l) if h != l => h }

      distinct.view
         .zip(distinct.drop(4))
         .collectFirst { case (h, l) if h.value == l.value + 4 => h }
         .orElse(lowStraight(distinct))

   def occurs(sorted: List[Rank]): List[(Int, Rank)] =
      sorted match
         case Nil => Nil
         case x :: _ =>
            val (same, rest) = sorted.span(_ == x)
            (same.length, x) :: occurs(rest)

extension [A](xs: List[A])
   def sortedDesc(using ord: Ordering[A]): List[A] =
      xs.sorted(using ord.reverse)
