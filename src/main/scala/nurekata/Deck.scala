package nurekata

import scala.util.Random

opaque type Deck = List[Card]

object Deck:
   private val suits = Suit.values
   private val ranks = Rank.values
   private val cards =
      for
         r <- ranks
         s <- suits
      yield Card(r, s)

   // TODO impure
   def apply(): Deck = List.from(Random.shuffle(cards))

   extension (deck: Deck)
      def deal: (Card, Deck) =
         (deck.head, deck.tail)

opaque type Deal[A] = Deck => (A, Deck)

object Deal:
   extension [A](deal: Deal[A])
      def run(deck: Deck): (A, Deck) = deal(deck)

      def map[B](f: A => B): Deal[B] =
         d =>
            val (a, nd) = deal(d)
            (f(a), nd)

      def flatMap[B](f: A => Deal[B]): Deal[B] =
         d =>
            val (a, nd) = deal(d)
            f(a)(nd)

      def map2[B, C](dealB: Deal[B])(f: (A, B) => C): Deal[C] =
         for
            a <- deal
            b <- dealB
         yield f(a, b)

   extension [A](ds: List[Deal[A]])
      def sequence: Deal[List[A]] =
         ds.foldRight(pure(List.empty))((d, acc) => d.map2(acc)(_ :: _))

   def pure[A](a: A): Deal[A] =
      d => (a, d)

   def deal: Deal[Card] =
      Deck.deal

   def dealPocket: Deal[Pocket] =
      deal.map2(deal)((_, _))

   def dealToPlayer(acc: Account): Deal[Player] =
      dealPocket.map(p => Player(acc, p))

   def dealToPlayers(names: List[Account]): Deal[List[Player]] =
      names
         .map(dealToPlayer)
         .sequence

   def dealBoard: Deal[Board] =
      List.fill(5)(deal).sequence
