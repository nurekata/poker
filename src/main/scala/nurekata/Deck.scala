package nurekata

import scala.util.Random

opaque type Deck = List[Card]

object Deck:
   private val suits = Suit.values
   private val ranks = Rank.values
   private[nurekata] val cards =
      for
         r <- ranks
         s <- suits
      yield Card(r, s)

   extension (deck: Deck)
      def dealPocket: ((Card, Card), Deck) = 
         val (cs, rest) = deck.splitAt(2)
         ((cs.head, cs.tail.head), rest)


   //TODO impure
   def apply(): Deck = List.from(Random.shuffle(cards))
