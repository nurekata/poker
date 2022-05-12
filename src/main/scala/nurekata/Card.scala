package nurekata

import Rank.*
import Suit.*

enum Rank:
   case Two, Three, Four, Five, Six, Seven, Eight, Nine,
      Ten, Jack, Queen, King, Ace

   def isBroadway: Boolean =
      ordinal >= Rank.Ten.ordinal

   def value =
      ordinal + 2

   override def toString: String =
      this match
         case Ace   => "A"
         case King  => "K"
         case Queen => "Q"
         case Jack  => "J"
         case Ten   => "T"
         case n     => value.toString

enum Suit:
   case Spades, Hearts, Diamonds, Clubs

   override def toString: String =
      this match
         case Spades   => "♠"
         case Hearts   => "♥"
         case Diamonds => "♦"
         case Clubs    => "♣"

case class Card(rank: Rank, suit: Suit):
   override def toString: String =
      rank.toString + suit.toString
