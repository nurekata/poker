package nurekata

import Rank.*
import Suit.*
import nurekata.std.*

enum Rank:
   case Two, Three, Four, Five, Six, Seven, Eight, Nine,
      Ten, Jack, Queen, King, Ace

   def isBroadway: Boolean =
      this >= Rank.Ten

   val value: Int =
      ordinal + 2

   def >=(other: Rank): Boolean =
      ordinal >= other.ordinal

   override def toString: String =
      this match
         case Ace   => "A"
         case King  => "K"
         case Queen => "Q"
         case Jack  => "J"
         case Ten   => "T"
         case _     => value.toString

object Rank:
   given ordering: Ordering[Rank] =
      Ordering.by(_.ordinal)

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

   def >=(other: Card): Boolean =
      rank.value >= other.rank.value

object Card:
   given ordering: Ordering[Card] =
      Ordering.by(_.rank)
      
