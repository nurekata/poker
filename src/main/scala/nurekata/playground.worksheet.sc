import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import ListCard.*

enum ListCard:
   case Nil
   case Cons(hd: Card, tl: ListCard)

   def head =
      this match
         case Nil        => throw new NoSuchElementException
         case Cons(h, _) => h

   def forall(p: Card => Boolean): Boolean =
      this match
         case Nil        => true
         case Cons(h, t) => p(h) && t.forall(p)

def isRoyalFlush(cs: ListCard): Boolean =
   cs.forall(p => p.suit == cs.head.suit && p.rank.isBroadway)

def isFlush(cs: ListCard, suit: Suit): Boolean =
   cs match
      case Nil        => true
      case Cons(h, t) => h.suit == suit && isFlush(t, suit)

isRoyalFlush(Cons(Card(Ten, Diamonds), Cons(Card(Jack, Diamonds), Nil)))

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway


ListCard.Cons(
   Card(Ten, Diamonds),
   ListCard.Cons(Card(Jack, Hearts), ListCard.Nil)
)