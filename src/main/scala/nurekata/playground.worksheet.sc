import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import nurekata.std.List.*
import nurekata.std.*

def isRoyalFlush(cs: List[Card]): Boolean =
   cs.forall(p => p.suit == cs.head.suit && p.rank.isBroadway)

def isStraightFlush(cs: List[Card]): Boolean =
   isFlush(cs) && isStraight(cs)

def isFlush(cs: List[Card]): Boolean =
   cs.forall(p => p.suit == cs.head.suit)

def isLowStraight(sorted: List[Card]): Boolean =
   sorted.map(c => c.rank) == Ace :: Five :: Four :: Three :: Two :: Nil

def isStraight(cs: List[Card]): Boolean =
   val sorted = cs.sorted(using Ordering[Card].reverse)
   sorted.head.rank.ordinal == sorted.last.rank.ordinal + 4 ||
   isLowStraight(sorted)




isRoyalFlush(Card(Ten, Diamonds) :: Card(Jack, Diamonds) :: Nil)

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted
val cs = Card(Six, Diamonds) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
   Card(Seven, Diamonds) :: Card(Ten, Diamonds) :: Nil
cs.sorted

scala.collection.immutable.List(1, 2, 3, 4, 5).sorted

isStraightFlush(cs)
