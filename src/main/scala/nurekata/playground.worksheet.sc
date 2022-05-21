import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import ListCard.*

enum ListCard:
   case Nil
   case ::(hd: Card, tl: ListCard)

   def head: Card =
      this match
         case Nil    => throw new NoSuchElementException
         case h :: _ => h

   def tail: ListCard =
      this match
         case Nil    => throw new NoSuchElementException
         case _ :: t => t

   def last: Card =
      this match
         case Nil      => throw new NoSuchElementException
         case h :: Nil => h
         case _ :: tl  => tl.last

   def ::(h: Card): ListCard =
      ListCard.::(h, this)

   def length: Int =
      this match
         case Nil    => 0
         case _ :: t => 1 + t.length

   def contains(e: Card): Boolean =
      this match
         case Nil     => false
         case x :: xs => x == e || xs.contains(e)

   def isEmpty: Boolean =
      this == Nil

   def take(n: Int): ListCard =
      this match
         case x :: xs if n > 0 =>
            x :: xs.take(n - 1)
         case _ => Nil

   def drop(n: Int): ListCard =
      if n <= 0 || isEmpty
      then this
      else tail.drop(n - 1)

   def takeWhile(p: Card => Boolean): ListCard =
      this match
         case x :: xs if p(x) =>
            x :: xs.takeWhile(p)
         case _ => Nil

   def dropWhile(p: Card => Boolean): ListCard =
      this match
         case x :: xs if p(x) => xs.dropWhile(p)
         case _               => this

   def :::(prefix: ListCard): ListCard =
      prefix match
         case Nil     => this
         case x :: xs => x :: xs ::: this

   def span(f: Card => Boolean): (ListCard, ListCard) =
      (takeWhile(f), dropWhile(f))

   def splitAt(i: Int): (ListCard, ListCard) =
      if i <= 0 then (Nil, this)
      else
         this match
            case Nil => (Nil, Nil)
            case h :: t =>
               val (l, r) = t.splitAt(i - 1)
               (h :: l, r)

   def sorted: ListCard =
      val m = length / 2
      if m == 0 then this
      else
         val (l, r) = splitAt(m)
         merge(l.sorted, r.sorted)

   private def merge(left: ListCard, right: ListCard): ListCard =
      (left, right) match
         case (Nil, _) => right
         case (_, Nil) => left
         case (l :: ls, r :: rs) =>
            if l >= r
            then l :: merge(ls, right)
            else r :: merge(left, rs)

   def forall(p: Card => Boolean): Boolean =
      this match
         case Nil    => true
         case h :: t => p(h) && t.forall(p)

   def exists(p: Card => Boolean): Boolean =
      this match
         case Nil    => false
         case h :: t => p(h) || t.exists(p)

   def mkString(start: String, sep: String, end: String): String =
      this match
         case Nil      => start + end
         case h :: Nil => start + h + end
         case h :: t   => t.mkString(start + h + sep, sep, end)

   override def toString = mkString("ListCard(", ", ", ")")

def isRoyalFlush(cs: ListCard): Boolean =
   cs.forall(p => p.suit == cs.head.suit && p.rank.isBroadway)

def isStraightFlush(cs: ListCard): Boolean =
   isFlush(cs) && isStraight(cs)

def isFlush(cs: ListCard): Boolean =
   cs.forall(p => p.suit == cs.head.suit)

def isStraight(cs: ListCard): Boolean =
   val sorted = cs.sorted
   sorted.head.rank.ordinal == sorted.last.rank.ordinal + 4

isRoyalFlush(Card(Ten, Diamonds) :: Card(Jack, Diamonds) :: Nil)

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted
val cs = Card(Six, Diamonds) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
   Card(Seven, Diamonds) :: Card(Ten, Diamonds) :: Nil

isStraightFlush(cs)
