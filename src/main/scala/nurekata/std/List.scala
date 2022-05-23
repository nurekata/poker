package nurekata.std

import nurekata.std.Ordering

enum List[+A]:
   case Nil
   case ::(hd: A, tl: List[A])

   def head: A =
      this match
         case Nil    => throw new NoSuchElementException
         case h :: _ => h

   def tail: List[A] =
      this match
         case Nil    => throw new NoSuchElementException
         case _ :: t => t

   def last: A =
      this match
         case Nil      => throw new NoSuchElementException
         case h :: Nil => h
         case _ :: tl  => tl.last

   def map[B](f: A => B): List[B] =
      this match
         case Nil    => Nil
         case h :: t => f(h) :: t.map(f)

   def ::[B >: A](h: B): List[B] =
      List.::(h, this)

   def length: Int =
      this match
         case Nil    => 0
         case _ :: t => 1 + t.length

   def contains[B >: A](e: B): Boolean =
      this match
         case Nil     => false
         case x :: xs => x == e || xs.contains(e)

   def isEmpty: Boolean =
      this == Nil

   def take(n: Int): List[A] =
      this match
         case x :: xs if n > 0 =>
            x :: xs.take(n - 1)
         case _ => Nil

   def drop(n: Int): List[A] =
      if n <= 0 || isEmpty
      then this
      else tail.drop(n - 1)

   def takeWhile(p: A => Boolean): List[A] =
      this match
         case x :: xs if p(x) =>
            x :: xs.takeWhile(p)
         case _ => Nil

   def dropWhile(p: A => Boolean): List[A] =
      this match
         case x :: xs if p(x) => xs.dropWhile(p)
         case _               => this

   def :::[B >: A](prefix: List[B]): List[B] =
      prefix match
         case Nil     => this
         case x :: xs => x :: xs ::: this

   def span(f: A => Boolean): (List[A], List[A]) =
      (takeWhile(f), dropWhile(f))

   def splitAt(i: Int): (List[A], List[A]) =
      if i <= 0 then (Nil, this)
      else
         this match
            case Nil => (Nil, Nil)
            case h :: t =>
               val (l, r) = t.splitAt(i - 1)
               (h :: l, r)

   def sorted[B >: A](using ord: Ordering[B]): List[B] =
      val m = length / 2
      if m == 0 then this
      else
         val (l, r) = splitAt(m)
         merge(l.sorted, r.sorted)

   private def merge[B >: A](left: List[B], right: List[B])(using ord: Ordering[B]): List[B] =
      (left, right) match
         case (Nil, _) => right
         case (_, Nil) => left
         case (l :: ls, r :: rs) =>
            if ord.compare(l, r) <= 0
            then l :: merge(ls, right)
            else r :: merge(left, rs)

   def forall(p: A => Boolean): Boolean =
      this match
         case Nil    => true
         case h :: t => p(h) && t.forall(p)

   def exists(p: A => Boolean): Boolean =
      this match
         case Nil    => false
         case h :: t => p(h) || t.exists(p)

   def mkString(start: String, sep: String, end: String): String =
      this match
         case Nil      => start + end
         case h :: Nil => start + h + end
         case h :: t   => t.mkString(start + h + sep, sep, end)

   override def toString = mkString("List[A](", ", ", ")")
