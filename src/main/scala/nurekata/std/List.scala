package nurekata.std

import nurekata.std.Ordering
import scala.annotation.tailrec

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

   def flatMap[B](f: A => List[B]): List[B] =
      this match
         case Nil    => Nil
         case h :: t => f(h) ::: t.flatMap(f)

   def ::[B >: A](h: B): List[B] =
      List.::(h, this)

   def length: Int =
      foldLeft(0)((n, _) => n + 1)

   def contains[B >: A](e: B): Boolean =
      this match
         case Nil     => false
         case x :: xs => x == e || xs.contains(e)

   def isEmpty: Boolean =
      this == Nil

   def take(n: Int): List[A] =
      @tailrec
      def loop(xs: List[A], n: Int, acc: List[A]): List[A] =
         if n <= 0 || xs.isEmpty
         then acc
         else loop(xs.tail, n - 1, xs.head :: acc)

      loop(this, n, Nil).reverse

   @tailrec
   final def drop(n: Int): List[A] =
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

   def reverse: List[A] =
      foldLeft(empty)((acc, x) => x :: acc)

   def foldLeft[B](z: B)(f: (B, A) => B): B =
      this match
         case Nil    => z
         case h :: t => t.foldLeft(f(z, h))(f)

   def foldRight[B](z: B)(f: (A, B) => B): B =
      reverse.foldLeft(z)((b, a) => f(a, b))

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

   def zip[B](that: List[B]): List[(A, B)] =
      (this, that) match
         case (x :: xs, y :: ys) => (x, y) :: xs.zip(ys)
         case _                  => Nil

   def unzip[A1, A2](using asPair: A => (A1, A2)): (List[A1], List[A2]) =
      this match
         case Nil => (Nil, Nil)
         case h :: t =>
            val (l, r) = asPair(h)
            val (ls, rs) = t.unzip
            (l :: ls, r :: rs)

   def sorted[B >: A](using ord: Ordering[B]): List[A] =
      val m = length / 2
      if m == 0 then this
      else
         val (l, r) = splitAt(m)
         merge(l.sorted, r.sorted)

   private def merge[B >: A](left: List[A], right: List[A])(using
      ord: Ordering[B]
   ): List[A] =
      (left, right) match
         case (Nil, _) => right
         case (_, Nil) => left
         case (l :: ls, r :: rs) =>
            if ord.compare(l, r) <= 0
            then l :: merge(ls, right)
            else r :: merge(left, rs)

   def sortBy[B](f: A => B)(using ord: Ordering[B]): List[A] =
      sorted(using ord.on(f))

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

   override def toString: String = mkString("List(", ", ", ")")

object List:
   def empty[A]: List[A] = Nil

   def apply[A](as: A*): List[A] =
      as.foldRight(empty)((a, b) => a :: b)

   def from[A](xs: IterableOnce[A]): List[A] =
      xs.iterator.foldRight(empty)((a, b) => a :: b)

export List.*
