package nurekata.std

import nurekata.std.*

trait Ordering[A]:
   def compare(x: A, y: A): Int

   def reverse: Ordering[A] =
      (x, y) => compare(y, x)

   def on[B](f: B => A): Ordering[B] =
      (x, y) => compare(f(x), f(y))

   def orElse(that: Ordering[A]): Ordering[A] =
      (x, y) =>
         val c = compare(x, y)
         if c != 0 then c
         else that.compare(x, y)

object Ordering:
   def apply[A](using ord: Ordering[A]): Ordering[A] = ord

   def by[A, B](f: A => B)(using ord: Ordering[B]): Ordering[A] =
      (x, y) => ord.compare(f(x), f(y))

   given int: Ordering[Int] =
      (x, y) => x.compare(y)

   extension [A](x: A)(using ord: Ordering[A])
      def >(y: A): Boolean = ord.compare(x, y) > 0
      def >=(y: A): Boolean = ord.compare(x, y) >= 0

   given tuple[A, B](using
      ordA: Ordering[A],
      ordB: Ordering[B],
   ): Ordering[(A, B)] =
      (x, y) =>
         val c1 = ordA.compare(x._1, y._1)
         if c1 != 0 then c1 else ordB.compare(x._2, y._2)

   given list[A](using ord: Ordering[A]): Ordering[List[A]] with
      def compare(xs: List[A], ys: List[A]) =
         (xs, ys) match
            case (Nil, Nil) => 0
            case (Nil, _)   => -1
            case (_, Nil)   => 1
            case (x :: xs, y :: ys) =>
               val c = ord.compare(x, y)
               if c != 0 then c else compare(xs, ys)
