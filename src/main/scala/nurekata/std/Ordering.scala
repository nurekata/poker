package nurekata.std

trait Ordering[A]:
   def compare(x: A, y: A): Int

   def reverse: Ordering[A] =
      (x, y) => compare(y, x)

   def on[B](f: B => A): Ordering[B] =
      (x, y) => compare(f(x), f(y))

object Ordering:
   def apply[A](using ord: Ordering[A]): Ordering[A] = ord

   def by[A, B](f: A => B)(using ord: Ordering[B]): Ordering[A] =
      (x, y) => ord.compare(f(x), f(y))

   given int: Ordering[Int] =
      (x, y) => x.compare(y)
