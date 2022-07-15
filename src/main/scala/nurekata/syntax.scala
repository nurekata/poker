package nurekata

import cats.Applicative

object syntax:
   extension [A](xs: List[A])
      def sortedDesc(using ord: Ordering[A]): List[A] =
         xs.sorted(using ord.reverse)

      def sortDescBy[B](f: A => B)(using ord: Ordering[B]): List[A] =
         xs.sortBy(f)(using ord.reverse)

      def map2[B, C](ys: List[B])(f: (A, B) => C): List[C] =
         xs.flatMap(x => ys.map(f(x, _)))

   extension [F[_], A](as: List[A])(using applicative: Applicative[F])
      def traverse[B](f: A => F[B]): F[List[B]] =
         as.foldRight(applicative.pure(List.empty))((a, acc) =>
            f(a).map2(acc)(_ :: _)
         )

   extension [F[_], A](fas: List[F[A]])(using Applicative[F])
      def sequence: F[List[A]] =
         fas.traverse(identity)