package nurekata

object syntax:
   extension [A](xs: List[A])
      def sortedDesc(using ord: Ordering[A]): List[A] =
         xs.sorted(using ord.reverse)

      def sortDescBy[B](f: A => B)(using ord: Ordering[B]): List[A] =
         xs.sortBy(f)(using ord.reverse)

      def map2[B, C](ys: List[B])(f: (A, B) => C): List[C] =
         xs.flatMap(x => ys.map(f(x, _)))
