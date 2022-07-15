package cats

import nurekata.syntax.sequence

trait Applicative[F[_]]:
   def pure[A](a: A): F[A]

   extension [A](fa: F[A])
      def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = 
         ???


   extension [A, B](ff: F[A => B])
      def ap(fa: F[A]): F[B] =
         ???
