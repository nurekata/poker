package nurekata.fp

trait Monad[F[_]] extends Applicative[F]:
   extension [A](a: A) def pure: F[A]

   extension [A](fa: F[A])
      def flatMap[B](f: A => F[B]): F[B]

      override def map[B](f: A => B): F[B] =
         fa.flatMap(a => f(a).pure)

      override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
         for
            a <- fa
            b <- fb
         yield f(a, b)

   extension [A, B](ff: F[A => B])
      override def ap(fa: F[A]): F[B] =
         ff.flatMap(f => fa.map(a => f(a)))
