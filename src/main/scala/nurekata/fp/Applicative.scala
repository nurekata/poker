package nurekata.fp

trait Applicative[F[_]] extends Functor[F]:
   extension [A](a: A) def pure: F[A]

   extension [A](fa: F[A])
      def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
         f.curried.pure.ap(fa).ap(fb)

      def map[B](f: A => B): F[B] =
         f.pure.ap(fa)

   extension [A, B](ff: F[A => B])
      def ap(fa: F[A]): F[B] =
         ff.map2(fa)((f, a) => f(a))

   def unit: F[Unit] = ().pure
