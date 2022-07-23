package nurekata.fp

trait Traverse[F[_]]:
   extension [A](ga: F[A])
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]

   extension [G[_]: Applicative, A](fga: F[G[A]])
      def sequence: G[F[A]] =
         fga.traverse(identity)

object Traverse:
   given listTraverse: Traverse[List] with
      extension [A](as: List[A])
         def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
            as.foldRight(List.empty.pure)((a, acc) => f(a).map2(acc)(_ :: _))
