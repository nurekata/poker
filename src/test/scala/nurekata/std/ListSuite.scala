package nurekata.std

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import nurekata.std.*
import Math.*
import nurekata.fp.Applicative
import nurekata.fp.Traverse

class ListSuite extends ScalaCheckSuite:

   given Applicative[Gen] with
      extension [A](a: A)
         def pure: Gen[A] =
            Gen.const(a)

      extension [A, B](ff: Gen[A => B])
         override def ap(fa: Gen[A]): Gen[B] =
            ff.flatMap(fa.map)

   given Traverse[List] with
      extension [A](as: List[A])
         def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
            as.foldRight(List.empty.pure)((a, acc) =>
               f(a).map2(acc)((b: B, bs) => b :: bs)
            )

   val int: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

   extension [A](g: Gen[A])
      def replicateA(n: Int): Gen[List[A]] =
         List.fill(n)(g).sequence

   def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      g.replicateA(n)

   val list: Gen[List[Int]] =
      Gen.sized(s =>
         Gen.choose(0, max(s, 0))
            .flatMap(n => listOfN(n, int))
      )

   val nonEmptyList: Gen[List[Int]] =
      Gen.sized(s =>
         Gen.choose(1, max(s, 1))
            .flatMap(n => listOfN(n, int))
      )

   given arbList: Arbitrary[List[Int]] = Arbitrary(list)

   property("reverse") {
      forAll(list) { ls =>
         ls.reverse.reverse == ls
      }
   }

   property("sorted") {
      forAll(list) { ls =>
         val sorted = ls.sorted
         sorted.isEmpty || sorted.zip(sorted.tail).forall((a, b) => a <= b) &&
         sorted.length == ls.length &&
         sorted.forall(ls.contains) // sorted.diff(ls).isEmpty is better
      }
   }

   property("last") {
      forAll(nonEmptyList) { ls =>
         ls.last == ls.reverse.head
      }
   }

   test("empty list length") {
      assert(List.empty.length == 0)
   }

   property("length") {
      forAll { (xs: List[Int], ys: List[Int]) =>
         xs.length + ys.length == (xs ::: ys).length
      }
   }

   property("zip/unzip") {
      forAll { (xs: List[Int], ys: List[Int]) =>
         val (xss, yss) = xs.zip(ys).unzip
         xs.startsWith(xss) && ys.startsWith(yss) &&
         xss.length == yss.length &&
         xss.length == min(xs.length, ys.length)
      }
   }

   property("take") {
      forAll { (xs: List[Int], n: Int) =>
         val res = xs.take(n)
         res.length == min(max(n, 0), xs.length) &&
         xs.startsWith(res)
      }
   }

   property("drop") {
      forAll { (xs: List[Int], n: Int) =>
         xs.take(n) ::: xs.drop(n) == xs
      }
   }

   property("splitAt") {
      forAll { (xs: List[Int], n: Int) =>
         val (ls, rs) = xs.splitAt(n)
         ls ::: rs == xs &&
         ls.length == min(max(n, 0), xs.length)
      }
   }

   property("takeWhile") {
      forAll { (xs: List[Int], f: Int => Boolean) =>
         val suffix = xs.takeWhile(f)
         suffix.drop(suffix.length).headOption.forall(!f(_)) &&
         xs.startsWith(suffix)
         suffix.forall(f)
      }
   }

   property("dropWhile") {
      forAll { (xs: List[Int], f: Int => Boolean) =>
         xs.takeWhile(f) ::: xs.dropWhile(f) == xs
      }
   }

   property("span") {
      forAll { (xs: List[Int], f: Int => Boolean) =>
         val (ls, rs) = xs.span(f)
         ls == xs.takeWhile(f) &&
         rs == xs.dropWhile(f)
      }
   }

   property("map") {
      forAll { (xs: List[Int], f: Int => Int, g: Int => Int) =>
         xs.map(f).length == xs.length &&
         xs.map(f).map(g) == xs.map(x => g(f(x))) &&
         xs.map(x => x) == xs
      }
   }
