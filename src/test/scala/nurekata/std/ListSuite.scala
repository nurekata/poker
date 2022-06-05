package nurekata.std

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import nurekata.std.*
import Math.*

class ListSuite extends ScalaCheckSuite:

   val intList: Gen[List[Int]] =
      Arbitrary
         .arbContainer[Seq, Int]
         .arbitrary
         .map(s => List.from(s))

   property("reverse") {
      forAll(intList) { ls =>
         ls.reverse.reverse == ls
      }
   }

   property("sorted") {
      forAll(intList) { ls =>
         val sorted = ls.sorted
         sorted.isEmpty || sorted.zip(sorted.tail).forall((a, b) => a <= b) &&
         sorted.length == ls.length &&
         sorted.forall(ls.contains) // sorted.diff(ls).isEmpty
      }
   }
