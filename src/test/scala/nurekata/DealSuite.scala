package nurekata

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Gen.*
import org.scalacheck.Arbitrary
import Math.*
import nurekata.generators.*

class DealSuite extends ScalaCheckSuite:

   val accountsGen: Gen[List[Account]] =
      choose(2, 9).flatMap(n => listOfN(n, accountGen))

   property("dealToPlayers") {
      forAll(accountsGen, deckGen) { (as, deck) =>
         val (players, newDeck) = Deal.dealToPlayers(as).run(deck)
         val (pockets, accounts) = players.unzip(p => (p.pocket, p.account))
         val restoredDeck =
            pockets.flatMap((c1, c2) => List(c1, c2)) ::: newDeck.toList
         assertEquals(restoredDeck, deck.toList)
         assertEquals(accounts, as)
      }
   }
