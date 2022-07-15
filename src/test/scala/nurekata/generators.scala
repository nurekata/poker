package nurekata

import org.scalacheck.Gen
import org.scalacheck.Gen.*

object generators:
   val accountGen: Gen[Account] =
      alphaNumStr.map(n => Account(n))

   val deckGen: Gen[Deck] = Gen.delay(Gen.const(Deck()))   
