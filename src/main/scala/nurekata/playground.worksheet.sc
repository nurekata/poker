import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import nurekata.std.List.*
import nurekata.std.*

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted
val cs = Card(Six, Diamonds) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
   Card(Seven, Diamonds) :: Card(Ten, Diamonds) :: Nil
cs.sorted

scala.collection.immutable.List(1, 2, 3, 4, 5).sorted