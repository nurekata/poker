import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import nurekata.std.List.*
import nurekata.std.*
import nurekata.std.Ordering.>

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted
val cs = Card(Six, Diamonds) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
   Card(Seven, Diamonds) :: Card(Ten, Diamonds) :: Nil
cs.sorted

scala.collection.immutable.List(1, 2, 3, 4, 5).sorted
val rs = cs.map(_.rank)

Hand(Straight, rs) > Hand(Flush, rs)
Hand(Straight, Ace :: Nil) > Hand(Straight, rs)

List(1, 2, 3, 4).map(_ + 1)