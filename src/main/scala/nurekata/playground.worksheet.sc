import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.*
import scala.Ordered.orderingToOrdered

val card = Card(Ten, Diamonds)
card.rank

Ten.isBroadway
Two.isBroadway

(Card(Ten, Diamonds) :: Card(Jack, Hearts) :: Nil).sorted
val cs = Card(Six, Clubs) :: Card(Nine, Diamonds) :: Card(Eight, Diamonds) ::
   Card(Ten, Hearts) :: Card(Ten, Diamonds) :: Card(Ten, Clubs) :: Card(
      Nine,
      Diamonds
   ) :: Nil
cs.sorted

scala.collection.immutable.List(1, 2, 3, 4, 5).sorted
val rs = cs.map(_.rank)
Hand.eval(cs)

Hand(Straight, rs) > Hand(Flush, rs)
Hand(Straight, Ace :: Nil) > Hand(Straight, rs)

List(1, 2, 3, 4).map(_ + 1)
