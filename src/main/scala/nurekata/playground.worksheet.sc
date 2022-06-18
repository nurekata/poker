import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.Deck.*
import nurekata.*

val (g, d) = start(List(Account("bot1"), Account("bot2"), Account("bot3")))
   .run(Deck())

g.winners
