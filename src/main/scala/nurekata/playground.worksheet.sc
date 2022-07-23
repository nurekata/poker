import nurekata.Rank.*
import nurekata.Suit.*
import nurekata.Deck.*
import nurekata.*

val (g, d) = start(List(Account("bot1"), Account("bot2"), Account("bot3")))
   .run(Deck())

g.winners

def a[A](a: Int) = a

case class Greets[T](private val name: T) {
   def hello() = { println("Hello " + name) }
   def getName: T = name
}

val greets1: Greets[String] = Greets("John")
val greets2: Greets[Symbol] = Greets(Symbol("Jack"))
val greetsList1: List[Greets[?]] = List(greets1, greets2)
