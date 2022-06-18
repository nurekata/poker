package nurekata

import nurekata.Deal.*
import nurekata.syntax.*

final case class Account(name: String)

type Pocket = (Card, Card)

final case class Player(account: Account, pocket: Pocket)

type Board = List[Card]

final case class Game(players: List[Player], board: Board):
   def winners =
      val ps = players
         .map(p => (Hand.eval(p.pocket, board), p))
         .sortDescBy((h, _) => h)
      ps.takeWhile((h, _) => h == ps.head._1)

def start(accounts: List[Account]): Deal[Game] =
   dealToPlayers(accounts).map2(dealBoard)(Game.apply)
