import scala.annotation.tailrec
import scala.io.Source

object Day15 extends App {

  sealed trait Terrain {
    def toChar: Char
  }
  case object Wall extends Terrain {
    override def toChar: Char = '#'
  }
  case object Open extends Terrain {
    override def toChar: Char = '.'
  }

  object Area {
    def parse(lines: Vector[String]): Area = {
      val grid = lines.map(line =>
        line.toVector.map {
          case '#' => Wall
          case '.' => Open
          case 'G'|'E' => Open
          case other => throw new IllegalArgumentException(s"Unexpected input $other.")
        }
      )
      Area(grid)
    }
  }
  case class Area(grid: Vector[Vector[Terrain]]) {
    assert(grid.nonEmpty)

    def distance(from: Point)(to: Point): Int = {
      Math.abs(from.x - to.x) + Math.abs(from.y - to.y)
    }

    //sorted by (y,x)
    def adjacentTo(pos: Point): Vector[Point] = {
      Vector(
        pos.copy(y = pos.y-1),
        pos.copy(x = pos.x-1),
        pos.copy(x = pos.x+1),
        pos.copy(y = pos.y+1)
      ).filter(withinBounds)
    }

    def at(target: Point): Option[Terrain] = {
      if(withinBounds(target)) {
        Some(grid(target.y)(target.x))
      } else {
        None
      }
    }

    def withinBounds(target: Point): Boolean = {
      (target.x >= 0 && target.x < grid(0).size) &&
        (target.y >= 0 && target.y < grid.size)
    }
  }
  case class Point(x: Int, y: Int)

  sealed trait Race {
    def toChar: Char
  }
  case object Goblin extends Race {
    override def toChar: Char = 'G'
  }
  case object Elf extends Race {
    override def toChar: Char = 'E'
  }

  case class Creature(name: String, race: Race, position: Point, hitPoints: Int, attackPower: Int) {
    def enemyOf(other: Creature): Boolean = race != other.race
    def damage(points: Int): Creature = copy(hitPoints = hitPoints - points)
  }


  object Game {
    def parse(lines: Vector[String],initialHitPoints: Int = 200, initialAttackPower: Int = 3): Game = {
      val area = Area.parse(lines)
      val creatures = parseCreatures(lines, initialHitPoints, initialAttackPower)
      Game(area, creatures)
    }

    def parseCreatures(lines: Vector[String], initialHitPoints: Int, initialAttackPower: Int): Map[String, Creature] = {
      val creatures = lines.zipWithIndex.flatMap { case (line, y) =>
        line.toVector.zipWithIndex.collect {
          case ('E', x) => Creature(name = s"Elf-$x-$y", race = Elf, position = Point(x, y), hitPoints = initialHitPoints, attackPower = initialAttackPower)
          case ('G', x) => Creature(name = s"Goblin-$x-$y", race = Goblin, position = Point(x, y), hitPoints = initialHitPoints, attackPower = initialAttackPower)
        }
      }
      creatures.map(c => c.name -> c).toMap
    }
  }
  case class Game(area: Area, creatures: Map[String, Creature], noEnemiesFound: Boolean = false) {

    def reachableNeighbours(current: Point, self: String): Vector[Point] = area.adjacentTo(current).filter(canMoveTo(self))


    def shortestDistances(self: String, from: Point): Map[Point, Int] = {

      @scala.annotation.tailrec
      def shortestDistancesRec(unvisited: List[Point], visited: Set[Point], distances: Map[Point, Int]): Map[Point, Int] = {
//        println(s"In shortestDistancesRec $unvisited $visited $distances")
        unvisited match {
          case Nil => distances
          case current :: remaining =>
            val adjacentPoints = reachableNeighbours(current, self).filterNot(visited)
            val updatedDistances = adjacentPoints.foldLeft(distances) { (agg, neighbour) =>
              val updatedDistance = Math.min(distances(current) + 1, agg.getOrElse(neighbour, Int.MaxValue))
              agg.updated(neighbour, updatedDistance)
            }
            val updatedUnvisited = (remaining ++ adjacentPoints).distinct.sortBy(p => distances.getOrElse(p, Int.MaxValue))
            shortestDistancesRec(updatedUnvisited, visited + current, updatedDistances)
        }
      }

      shortestDistancesRec(List(from), Set.empty, Map(from -> 0))
    }
    def shortestPath(self: String, distances: Map[Point, Int], from: Point)(to: Point): Option[List[Point]] = {
      @scala.annotation.tailrec
      def constructShortestPathsRec(target: Point, current: Point, constructedPath: List[Point]): Option[List[Point]] = {
//        println(s"constructing shortest path: target: $Point current: $current constructed $constructedPath")
        if(current == target) {
          Some(current :: constructedPath)
        } else {
          val selectedNeighbour = reachableNeighbours(current, self).filterNot(constructedPath.contains)
            .filter(distances.contains)
            .filterNot(constructedPath.contains)
            .sortBy(pos => (distances(pos),pos.y,pos.x))
            .headOption

          selectedNeighbour match {
            case Some(neighbour) => constructShortestPathsRec(target, neighbour, current :: constructedPath)
            case None => None
          }
        }
      }

      if(from == to) {
        Some(List.empty)
      } else {
        reachableNeighbours(from, self)
          .flatMap(constructShortestPathsRec(_, to, List.empty))
          .sortBy(path => (path.size, path.headOption.map(_.y), path.headOption.map(_.x)))
          .headOption
      }
    }

    def doRound(): Game = {
      val turns = creatures.toList.sortBy { case (_, c) => c.position.y -> c.position.x }.map(_._1)
      turns.foldLeft(this){ (game, player) =>
        if(game.creatures.contains(player)) {
          game.doTurn(player)
        } else {
          game
        }
      }
    }

    def doTurn(player: String): Game = {
      println(s"Starting turn for player $player")
      val enemies = creatures.values.filter(_.enemyOf(creatures(player))).toSet
      if(enemies.isEmpty) {
        println(s"No enemies found, this will be the last round.")
        copy(noEnemiesFound = true)
      } else {
        moveInRange(player, enemies)
          .attack(player, enemies)
      }
    }

    def moveInRange(player: String, enemies: Set[Creature]): Game = {
      val self = creatures(player)
      val newPosition = positionToMove(self, enemies)
      val newSelf = newPosition match {
        case None =>
          println(s"Not moving $player, it will remain at position ${self.position}.")
          self
        case Some(newPos) =>
          println(s"Moving $player to $newPos.")
          self.copy(position = newPos)
      }
      copy(creatures = creatures.updated(player, newSelf))
    }

    def attack(player: String, enemies: Set[Creature]): Game = {
      val self = creatures(player)
      val target = enemies.toList
        .filter(enemy => area.adjacentTo(self.position).contains(enemy.position))
        .sortBy(enemy => (enemy.hitPoints, enemy.position.y, enemy.position.x))
        .headOption

      val damagedTarget = target.map(_.damage(self.attackPower))
      damagedTarget match {
        case None =>
          println(s"Player $player could not select an enemy to attack.")
          this
        case Some(enemy) if enemy.hitPoints <= 0 =>
          println(s"Player $player selected enemy ${enemy.name} to attack, and kills it.")
          copy(creatures = creatures - enemy.name)
        case Some(enemy) =>
          println(s"Player $player selected enemy ${enemy.name} to attack, it lives on with ${enemy.hitPoints} HP.")
          copy(creatures = creatures.updated(enemy.name, enemy))
      }
    }

    def positionToMove(self: Creature, enemies: Set[Creature]): Option[Point] = {
      val openAdjacentToEnemies = inRange(self.name, enemies).toList.sortBy(pos => area.distance(self.position)(pos) -> pos.y -> pos.x)
      println(s"Player: ${self.name} selected open spaces adjacent to enemies: $openAdjacentToEnemies.")

      val distances = shortestDistances(self.name, self.position)
      println(s"Calculated all shortest distances $distances")

      val chosen = openAdjacentToEnemies
        .flatMap(shortestPath(self.name, distances, self.position))
        .sortBy(p => (p.size, p.lastOption.map(_.y) , p.lastOption.map(_.x)))
        .headOption

      println(s"Player: ${self.name} selected destination ${chosen.map(_.lastOption)} with the path $chosen.")
      chosen.flatMap(_.headOption)
    }

    def inRange(self: String, enemies: Set[Creature]): Set[Point] = {
      enemies.map(_.position)
        .flatMap(area.adjacentTo)
        .filter(canMoveTo(self))
    }

    def canMoveTo(self: String)(target: Point): Boolean = {
      area.at(target).contains(Open) && !creatures.values.filterNot(_.name == self).exists(_.position == target)
    }

    def toLines: Vector[String] = {
      area.grid.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map {
          case (cell, x) =>
            val foundCreature = creatures.values.find(_.position == Point(x, y))
            foundCreature match {
              case None => cell.toChar
              case Some(creature) => creature.race.toChar
            }
        }.mkString
      }
    }
  }


  def playUntilEnd(game: Game, roundNr: Int = 0): (Game, Int) = {
    println(s"Playing round nr: $roundNr")
    println(s"**************************")
    game.toLines.foreach(println)
    println(s"**************************")
    if(game.noEnemiesFound) game -> roundNr
    else playUntilEnd(game.doRound(), roundNr+1)
  }

  def part1(lines: Vector[String]): Long = {
    val game = Game.parse(lines)
    val (endGame, rounds) = playUntilEnd(game)

    val sumOfallHitPoints = endGame.creatures.values.map(_.hitPoints).sum
    (rounds-1)*sumOfallHitPoints
  }

  val testInput1 = """#######
                     |#E..G.#
                     |#...#.#
                     |#.G.#G#
                     |#######""".stripMargin

  val testInput2 = """#######
                     |#.E...#
                     |#.....#
                     |#...G.#
                     |#######""".stripMargin

  val testInput3 = """#######
                   |#.G...#
                   |#...EG#
                   |#.#.#G#
                   |#..G#E#
                   |#.....#
                   |#######""".stripMargin

  val testInput4 = """#########
                     |#G..G..G#
                     |#.......#
                     |#.......#
                     |#G..E..G#
                     |#.......#
                     |#.......#
                     |#G..G..G#
                     |#########""".stripMargin

  val testInput5 = """#######
                     |#G..#E#
                     |#E#E.E#
                     |#G.##.#
                     |#...#E#
                     |#...E.#
                     |#######""".stripMargin

  val testInput6 = """#######
                     |#E..EG#
                     |#.#G.E#
                     |#E.##E#
                     |#G..#.#
                     |#..E#.#
                     |#######""".stripMargin

  val testInput7 = """#######
                     |#E.G#.#
                     |#.#G..#
                     |#G.#.G#
                     |#G..#.#
                     |#...E.#
                     |#######""".stripMargin

  val testInput8 = """#######
                     |#.E...#
                     |#.#..G#
                     |#.###.#
                     |#E#G#G#
                     |#...#G#
                     |#######""".stripMargin

  val testInput9 = """#########
                     |#G......#
                     |#.E.#...#
                     |#..##..G#
                     |#...##..#
                     |#...#...#
                     |#.G...G.#
                     |#.....G.#
                     |#########""".stripMargin


//  assert(part1(testInput3.split("\n").toVector) == 27730)
//  assert(part1(testInput5.split("\n").toVector) == 36334)
//  assert(part1(testInput6.split("\n").toVector) == 39514)
//  assert(part1(testInput7.split("\n").toVector) == 27755)
//  assert(part1(testInput8.split("\n").toVector) == 28944)
//  assert(part1(testInput9.split("\n").toVector) == 18740)
////
  val input = Source.fromResource("Day15-Input.txt").getLines().toVector
  println(part1(input)) //

}
