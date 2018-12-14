import scala.io.Source

object Day13 extends App {

  sealed trait Track

  case object Empty extends Track

  case object Vertical extends Track

  case object Horizontal extends Track

  case object RightCorner extends Track

  case object LeftCorner extends Track

  case object Crossroads extends Track

  sealed trait Direction {
    def goTo(move: Move): Direction = move match {
      case Straight => this
      case Left => nextCounterClockwise
      case Right => nextClockwise
    }
    def nextClockwise:Direction
    def nextCounterClockwise:Direction
  }

  case object North extends Direction {
    override def nextClockwise: Direction = East
    override def nextCounterClockwise: Direction = West
  }

  case object South extends Direction {
    override def nextClockwise: Direction = West
    override def nextCounterClockwise: Direction = East
  }

  case object West extends Direction {
    override def nextClockwise: Direction = North
    override def nextCounterClockwise: Direction = South
  }

  case object East extends Direction {
    override def nextClockwise: Direction = South
    override def nextCounterClockwise: Direction = North
  }


  sealed trait Move {
    def next(): Move
  }

  case object Left extends Move {
    override def next(): Move = Straight
  }

  case object Straight extends Move {
    override def next(): Move = Right
  }

  case object Right extends Move {
    override def next(): Move = Left
  }

  case class Point(x: Int, y: Int) {
    def move(direction: Direction): Point = {
      direction match {
        case North => Point(x, y-1)
        case South => Point(x, y+1)
        case East => Point(x+1, y)
        case West => Point(x-1, y)
      }
    }

  }

  case class Cart(direction: Direction, pos: Point, nextMove: Move, crashed: Boolean = false)

  /**
    * Example map to be parsed.
    * /---\
    * |   |  /----\
    * | /-+--+-\  |
    * | | |  X |  |
    * \-+-/  \-+--/
    * \------/
    */
  object State {

    def parse(lines: Vector[String]): (Vector[Vector[Track]], List[Cart]) = {
      val grid: Vector[Vector[Track]] = lines.map(line => line.toVector.map {
        case ' ' => Empty
        case '-' => Horizontal
        case '|' => Vertical
        case '/' => RightCorner
        case '\\' => LeftCorner
        case '+' => Crossroads
        case '^' => Vertical
        case 'v' => Vertical
        case '>' => Horizontal
        case '<' => Horizontal
      })

      val carts = lines.zipWithIndex.flatMap {
        case (line, y) => line.zipWithIndex.collect {
          case ('^', x) => Some(Cart(North, Point(x, y), Left))
          case ('v', x) => Some(Cart(South, Point(x, y), Left))
          case ('>', x) => Some(Cart(East, Point(x, y), Left))
          case ('<', x) => Some(Cart(West, Point(x, y), Left))
          case _ => None
        }
      }.flatten.toList
      println(carts)

      grid -> carts
    }
  }

  def moveCarts(grid: Vector[Vector[Track]], removeCrashedCarts: Boolean)(carts: List[Cart]): List[Cart] = {
    @scala.annotation.tailrec
    def moveCartsRec(cartsToMove: List[Cart], movedCarts: List[Cart]): List[Cart] = {
      cartsToMove match {
        case Nil => movedCarts
        case x :: xs =>
          val movedCart = moveCart(grid, xs++movedCarts)(x)
          if(movedCart.crashed && removeCrashedCarts) {
            moveCartsRec(xs.filterNot(_.pos == movedCart.pos), movedCarts.filterNot(_.pos == movedCart.pos))
          } else {
            moveCartsRec(xs, movedCart::movedCarts)
          }
      }
    }
    val sortedCarts = carts.sortBy(cart => cart.pos.y -> cart.pos.x)
    moveCartsRec(sortedCarts, List.empty)
  }

  def moveCart(grid: Vector[Vector[Track]], otherCarts: List[Cart])(cart: Cart): Cart = {
      val nextPosition = cart.pos.move(cart.direction)
      val nextTrack = grid(nextPosition.y)(nextPosition.x)
      val nextDirection = nextTrack match {
        case Vertical | Horizontal => cart.direction

        case RightCorner if cart.direction == North => East
        case RightCorner if cart.direction == South => West
        case RightCorner if cart.direction == East => North
        case RightCorner if cart.direction == West => South

        case LeftCorner if cart.direction == North => West
        case LeftCorner if cart.direction == South => East
        case LeftCorner if cart.direction == East => South
        case LeftCorner if cart.direction == West => North

        case Crossroads => cart.direction.goTo(cart.nextMove)
      }
      val nextMove = if (nextTrack == Crossroads) cart.nextMove.next() else cart.nextMove
      val crashed = otherCarts.map(_.pos).contains(nextPosition)
      Cart(nextDirection, nextPosition, nextMove, crashed)
  }

  def runUntilFirstCrashed(grid: Vector[Vector[Track]])(carts: List[Cart]): Cart = {
    val crashedCar = carts.find(_.crashed)
    crashedCar match {
      case Some(cart) => cart
      case None => runUntilFirstCrashed(grid)(moveCarts(grid, removeCrashedCarts = false)(carts))
    }
  }

  def runUntilOnlyOneLeft(grid: Vector[Vector[Track]])(carts: List[Cart]): Cart = {
    if(carts.size == 1) {
      carts.head
    } else {
      runUntilOnlyOneLeft(grid)(moveCarts(grid, removeCrashedCarts = true)(carts))
    }
  }

  val input = Source.fromResource("Day13-Input.txt").getLines().toVector
  val (grid, carts) = State.parse(input)

  val part1 = runUntilFirstCrashed(grid)(carts)
  val part2 = runUntilOnlyOneLeft(grid)(carts)
  println(part1.pos) // (46,18)
  println(part2.pos) // (124,103)

}