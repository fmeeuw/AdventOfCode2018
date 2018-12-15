import scala.annotation.tailrec
object Day14 extends App {

  @tailrec
  def combineRecipesUntil[A](terminate: Vector[Int] => Option[A], previousRecipes: Int = 0)(recipes: Vector[Int], firstIdx: Int, secondIdx: Int): A = {
//    println(s"Current board: $recipeBoard , firstElfIdx = $firstElfIdx, secondElfIdx = $secondElfIdx")
    terminate(recipes) match {
      case Some(output) => output
      case None =>
        val firstRecipe = recipes(firstIdx)
        val secondRecipe = recipes(secondIdx)
        val newRecipeBoard = {
          val sum = firstRecipe + secondRecipe
          val recipesToAdd = if (sum >= 10) Vector(1, sum % 10) else Vector(sum)
          recipes ++ recipesToAdd
        }
        val newFirstIdx = (firstIdx + 1 + firstRecipe) % newRecipeBoard.size
        val newSecondIdx = (secondIdx + 1 + secondRecipe) % newRecipeBoard.size
        combineRecipesUntil(terminate)(newRecipeBoard, newFirstIdx, newSecondIdx)
    }
  }

  def tenRecipesAfter(nr: Int): Vector[Int] => Option[String] = recipes => if(recipes.size >= nr + 10) Some(recipes.drop(nr).take(10).mkString) else None

  def recipesBefore(seq: Vector[Int]): Vector[Int] => Option[Int] = recipes => {
    if(recipes.takeRight(seq.size) == seq) Some(recipes.size - seq.size)
    else if (recipes.dropRight(1).takeRight(seq.size) == seq) Some(recipes.size - seq.size - 1)
    else None
  }

  def part1(nr: Int): String = combineRecipesUntil(tenRecipesAfter(nr))(recipes = Vector(3,7), firstIdx = 0, secondIdx = 1)

  def part2(seq: Vector[Int]): Int = combineRecipesUntil(recipesBefore(seq))(recipes = Vector(3,7), firstIdx = 0, secondIdx = 1)

  println("Tests part 1:")
  println(part1(9))
  println(part1(5))
  println(part1(18))
  println(part1(2018))
  println

  println("part 1:")
  println(part1(84601)) //2688510125
  println

  println("Tests part 2")
  println(part2(Vector(5,1,5,8,9)))
  println(part2(Vector(0,1,2,4,5)))
  println(part2(Vector(9,2,5,1,0)))
  println(part2(Vector(5,9,4,1,4)))
  println

  println("part 2:")
  println(part2(Vector(0,8,4,6,0,1))) //20188250
  println
}
