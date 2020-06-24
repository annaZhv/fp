package exersise1
/** Напишите решение в виде функции.
 *
 * Синтаксис:
 *   val a: Int = ???
 *
 *   a match {
 *     case 0 => true
 *     case _ => false
 *   }
 */
object PatternMatching extends App {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal

  // a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
  val IntToString = (a: Int) => {
    var str = ""
    if (a == 1) {
      str = "it is one"
      str
    }
    else if (a == 2) {
      str = "it is two"
      str
    }
    else if (a == 3) {
      str = "it is three"
      str
    }
    else {
      str = "what is that"
      str
    }
  }
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = IntToString(value)

  // b) Напишите функцию которая возвращает true если переменная `value` принимает значение:"max" или "Max", "moritz" или "Moritz"
   val IsMaxAndMoritz = (a: String) => {
    if (a == "max" || a == "Max" || a == "moritz" || a == "Moritz") {
      true
    }
    else false
  }
  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = IsMaxAndMoritz(value)

  // c) Напишите функцию проверки является ли `value` четным
  val IsEven = (a: Int) => {
    if (a % 2 == 0)
      true
    else false
  }
  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = IsEven(value)



  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */
  val RPS = (a: Hand, b: Hand) => {
    a match {
      case Rock => {
        b match {
          case Rock => Draw
          case Paper => Lose
          case Scissor => Lose
        }
      }
      case Paper => {
        b match {
          case Rock => Win
          case Paper => Draw
          case Scissor => Lose
        }
      }
      case Scissor => {
        b match {
          case Rock => Lose
          case Paper => Win
          case Scissor => Draw
        }
      }
    }
  }

  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = RPS(a,b)



  // Примечание: используйте определение Animals

  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.

  def Weight(animal: Animal): Int =
    animal match {
      case mammal : Mammal => mammal.weight
      case _ => -1
    }

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = Weight(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
  def updateFood(animal: Animal): Animal =
    animal match {
      case fish: Fish =>  fish.food = Plants; fish
      case bird: Bird => bird.food= Plants; bird
      case _ => animal
    }



  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = updateFood(animal)

}
