package exersise1

sealed trait Food

case object Meat extends Food
case object Vegetables extends Food
case object Plants extends Food

/** Переопределение класа Animal как trait **/
sealed trait Animal {
  val name: String
  val food: Food
  def eats(food: Food): Boolean = this.food.equals(food)
}

case class Mammals(name: String, food: Food) extends Animal
case class Birds(name: String, food: Food) extends Animal
case class Fish(name: String, food: Food) extends Animal

/** Объект-компаньон для класса Animal **/
object Animal {
  val cat = Mammals("cat", Meat)
  val parrot = Birds("parrot", Vegetables)
  val goldfish = Fish("goldfish", Plants)

  def knownAnimal(name: String): Boolean =
    name.equals(cat.name) || name.equals(parrot.name) ||
      name.equals(goldfish.name)

  def apply(name: String): Option[Animal] = {
    name match {
      case cat.name => Some(cat)
      case parrot.name => Some(parrot)
      case goldfish.name => Some(goldfish)
      case _ => None
    }
  }
}

object program extends App {
  println("Animal(\"cat\").get.eats(Plants): " + Animal("cat").get.eats(Plants))
}

