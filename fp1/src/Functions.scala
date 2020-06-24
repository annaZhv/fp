/** Напишите отдельные функции, решающие поставленную задачу.
 *
 * Синтаксис:
 *   // метод
 *   def myFunction(param0: Int, param1: String): Double = // тело
 *
 *   // значение
 *   val myFunction: (Int, String) => Double (param0, param1) => // тело
 */
object Functions extends App {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  val Circle=(r: Double) => r*r*Math.PI
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = Circle(r)

  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def RectangleCurried(a:Double)(b:Double)=a*b
  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = RectangleCurried(a)(b)
  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  val Rectangle=(c: Double,d:Double) => c*d
  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = Rectangle(a,b)

  println("testCircle: \t"+testCircle(3))
  println("testRectangleCurried: \t"+testRectangleCurried(5,7))
  println("testRectangleUc: \t"+testRectangleUc(5,7))
}
