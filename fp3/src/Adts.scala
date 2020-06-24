import scala.util.{Failure, Success, Try}
  object Adts extends App {
    // a) Дан List[Int], верните элемент с индексом n
    def GetNth(list: List[Int], n: Int): Option[Int] =Option(list(n))
    // примените функцию из пункта (a) здесь, не изменяйте сигнатуру
    def testGetNth(list: List[Int], n: Int): Option[Int] = GetNth(list,n)

    // b) Напишите функцию, увеличивающую число в два раза.
    def Double(n: Option[Int]): Option[Int] = n  match {
      case Some(a)=> Option(a*2)
      case None=>n
    }
    // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
    def testDouble(n: Option[Int]): Option[Int] = Double(n)

    // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").
    def IsEven(n: Int): Either[String, Int] = Either.cond(n % 2 == 0, n, "Нечетное число.") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
    // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
    def testIsEven(n: Int): Either[String, Int] = IsEven(n)

    // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").
    def SafeDivide(a: Int, b: Int): Either[String, Int] = Try(a/b) match {
      case Success(a) => Right(a)
      case Failure(error) => Left("Вы не можете делить на ноль.")
    }
    // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
    def testSafeDivide(a: Int, b: Int): Either[String, Int] = SafeDivide(a,b)

    // e) Обработайте исключения функции с побочным эффектом вернув 0.

    def GoodOldJava(impure: String => Int, str: String): Try[Int] = Try(impure(str)).toEither match {
      case Left(i) => Success(0)
      case Right(s) => Success(s)
    }

    // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
    def testGoodOldJava(impure: String => Int, str: String): Try[Int] = GoodOldJava(impure,str)


}
