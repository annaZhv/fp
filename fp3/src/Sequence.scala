import scala.annotation.tailrec
object Sequence extends App {
  // a) Найдите последний элемент Seq.
     def LastElement[A](seq: Seq[A]): Option[A] = {
    seq match {
      case last +: Nil  => Option(last)
      case head +: tail => LastElement(tail)
    }
  }
  def testLastElement[A](seq: Seq[A]): Option[A] =  seq match {
    case last +: Nil  => Option(last)
    case head +: tail => LastElement (tail)
  }

  // b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.

  def Zip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    def loop[A](a: Seq[A], b: Seq[A], c: Seq[(A, A)]): Seq[(A, A)] = {
      a match {
        case ahead +: atail => b match {
          case blast +: Nil  => c:+(ahead,blast)
          case bhead +: btail => loop(atail,btail,c:+(ahead,bhead))
        }
        case Nil  => c
      }
    }
    loop(a,b,Nil)
  }
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = Zip(a,b)

  // c) Проверьте, выполняется ли условие для всех элементов в Seq.

  def ForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    def loop[A](seq: Seq[A],flag: Boolean)(cond: A => Boolean): Boolean = {
      seq match {
        case head :: tail => loop(tail,flag && cond(head))(cond)
        case Nil => flag
      }
    }
    loop(seq,true)(cond)
  }
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = ForAll(seq)(cond)

  /* d) Проверьте, является ли Seq палиндромом
   *
   */
  def Palindrom[A](seq: Seq[A]): Boolean = {
    def loop[A](sseq: Seq[A], aseq: Seq[A]): Boolean = {
      sseq match {
        case head :: tail => loop(tail,aseq = head+:aseq)
        case Nil => seq.equals(aseq)
      }
    }
    loop(seq,Nil)
  }
  def testPalindrom[A](seq: Seq[A]): Boolean = Palindrom(seq)

  /* e) Реализуйте flatMap используя foldLeft.
   *
   */
  def FlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft[Seq[B]](Nil)((acc, next) => f(next).++:(acc))
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = FlatMap(seq)(f)
}
