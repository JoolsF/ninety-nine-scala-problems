package ninetynineproblems

import scala.collection.generic.SeqFactory

//http://aperiodic.net/phil/scala/s-99/
object OneToTen {

  /*
   * P01 (*) Find the last element of a list.
   * Example:
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   */
  def last[A](l: Seq[A]): A = l match {
    case Nil => throw new Error("")
    case (hd :: Nil) => hd
    case (_ :: tl) => last(tl)
  }

  /*
   * P02 (*) Find the last but one element of a list
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](l: Seq[A]): A = l match {
    case (p :: _ :: Nil) => p
    case (_ :: tl) => penultimate(tl)
    case _ => throw new Error("")
  }

  /*
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  def nth[A](n: Int, l: Seq[A]): A = (n, l) match {
    case (_, Nil) => throw new Error("")
    case (0, hd :: _) => hd
    case (x, hd :: tl) => nth(x - 1, tl)
  }

  /*
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def seqLength[A](l: Seq[A]): Int = l match {
    case Nil => 0
    case (hd :: tl) => 1 + seqLength(tl)
  }

  /*
   *P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](l: Seq[A]): Seq[A] = l match {
    case Nil => Nil
    case hd :: tl => reverse(tl) :+ hd
  }

  /*
   * P06 (*) Find out whether a list is a palindrome.
   * Example:
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[A](l: Seq[A]): Boolean =
    l == reverse(l)

  /*
   * P07 (**) Flatten a nested list structure.
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten[A](l: Seq[A]): Seq[A] = l match {
    case Nil => Nil
    case (hd: Seq[A]) :: (tl: Seq[A]) => flatten(hd) ++ flatten(tl)
    case hd :: tl => hd +: flatten(tl)

  }

  /*
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element.
   * The order of the elements should not be changed.
   * Example:
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  // Fold right would be good here too
  def compress[A](l: Seq[A]): Seq[A] = {
    def compress[A](l2: Seq[A], acc: Seq[A]): Seq[A] =
    l2 match {
      case Nil => acc
      case hd :: Nil => acc
      case hd :: tl => if(hd == last(acc)) compress(tl, acc) else compress(tl, acc :+ hd)

    }
    if(l.isEmpty) l
    else compress(l.tail, Seq(l.head))
  }
}
