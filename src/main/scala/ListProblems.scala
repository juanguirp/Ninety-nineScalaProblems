/* Ninety-nine Scala Problems: */

/* 1. Working with lists. */

class ListProblems {

  // Constants.
  val ZERO = 0
  val ONE = 1

  // --------------------------------------------------
  /* P01 (*) Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
*/

  // S01: Built-in method.
  // val myLastItem1 = myList.last

  // S02: Own implementation of last.
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => head
    case head :: tail => getLast(tail)
  }

  // --------------------------------------------------
  /* P02 (*) Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
*/

  // S01: Built-in solution.
  // val myPenultimate1 = myList.init.last

  // S02: Own implementation to get penultimate element.
  def getPenultimate[A](list: List[A]): A = list match {
    case Nil => throw new UnsupportedOperationException
    case head :: Nil => throw new NoSuchElementException
    case head :: last :: Nil => head
    case head :: tail => getPenultimate(tail)
  }

  // --------------------------------------------------
  /* P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:
    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
*/

  // S01: Built-in method.
  // val n = 4
  // val myNth1 = myList(n)

  // S02: Own implementation to get Nth element of a list.
  def getNth[A](list: List[A], index: Int): A = (list, index) match {
    case (Nil, _) => throw new IndexOutOfBoundsException
    case (head :: _, ZERO) => head
    case (_ :: tail, _) => getNth(list = tail, index - ONE)
  }

  // --------------------------------------------------
  /* P04 (*) Find the number of elements of a list.
    Example:
    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
*/

  // S01: Built-in method.
  // val myLength1 = myList.length

  // S02: Own implementation to get the number of elements inside the list.
  // (Yes, I love tail recursion <3).
  private def getRealLength[A](list: List[A], initLength: Int): Int = list match {
    case Nil => initLength
    case head :: tail => getRealLength(tail, initLength + ONE)
  }

  // Wrapper method. Other option: Nested/Local function.
  def getLength[A](list: List[A]): Int = getRealLength(list, ZERO)

  // --------------------------------------------------
  /* P05 (*) Reverse a list.
    Example:
    scala> reverse(List(1, 1, 2, 3, 5, 8))
    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/

  // S01: Built-in method.
  // val myReversedList1 = myList.reverse

  // S02: Own implementation to reverse a list.
  def getReverse[A](list: List[A]): List[A] = {

    // Nested/Local function that gets reversed list using tail recursion.
    def getRealReversedList[A](finalList: List[A], realList: List[A]): List[A] = realList match {
      case Nil => finalList
      case head :: tail => getRealReversedList(head :: finalList, tail)
    }
    getRealReversedList(Nil, list)
  }


}
