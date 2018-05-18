/* Ninety-nine Scala Problems:
   Working with lists */

/* P01 (*) Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
*/

val myList = List(1, 1, 2, 3, 5, 8)

// S01: Built-in method
val myLastItem1 = myList.last

// S02: Own implementation of last
def getLast[A](list: List[A]): A = list match {
  case Nil => throw new NoSuchElementException
  case head :: Nil => head
  case head :: tail => getLast(tail)
}

val myLastItem2 = getLast(myList)

/* P02 (*) Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
*/

// S01: Built-in solution
val myPenultimate1 = myList.init.last

// S02: Own implementation to get penultimate element
def getPenultimate[A](list: List[A]): A = list match {
  case Nil => throw new UnsupportedOperationException
  case head :: Nil => throw new NoSuchElementException
  case head :: last :: Nil => head
  case head :: tail => getPenultimate(tail)
}

val myPenultimate2 = getPenultimate(myList)

/* P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:
    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
*/

// S01: Built-in method
val n = 4
val myNth1 = myList(n)

// S02: Own implementation to get Nth element of a list
def getNth[A](list: List[A], index: Int): A = (list, index) match {
  case (Nil, _) => throw new IndexOutOfBoundsException
  case (head :: _, 0) => head
  case (_ :: tail, _) => getNth(list = tail, index - 1)
}

val myNth2 = getNth(myList, n)

