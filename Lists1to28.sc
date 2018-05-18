/* Ninety-nine Scala Problems:
   Working with lists */

/* P01 (*) Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
*/

val myList = List(1, 1, 2, 3, 5, 8)

// S01: Build-in method
val myLastItem1 = myList.last

// S02: Own implementation of last
def getLast[A](list: List[A]): A = list match {
  case Nil => throw new NoSuchElementException
  case head :: Nil => head
  case head :: tail => getLast(tail)
}

val myLastItem2 = getLast(myList)