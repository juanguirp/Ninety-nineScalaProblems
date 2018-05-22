import org.scalatest.FlatSpec

class ListSetSpec extends FlatSpec {

  //info("Starting tests... OK!")
  //info("Creating necessary objects to test...")
  val ZERO, POSITION0 = 0
  val POSITION1 = 1
  val POSITION4 = 4
  val POSITION99 = 99
  val emptyList = List()
  val oneElementList = List(77)
  val twoElementsList = List(6, 10)
  val fourDuplicateElementsList = List(8, 8, 8, 8)
  val normalList = List(1, 1, 2, 3, 5, 8)
  val nonPalindromeList = List(1, 2, 3, 2, 4)
  val impairPalindromeList = List(1, 2, 3, 2, 1)
  val pairPalindromeList = List(1, 2, 2, 1)
  val listOfList = List(List(1, 2), List(3, 4, 5), List(6, 7, 8, 9), List(10))
  val troubleshooter = new ListProblems()
  //info("... OK!")

  "S01: getLast method" should "throw NoSuchElementException if an empty list is passed as parameter" in {
    assertThrows[NoSuchElementException] {
      troubleshooter.getLast(emptyList)
    }
  }

  it should "return one value if list has just one element" in {
    assert(troubleshooter.getLast(oneElementList) === 77)
    // Using built-in solution:
    assert(troubleshooter.getLast(oneElementList) === oneElementList.last)
  }

  it should "return the last element of a list" in {
    assert(troubleshooter.getLast(normalList) === 8)
    // Using built-in solution:
    assert(troubleshooter.getLast(normalList) === normalList.last)
  }

  "S02: getPenultimate method" should "throw UnsupportedOperationException if an empty list is passed as parameter" in {
    assertThrows[UnsupportedOperationException] {
      troubleshooter.getPenultimate(emptyList)
    }
  }

  it should "throw NoSuchElementException if list has just one element" in {
    assertThrows[NoSuchElementException] {
      troubleshooter.getPenultimate(oneElementList)
    }
  }

  it should "return the first element if list has two elements" in {
    assert(troubleshooter.getPenultimate(twoElementsList) === 6)
    // Using built-in solution:
    assert(troubleshooter.getPenultimate(twoElementsList) === twoElementsList.init.last)
  }

  it should "return the penultimate element of a list" in {
    assert(troubleshooter.getPenultimate(normalList) === 5)
    // Using built-in solution:
    assert(troubleshooter.getPenultimate(normalList) === normalList.init.last)
  }

  "S03: getNth method" should "throw IndexOutBoundsException if an empty list is passed as parameter" in {
    assertThrows[IndexOutOfBoundsException] {
      troubleshooter.getNth(emptyList, POSITION4)
    }
  }

  it should "throw IndexOutBoundsException if passed position is bigger than list's length" in {
    assertThrows[IndexOutOfBoundsException] {
      troubleshooter.getNth(oneElementList, POSITION4)
    }
    assertThrows[IndexOutOfBoundsException] {
      troubleshooter.getNth(twoElementsList, POSITION4)
    }
    assertThrows[IndexOutOfBoundsException] {
      troubleshooter.getNth(normalList, POSITION99)
    }
  }

  it should "return the first element if passed position is zero" in {
    assert(troubleshooter.getNth(oneElementList, POSITION0) === 77)
    assert(troubleshooter.getNth(twoElementsList, POSITION0) === 6)
    assert(troubleshooter.getNth(normalList, POSITION0) === 1)
    // Using built-in solution:
    assert(troubleshooter.getNth(oneElementList, POSITION0) === oneElementList(POSITION0))
    assert(troubleshooter.getNth(twoElementsList, POSITION0) === twoElementsList(POSITION0))
    assert(troubleshooter.getNth(normalList, POSITION0) === normalList(POSITION0))
  }

  it should "return the element in passed position" in {
    assert(troubleshooter.getNth(normalList, POSITION4) === 5)
    // Using built-in solution:
    assert(troubleshooter.getNth(normalList, POSITION4) === normalList(POSITION4))
  }

  "S04: getLength method" should "return zero if list is empty" in {
    assert(troubleshooter.getLength(emptyList) === ZERO)
    // Using built-in solution:
    assert(troubleshooter.getLength(emptyList) === emptyList.length)
  }

  it should "return the number of elements of a list" in {
    assert(troubleshooter.getLength(oneElementList) === 1)
    assert(troubleshooter.getLength(twoElementsList) === 2)
    assert(troubleshooter.getLength(normalList) === 6)
    // Using built-in solution:
    assert(troubleshooter.getLength(oneElementList) === oneElementList.length)
    assert(troubleshooter.getLength(twoElementsList) === twoElementsList.length)
    assert(troubleshooter.getLength(normalList) === normalList.length)
  }

  "S05: getReverse method" should "return same passed list if it is empty" in {
    assert(emptyList.isEmpty)
    assert(troubleshooter.getReverse(emptyList) === emptyList)
    // Using built-in solution:
    assert(troubleshooter.getReverse(emptyList) === emptyList.reverse)
  }

  it should "return reversed list" in {
    assert(troubleshooter.getReverse(oneElementList) === List(77))
    assert(troubleshooter.getReverse(twoElementsList) === List(10, 6))
    assert(troubleshooter.getReverse(normalList) === List(8, 5, 3, 2, 1, 1))
    // Using built-in solution:
    assert(troubleshooter.getReverse(oneElementList) === oneElementList.reverse)
    assert(troubleshooter.getReverse(twoElementsList) === twoElementsList.reverse)
    assert(troubleshooter.getReverse(normalList) === normalList.reverse)
  }

  "S06-A: isPalindrome method" should "return true if passed list is empty" in {
    assert(troubleshooter.isPalindrome(emptyList))
  }

  it should "return true if passed list has just one element" in {
    assert(troubleshooter.isPalindrome(oneElementList))
  }

  it should "return true if passed list is a palindrome" in {
    assert(troubleshooter.isPalindrome(impairPalindromeList))
    assert(troubleshooter.isPalindrome(pairPalindromeList))
  }

  it should "return false if passed list is not a palindrome" in {
    assert(!troubleshooter.isPalindrome(nonPalindromeList))
  }

  "S06-B: isPalindromeSimple method" should "return true if passed list is empty" in {
    assert(troubleshooter.isPalindromeSimple(emptyList))
  }

  it should "return true if passed list has just one element" in {
    assert(troubleshooter.isPalindromeSimple(oneElementList))
  }

  it should "return true if passed list is a palindrome" in {
    assert(troubleshooter.isPalindromeSimple(impairPalindromeList))
    assert(troubleshooter.isPalindromeSimple(pairPalindromeList))
  }

  it should "return false if passed list is not a palindrome" in {
    assert(!troubleshooter.isPalindromeSimple(nonPalindromeList))
  }

  "S07: flatList method" should "return the same list if it is empty" in {
    assert(troubleshooter.flatList(emptyList) === emptyList)
    // Using built-in solution:
    assert(troubleshooter.flatList(emptyList) === emptyList.flatten)
  }

  it should "return flatten list (not sorted)" in {
    assert(troubleshooter.flatList(listOfList).sortWith(_ < _) === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).sortWith(_ < _))
    // Using built-in solution:
    assert(troubleshooter.flatList(listOfList).sortWith(_ < _) === listOfList.flatten.sortWith(_ < _))
  }

  "S08: compressList" should "return the same list if it is empty" in {
    assert(troubleshooter.compressList(emptyList) === emptyList)
    assert(troubleshooter.compressListTailRecursion(emptyList) === emptyList)
  }

  it should "return the same list if there is not consecutive duplicates in it" in {
    assert(troubleshooter.compressList(twoElementsList) === twoElementsList)
    assert(troubleshooter.compressListTailRecursion(twoElementsList) === twoElementsList)
  }

  it should "return a list with not consecutive duplicates elements" in {
    assert(troubleshooter.compressList(fourDuplicateElementsList) === List(8))
    assert(troubleshooter.compressList(normalList) === List(1,2,3,5,8))
    assert(troubleshooter.compressList(pairPalindromeList) === List(1,2,1))
    assert(troubleshooter.compressListTailRecursion(fourDuplicateElementsList) === List(8))
    assert(troubleshooter.compressListTailRecursion(normalList) === List(1,2,3,5,8))
    assert(troubleshooter.compressListTailRecursion(pairPalindromeList) === List(1,2,1))
  }

}
