import org.scalatest.{FlatSpec, GivenWhenThen, fixture}

class ListSetSpec extends fixture.FlatSpec with GivenWhenThen {

  case class FixtureParam(solution: ListProblems,
                          emptyList: List[_],
                          oneElementList: List[_],
                          twoElementsList: List[_],
                          fourDuplicateElementsList: List[_],
                          normalList: List[_])

  def withFixture(test: OneArgTest) = {
    // Setup, objects used in many/all test
    val emptyList = List()
    val oneElementList = List(77)
    val twoElementsList = List(6, 10)
    val fourDuplicateElementsList = List(8, 8, 8, 8)
    val normalList = List(1, 1, 2, 3, 5, 8)
    val troubleshooter = new ListProblems()
    val fixtures = FixtureParam(troubleshooter,
      emptyList,
      oneElementList,
      twoElementsList,
      fourDuplicateElementsList,
      normalList)
    // Test
    try {
      info("Starting test - " + test.name)
      super.withFixture(test.toNoArgTest(fixtures))
    }
    // Cleanup
    finally {
      info("Finished test - " + test.name)
    }
  }

  "S01: getLast method" should "throw NoSuchElementException if an empty list is passed as parameter" in { f =>
    assertThrows[NoSuchElementException] {
      f.solution.getLast(f.emptyList)
    }
  }

  it should "return one value if list has just one element" in { f =>
    assert(f.solution.getLast(f.oneElementList) === 77)
    // Using built-in solution:
    assert(f.solution.getLast(f.oneElementList) === f.oneElementList.last)
  }

  it should "return the last element of a list" in { f =>
    assert(f.solution.getLast(f.normalList) === 8)
    // Using built-in solution:
    assert(f.solution.getLast(f.normalList) === f.normalList.last)
  }

  "S02: getPenultimate method" should "throw UnsupportedOperationException if an empty list is passed as parameter" in { f =>
    assertThrows[UnsupportedOperationException] {
      f.solution.getPenultimate(f.emptyList)
    }
  }

  it should "throw NoSuchElementException if list has just one element" in { f =>
    assertThrows[NoSuchElementException] {
      f.solution.getPenultimate(f.oneElementList)
    }
  }

  it should "return the first element if list has two elements" in { f =>
    assert(f.solution.getPenultimate(f.twoElementsList) === 6)
    // Using built-in solution:
    assert(f.solution.getPenultimate(f.twoElementsList) === f.twoElementsList.init.last)
  }

  it should "return the penultimate element of a list" in { f =>
    assert(f.solution.getPenultimate(f.normalList) === 5)
    // Using built-in solution:
    assert(f.solution.getPenultimate(f.normalList) === f.normalList.init.last)
  }

  "S03: getNth method" should "throw IndexOutBoundsException if an empty list is passed as parameter" in { f =>
    assertThrows[IndexOutOfBoundsException] {
      f.solution.getNth(f.emptyList, 10)
    }
  }

  it should "throw IndexOutBoundsException if passed position is bigger than list's length" in { f =>
    assertThrows[IndexOutOfBoundsException] {
      f.solution.getNth(f.oneElementList, 10)
    }
    assertThrows[IndexOutOfBoundsException] {
      f.solution.getNth(f.twoElementsList, 10)
    }
    assertThrows[IndexOutOfBoundsException] {
      f.solution.getNth(f.normalList, 99)
    }
  }

  it should "return the first element if passed position is zero" in { f =>
    assert(f.solution.getNth(f.oneElementList, 0) === 77)
    assert(f.solution.getNth(f.twoElementsList, 0) === 6)
    assert(f.solution.getNth(f.normalList, 0) === 1)
    // Using built-in solution:
    assert(f.solution.getNth(f.oneElementList, 0) === f.oneElementList(0))
    assert(f.solution.getNth(f.twoElementsList, 0) === f.twoElementsList(0))
    assert(f.solution.getNth(f.normalList, 0) === f.normalList(0))
  }

  it should "return the element in passed position" in { f =>
    assert(f.solution.getNth(f.normalList, 4) === 5)
    // Using built-in solution:
    assert(f.solution.getNth(f.normalList, 4) === f.normalList(4))
  }

  "S04: getLength method" should "return zero if list is empty" in { f =>
    assert(f.solution.getLength(f.emptyList) === 0)
    // Using built-in solution:
    assert(f.solution.getLength(f.emptyList) === f.emptyList.length)
  }

  it should "return the number of elements of a list" in { f =>
    assert(f.solution.getLength(f.oneElementList) === 1)
    assert(f.solution.getLength(f.twoElementsList) === 2)
    assert(f.solution.getLength(f.normalList) === 6)
    // Using built-in solution:
    assert(f.solution.getLength(f.oneElementList) === f.oneElementList.length)
    assert(f.solution.getLength(f.twoElementsList) === f.twoElementsList.length)
    assert(f.solution.getLength(f.normalList) === f.normalList.length)
  }

  "S05: getReverse method" should "return same passed list if it is empty" in { f =>
    assert(f.solution.getReverse(f.emptyList) === f.emptyList)
    // Using built-in solution:
    assert(f.solution.getReverse(f.emptyList) === f.emptyList.reverse)
  }

  it should "return reversed list" in { f =>
    assert(f.solution.getReverse(f.oneElementList) === List(77))
    assert(f.solution.getReverse(f.twoElementsList) === List(10, 6))
    assert(f.solution.getReverse(f.normalList) === List(8, 5, 3, 2, 1, 1))
    // Using built-in solution:
    assert(f.solution.getReverse(f.oneElementList) === f.oneElementList.reverse)
    assert(f.solution.getReverse(f.twoElementsList) === f.twoElementsList.reverse)
    assert(f.solution.getReverse(f.normalList) === f.normalList.reverse)
  }

  "S06-A: isPalindrome method" should "return true if passed list is empty" in { f =>
    assert(f.solution.isPalindrome(f.emptyList))
  }

  it should "return true if passed list has just one element" in { f =>
    assert(f.solution.isPalindrome(f.oneElementList))
  }

  it should "return true if passed list is a palindrome" in { f =>
    Given("a palindrome")
    val impairPalindromeList = List(1, 2, 3, 2, 1)
    val pairPalindromeList = List(1, 2, 2, 1)

    When("it is verified")
    val isImpairPalindrome = f.solution.isPalindrome(impairPalindromeList)
    val isPairPalindrome = f.solution.isPalindrome(pairPalindromeList)

    Then("the result is true")
    assert(isImpairPalindrome)
    assert(isPairPalindrome)
  }

  it should "return false if passed list is not a palindrome" in { f =>
    Given("a non palindrome list")
    val nonPalindromeList = List(1, 2, 3, 2, 4)

    When("it is verified")
    val isNoPalindrome = f.solution.isPalindrome(nonPalindromeList)

    Then("the result is false")
    assert(!isNoPalindrome)
  }

  "S06-B: isPalindromeSimple method" should "return true if passed list is empty" in { f =>
    assert(f.solution.isPalindromeSimple(f.emptyList))
  }

  it should "return true if passed list has just one element" in { f =>
    assert(f.solution.isPalindromeSimple(f.oneElementList))
  }

  it should "return true if passed list is a palindrome" in { f =>
    Given("a palindrome")
    val impairPalindromeList = List(1, 2, 3, 2, 1)
    val pairPalindromeList = List(1, 2, 2, 1)

    When("it is verified")
    val isImpairPalindrome = f.solution.isPalindromeSimple(impairPalindromeList)
    val isPairPalindrome = f.solution.isPalindromeSimple(pairPalindromeList)

    Then("the result is true")
    assert(isImpairPalindrome)
    assert(isPairPalindrome)
  }

  it should "return false if passed list is not a palindrome" in { f =>
    Given("a non palindrome list")
    val nonPalindromeList = List(1, 2, 3, 2, 4)

    When("it is verified")
    val isNoPalindrome = f.solution.isPalindromeSimple(nonPalindromeList)

    Then("the result is false")
    assert(!isNoPalindrome)
  }

  "S07: flatList method" should "return an empty list if passed list has only an empty list" in { f =>
    assert(f.solution.flatList(List(f.emptyList)) === f.emptyList)
    // Using built-in solution:
    assert(f.solution.flatList(List(f.emptyList)) === List(f.emptyList).flatten)
  }

  it should "return flatten list (not sorted)" in { f =>
    Given("a nested list")
    val listOfList = List(List(1, 2), List(3, 4, 5), List(6, 7, 8, 9), List(10))

    When("it is flattened")
    val flattenedList1 = f.solution.flatList(listOfList).sortWith(_ < _)
    val flattenedList2 = f.solution.flatList(listOfList).sortWith(_ < _)

    Then("the result is a list with all elements of nested list")
    assert(flattenedList1 === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).sortWith(_ < _))
    // Using built-in solution:
    assert(flattenedList2 === listOfList.flatten.sortWith(_ < _)
    )
  }

  "S08: compressList method" should "return the same list if it is empty" in { f =>
    assert(f.solution.compressList(f.emptyList) === f.emptyList)
    assert(f.solution.compressListTailRecursion(f.emptyList) === f.emptyList)
  }

  it should "return the same list if there is not consecutive duplicates in it" in { f =>
    assert(f.solution.compressList(f.twoElementsList) === f.twoElementsList)
    assert(f.solution.compressListTailRecursion(f.twoElementsList) === f.twoElementsList)
  }

  it should "return a list with not consecutive duplicates elements" in { f =>
    Given("a list with duplicated elements")
    val duplicateElementsList = List(1, 2, 2, 1)

    When("it is compressed")
    val finalCompresedList1 = f.solution.compressList(duplicateElementsList)
    val finalCompresedList2 = f.solution.compressListTailRecursion(duplicateElementsList)

    Then("the result is a list with not consecutive duplicates elements")
    assert(finalCompresedList1 === List(1, 2, 1))
    assert(f.solution.compressList(f.fourDuplicateElementsList) === List(8))
    assert(f.solution.compressList(f.normalList) === List(1, 2, 3, 5, 8))
    // Tail recursive version.
    assert(finalCompresedList2 === List(1, 2, 1))
    assert(f.solution.compressListTailRecursion(f.fourDuplicateElementsList) === List(8))
    assert(f.solution.compressListTailRecursion(f.normalList) === List(1, 2, 3, 5, 8))
  }

  "S09: packList method" should "return a list with an empty list if  passed list is empty" in { f =>
    assert(f.solution.packList(f.emptyList) === List(f.emptyList))
  }

  it should "return a list with the same list if all items are the same in value" in { f =>
    assert(f.solution.packList(f.fourDuplicateElementsList) === List(f.fourDuplicateElementsList))
  }

  it should "return a list for each consecutive duplicates elements in list" in { f =>
    Given("a list with consecutive duplicates elements")
    val duplicateElementsList = List(1, 2, 2, 1)

    When("it is packed")
    val packedList = f.solution.packList(duplicateElementsList)

    Then("the result is a list for each consecutive duplicates elements in list")
    assert(packedList === List(List(1), List(2, 2), List(1)))
    assert(f.solution.packList(f.normalList) === List(List(1, 1), List(2), List(3), List(5), List(8)))
  }

}
