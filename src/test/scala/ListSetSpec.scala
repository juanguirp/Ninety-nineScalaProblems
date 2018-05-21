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
  val normalList = List(1, 1, 2, 3, 5, 8)
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
    assertThrows[NoSuchElementException]{
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
    assertThrows[IndexOutOfBoundsException]{
      troubleshooter.getNth(emptyList, POSITION4)
    }
  }

  it should "throw IndexOutBoundsException if passed position is bigger than list's length" in {
    assertThrows[IndexOutOfBoundsException]{
      troubleshooter.getNth(oneElementList, POSITION4)
    }
    assertThrows[IndexOutOfBoundsException]{
      troubleshooter.getNth(twoElementsList, POSITION4)
    }
    assertThrows[IndexOutOfBoundsException]{
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
}
