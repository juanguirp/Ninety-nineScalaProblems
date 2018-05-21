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

}
