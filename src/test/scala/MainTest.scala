import org.scalatest.FunSuite
class MainTest extends FunSuite {

  test("No of dishes liked in restaurant with id 10") {
    assert(Main.nofOfDishesLiked()(10) === 7)
  }

  test("Number of distinct cuisines at Basavanagudi") {
    assert(Main.noOfCuisines()("Basavanagudi") === 12)
  }
}
