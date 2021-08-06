import org.scalatest.FunSuite
class MainTest extends FunSuite {

  test("No of dishes liked in restaurant with id 10") {
    assert(Main.nofOfDishesLiked()(10) === 7)
  }

  test("Number of distinct cuisines at Basavanagudi") {
    assert(Main.noOfCuisines()("Basavanagudi") === 87)
  }


  test("Top negative restaurants by rating") {
    assertThrows[IllegalArgumentException](Main.topRestaurantByRating(-10) )
  }

  test("If queried number grater than actual result") {
    assertThrows[IllegalArgumentException](Main.topRestaurantByRating(10000000))
  }

  test("Top negative restaurants by rating in given location") {
    assertThrows[IllegalArgumentException](Main.topRestaurantByLocation("Basavanagudi",List("North Indian", "Rajasthani"),-10))
  }

  test("Top restaurants by rating in given location but queried number is large"){
    assertThrows[IllegalArgumentException](Main.topRestaurantByLocation("Basavanagudi",List("North Indian", "Rajasthani"),100000))
  }
}

