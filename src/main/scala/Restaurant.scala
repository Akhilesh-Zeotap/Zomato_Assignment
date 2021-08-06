import Initialize.initialize

object Restaurant {
  val restaurants: Map[Int, Restaurant] = initialize()

  case class Restaurant(restaurantName: String,
                        rating: Double,
                        numberOfVotes: Int,
                        location: String,
                        restaurantType: List[String],
                        dishesLiked: List[String],
                        typesOfCuisines: List[String],
                        costForTwo: Double)
}
