import Initialize.initialize
object Restaurant {
  case class Restaurant(restaurantName:String,
                        rating:Double,
                        numberOfVotes:Int,
                        location:String,
                        restaurantType:List[String],
                        dishesLiked:List[String],
                        typesOfCuisines:List[String],
                        costForTwo:Double)

  val restaurants:Map[Int,Restaurant] = initialize()
}
