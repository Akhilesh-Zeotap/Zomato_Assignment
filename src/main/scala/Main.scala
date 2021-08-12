import Restaurant.restaurants

object Main extends App {

  def topRestaurantByRating(n: Int): List[Int] = {
    if (n <= 0) throw new IllegalArgumentException("Negative number")
    val ratings = restaurants.map(x => (x._1, x._2.rating)).toList.sortBy(t => -t._2)
    /*
    First we are mapping restaurants to contain only id and ratings.
    Then we are sorting the list of tuples by descending order of 2nd element.
    */
    if (ratings.size < n) throw new IllegalArgumentException(s"Only ${ratings.map(x => x._1)} restaurants meets the requirement")
    else ratings.map(x => x._1).slice(0, n)
  }


  def topRestaurantByLocation(location: String, restaurantType: List[String], n: Int): List[Int] = {
    if (n <= 0) throw new IllegalArgumentException("Negative number")

    val filtered = restaurants.filter(x =>
      x._2.location == location && x._2.restaurantType == restaurantType)
    /*
    Filter out the restaurant which does not belong to given location or does not have given type
    */
    if (filtered.size <= 0) throw new IllegalArgumentException("No restaurants satisfying the criteria")

    val sortedRatings = filtered.map(x => (x._2.rating, x._1)).toList.sortBy(t => -t._1) // It will create List(restaurantId , rating).

    if (sortedRatings.size < n) throw new IllegalArgumentException(s"Only ${sortedRatings.map(x => x._2)} restaurants meets the requirement")
    else sortedRatings.map(x => x._2).slice(0, n) // return List of restaurant Id's
  }


  def topRestaurantByVote(location: String, n: Int): List[Int] = {
    if (n <= 0) throw new IllegalArgumentException("Negative number")
    val filtered = restaurants.filter(x => x._2.location == location && x._2.numberOfVotes != 0)
    /*
    Filter out the restaurant which does not belong to given location or has not any votes
     */
    if (filtered.size <= 0) throw new IllegalArgumentException("No restaurants satisfying the criteria")

    val ratings = filtered.map(x => (x._2.numberOfVotes, x._2.rating, x._1)).toList // Create list of tuples(votes,rating,id)
    val sortedRatings = ratings.sortBy(t => (t._1, -t._2)) // first sort by votes in ascending order if equal then sort by rating in descending order
    sortedRatings.map(x => x._3).slice(0, n) // return List of restaurant Id's
  }


  def nofOfDishesLiked(): Map[Int, Int] = {
    restaurants.map(x => x._1 -> x._2.dishesLiked.length) // Function will return Map(restaurantId -> dishesLiked.length)
    // we can use this to find dishes liked in any restaurant by querying it's ID
  }


  def noOfDistinctLocations(): List[String] = {
    restaurants.map(x => x._2.location).toList.distinct // Will return list of Distinct Location present in dataset
  }


  def noOfCuisinesByLocation(location: String) = {
    noOfCuisines()(location)
  }


  def noOfCuisines(): Map[String, Int] = {
    restaurants.foldLeft(Map[String, Set[String]]())((cuisines, restaurant) => {
      val location = restaurant._2.location
      val listOfCuisines = restaurant._2.typesOfCuisines.toSet
      cuisines.+(location -> listOfCuisines.++(cuisines.getOrElse(location, Set())))
    }).map(x => (x._1, x._2.size))
    // Will return Map(Location -> cuisines in that location), we can query any location with this
  }


  def restaurantCountForCuisine() = {
    restaurants.foldLeft(Map[String, Int]())((cnt,restaurant) => {
      val listOfCuisines = restaurant._2.typesOfCuisines.toSet
      cnt ++ listOfCuisines.map(x => x->1).map{ case (k,v) => k -> (v + cnt.getOrElse(k,0)) }
    })
  }
}
