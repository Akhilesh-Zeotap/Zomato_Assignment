import Restaurant.restaurants

object Main extends App {

  def nofOfDishesLiked(): Map[Int, Int] = {
    restaurants.map(x => x._1 -> x._2.dishesLiked.length) // Function will return Map(restaurantId -> dishesLiked.length)
    // we can use this to find dishes liked in any restaurant by querying it's ID
  }


  def noOfCuisines(): scala.collection.mutable.Map[String, Int] = {
    val cuisines: scala.collection.mutable.Map[String, Set[String]] = scala.collection.mutable.Map()
    // cuisines is Map(Location -> cuisines in that location)

    for (restaurant <- restaurants) { // iterating through all restaurants
      val location = restaurant._2.location // store the location of current restaurant
      val listOfCuisines = restaurant._2.typesOfCuisines.toSet // store type of cuisines in current restaurant

      if (cuisines.contains(location))
        for (c <- listOfCuisines) cuisines(location) += c // iterating through all the cuisines in current restaurant and adding it to set
      else
        cuisines += (location -> listOfCuisines)
    }
    cuisines.map(x => (x._1, x._2.size)) // Map set of cuisines to it's size as per requirement
    // Will return Map(Location -> cuisines in that location), we can query any location with this
  }


  def noOfDistinctLocations(): List[String] = {
    restaurants.map(x => x._2.location).toList.distinct // Will return list of Distinct Location present in dataset
  }


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

    val sortedRatings = filtered.map(x => (x._1, x._2.rating)).toList.sortBy(t => -t._2) // It will create List(restaurantId , rating).

    if (sortedRatings.size < n) throw new IllegalArgumentException(s"Only ${sortedRatings.map(x => x._1)} restaurants meets the requirement")
    else sortedRatings.map(x => x._1).slice(0, n) // return List of restaurant Id's
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
}
