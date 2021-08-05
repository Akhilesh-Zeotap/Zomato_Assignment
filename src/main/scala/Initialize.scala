import Restaurant.Restaurant

object Initialize {

  def parseRow(row:String):List[String] = {
    row.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)",-1).toList
  }

  def initialize():Map[Int,Restaurant] ={
    val bufferedSource = io.Source.fromFile("Data/zomato_cleaned - zomato_cleaned.csv")

    var data:Map[Int,Restaurant] = Map()

    for (line <- bufferedSource.getLines) {
      val row = parseRow(line)
      val id = row.head.toInt
      val restaurant = Restaurant(
        row(1),
        if (row(2).contains('/')) row(2).split('/').head.toDouble else 0,
        if (row(3).isEmpty) 0 else row(3).filter(x => x!=',').toInt,
        row(4),
        row(5).split(',').toList,
        row(6).split(',').toList,
        row(7).split(',').toList,
        if (row(8).isEmpty) 0 else row(8).filter(x => x!=',' && x!='\"').toDouble
      )
      data += (id->restaurant)
    }
    bufferedSource.close()
    data
  }

}
