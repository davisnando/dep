import scala.math.pow

object main {

  def main(args: Array[String]): Unit = {
    val input = convToDouble(args);
    println(input.mkString("\n"));
    val columns = Array("Iris-setosa", "Iris-versicolor", "Iris-virginica")

    var value = majorityVote(getNeighbours(getDistances(convert_data(get_data("iris.data")), input, input.length), 3))
    println("Outcome: " + columns(value.toInt))
  }

  def convert_data(data : Array[Array[String]]) : Array[Array[Double]] ={
    if(data.length > 1 ){
      return convert_data(data.slice(1, data.length)) :+ convToDouble(data(0))
    }
    return Array(convToDouble(data(0)))
  }

  def get_data(filename: String): Array[Array[String]] = {
    val bufferedSource = io.Source.fromFile(filename)
    return read_line(bufferedSource.getLines)
  }
  def read_line(lines :Iterator[String]) : Array[Array[String]] = {
    if (lines.hasNext){
      val line = lines.next()
      println(line)
      if(line.length > 1){
        return read_line(lines) :+ line.split(",")
      }
      return Array(line.split(","))
    }
    return Array()
  }
  def convToDouble(args: Array[String]): Array[Double] = {
    if (args.length > 1) {
      return args(0).toDouble +: convToDouble(args.slice(1, args.length))
    }
    return Array(args(0).toDouble)
  }

  def calcDistance(x: Array[Double], y: Array[Double], length: Int): Double = {
    if (length > 1) {
      return pow(x(0) - y(0), 2) + calcDistance(
        x.slice(1, length),
        y.slice(1, length),
        length - 1
      )
    }
    return math.sqrt(pow(x(0) - y(0), 2))
  }

  def getDistances(data: Array[Array[Double]], predict: Array[Double], length: Int): Array[Array[Double]] ={
    var distance =  data(0) :+ calcDistance(data(0),predict, length)
    if(data.length > 1){
      return distance +: getDistances(data.slice(1, data.length), predict, length)
    }
    return Array(distance)
  }

  def getNeighbours(distances: Array[Array[Double]], k: Int): Array[Array[Double]] = {
    // returns inputs, key, distance
    return distances.sortBy(_.length).reverse.slice(0, k)
  }

  def getKeys(neighbours: Array[Array[Double]]) : Array[Double] = {
    val data = neighbours(0)
    if(neighbours.length > 1){
      return getKeys(neighbours.slice(1, neighbours.length)) :+ data(data.length - 2)
    }
    return Array(data(data.length - 2))
  }
  def majorityVote(neighbours: Array[Array[Double]]) : Double = {
    val keys = getKeys(neighbours)
    return keys.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  }
}
