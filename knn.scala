import scala.math.pow

object main {

  def main(args: Array[String]): Unit = {
    var input = convToDouble(args);
    println(input.mkString("\n"));

    var d = calcDistance(input, input, 3)
    var distancesArray = Array(Array(1.0,2.0,34.0), Array(1.0,3.0,34.0), Array(5.0,2.0,34.0))

    var distances = getDistances(distancesArray, input, input.length)
    // println(distances.map(_.mkString(" ")).mkString("\n"))
    distances = getNeighbours(distances, 1)
    println(distances.map(_.mkString(" ")).mkString("\n"))

  }

  def convToDouble(args: Array[String]): Array[Double] = {
    if (args.length > 1) {
      return args(0).toDouble +: convToDouble(args.slice(1, args.length))
    }
    return Array(args(0).toDouble)
  }

  def calcDistance(x: Array[Double], y: Array[Double], length: Int): Double = {
    var distance = pow(x(0) - y(0), 2);
    if (length > 1) {
      return distance + calcDistance(
        x.slice(1, length),
        y.slice(1, length),
        length - 1
      )
    }
    return math.sqrt(distance)
  }

  def getDistances(data: Array[Array[Double]], predict: Array[Double], length: Int): Array[Array[Double]] ={
    var distance =  data(0) :+ calcDistance(data(0),predict, length)
    if(data.length > 1){
      return distance +: getDistances(data.slice(1, data.length), predict, length)
    }
    return Array(distance)
  }

  def getNeighbours(distances: Array[Array[Double]], k: Int): Array[Array[Double]] = {
    return distances.sortBy(_.length).reverse.slice(0, k)
  }

  
}
