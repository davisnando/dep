import scala.math.pow

object main {

  def main(args: Array[String]): Unit = {
    // get console input and convert to double
    val input = convToDouble(args);
    // Hardcoded prediction names
    val columns = Array("Iris-setosa", "Iris-versicolor", "Iris-virginica")

    // calculate closest group 
    var value = majorityVote(getNeighbours(getDistances(convert_data(get_data("iris.data")), input, input.length), 3))
    // print prediction
    println("Predict: " + columns(value.toInt))
  }

  def convert_data(data : Array[Array[String]]) : Array[Array[Double]] ={
    // convert output of read_lines function to useable double matrix
    if(data.length > 1 ){
      return convert_data(data.slice(1, data.length)) :+ convToDouble(data(0))
    }
    return Array(convToDouble(data(0)))
  }

  def get_data(filename: String): Array[Array[String]] = {
    // open an text file
    val bufferedSource = io.Source.fromFile(filename)
    return read_line(bufferedSource.getLines)
  }
  def read_line(lines :Iterator[String]) : Array[Array[String]] = {
    // Read line out of an iterator string one by one and create an Array[Array[String]]
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
    // convert string array to a double array
    if (args.length > 1) {
      return args(0).toDouble +: convToDouble(args.slice(1, args.length))
    }
    return Array(args(0).toDouble)
  }

  def calcDistance(x: Array[Double], y: Array[Double], length: Int): Double = {
    // distances function caculates distance between 2 vectors
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
    // Calculate all the distances of all the data
    var distance =  data(0) :+ calcDistance(data(0),predict, length)
    if(data.length > 1){
      return distance +: getDistances(data.slice(1, data.length), predict, length)
    }
    return Array(distance)
  }

  def getNeighbours(distances: Array[Array[Double]], k: Int): Array[Array[Double]] = {
    // Sort all of the distances by smallest first and selecting only k amount
    // returns inputs, key, distance
    return distances.sortBy(_.length).reverse.slice(0, k)
  }

  def getKeys(neighbours: Array[Array[Double]]) : Array[Double] = {
    // make an array of all of the keys
    if(neighbours.length > 1){
      return getKeys(neighbours.slice(1, neighbours.length)) :+ neighbours(0)(neighbours(0).length - 2)
    }
    return Array(neighbours(0)(neighbours(0).length - 2))
  }
  def majorityVote(neighbours: Array[Array[Double]]) : Double = {
    // Count the most common object
    return getKeys(neighbours).groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  }
}
