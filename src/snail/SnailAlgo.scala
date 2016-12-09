package snail

object SnailAlgo {
  val numRows = 4
  val numCols = 3        
  val matrixSize = numRows * numCols 
  
  type Coord = (Int, Int)
  type Operand = (Int, Int)
  type Path = List[Coord]

  val incs = List((0, 1), (1, 0), (0, -1), (-1, 0))
  
  // incs defines directions: go right, go down, go left, go up, etc. 
  // In order to continuously iterate through those values, I used Stream.continually():
  val incsIt = (for(x <- Stream.continually(); y <- incs) yield y).iterator
  def nxtOp = incsIt.next 
  
  def walk(coord: Coord = (0, 0), op: Operand = nxtOp, path: Path = List()): Path = {
    if (path.size == matrixSize) path else {
      val nrc = (coord._1+op._1, coord._2+op._2)
      if (nrc._1 < 0 || nrc._1 >= numRows || nrc._2 < 0 || nrc._2 >= numCols || (path contains nrc))
        walk(coord, nxtOp, path)
      else
        walk(nrc, op, if (path.size == matrixSize-2) path :+ coord :+ nrc else path :+ coord)
    }
  }   
  
  def main(args: Array[String]): Unit = {
    val p1 = walk()          
    println (p1)
  }
}