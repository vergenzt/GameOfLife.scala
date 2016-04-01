/**
 * Given "w h" on the first line of stdin, then `h` lines of grid with '0's for
 * dead cells and '1's for live cells, output the next generation according to
 * the rules of the Game of Life.
 */
object GameOfLife extends App {
    val Array(width, height) = readLine.split(" ").map(_.toInt)
    val lines = for(i <- 0 until height) yield readLine

    lines
      // pad the borders with dead cells (add 2 to width and height)
      .map('0' +: _ :+ '0').transpose
      .map('0' +: _ :+ '0').transpose
  
      // turn into a grid of length-3 sliding windows (3x3 "neighborhoods")
      // grid is now back to original dimensions
      .sliding(3).map(_.transpose.sliding(3))
      
      // for each row of neighborhoods
      .map( _
        // flatten each neighborhood into 1x9 instead of 3x3
        .map(_.flatten)
        // for each neighborhood
        .map({ nbhood =>
          val cell = nbhood(4)
          val nbcount = nbhood.count(_ == '1') - (if (cell == '1') 1 else 0)
          (cell, nbcount) match {
            case ('1', n) if n < 2   => '0' // under-population
            case ('1', 2) | ('1', 3) => '1' // just right
            case ('1', n) if n > 2   => '0' // over-population
            case ('0', 3)            => '1' // reproduction
          }
        })
      )
      
      .map(_.mkString)
      .foreach(println)
}
