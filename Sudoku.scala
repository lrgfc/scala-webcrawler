import scala.swing.FileChooser
import java.io.File
import scala.io.Source
import scala.swing.Panel

/**a simple Sudoku solver
 * 
 * @author Chenxi Chauncey Wang
 * @version Nov 15 2012
 */

object Sudoku {
  
  /** reads from a file containing a Sudoku puzzle
   * 
   * @return a Option type of java.io.File from the selected file
   */
  
  def readFromFile() :Option[File] ={
  	val chooser = new FileChooser(new File("."))
    val result = chooser.showOpenDialog(new Panel{background})
    if (result == FileChooser.Result.Approve) {
      Some(chooser.selectedFile)
    } else None
  }
  
  /**makes a list of integers based on a character input
   * For each filled-in value in the puzzle, set the corresponding list to contain just that one number and unsolved with all possible numbers 
   * @param ch a character
   * @return a list of that integer if the input is a digit
   * 				 List(1,2,3,4,5,6,7,8,9) if input is '_'
   */
  def makeList (ch: Char):List[Int] = {
    ch match {
      case '_' => (1 to 9).toList
      case digit => List(digit.asDigit)  
    }
  }
  
  /**makes a list of lists containing integers representing the to-be-solved sudoku puzzle
   */
    
  def initializeMatrix(lst:String): List[List[Int]] =
    lst map makeList toList

  /** finds all the solved numbers in one row of Sudoke game
   * @param lst List[List[Int] a row of a two dimentional list
   * @return List[Int] a list of integers in which those numbers are solved already
   */
  def solvedSet(lst:List[List[Int]]):List[Int] = {
    ((lst.filter (_.length == 1)) :\ List(0) ) (_ ::: _)
  }
  
  /** removes the impossible answers at current position from analyzing the current row
   * @param item	a list consists of all the possible answers at that postions
   * @param lst	 the current row is being analyzed
   * @return a list of integers representing the possible numbers at the the input position after analyzing current row  
   */
  def guess(item: List[Int], lst: List[List[Int]]):List[Int] = {
  		item.length match{
  		  case 1 => item
  		  case _ => item diff solvedSet(lst)
  		}
  }
  
  /** removes the impossible answers for all the positions by analyzing every row of the input 2 d lists
   * @param lst a 2 d list(transformed either as rows, columns, or 3x3) in which each position consisting all the possible numbers in the sudoke puzzle
   * @return a 2 d list in which each position consisting all the possible numbers after analyzing
   */
  
  def solveOneRow(lst:List[List[List[Int]]]):List[List[List[Int]]] ={
  		for (a <- lst)
      yield for (b <- a)
        yield guess(b, a.toList)
  }
  
  /**transforming a 2 d list to a new 2d list with nine 3x3 block as nine rows
   *  
   *  @param lst the 2d list to be transformed
   *  @return a 2 d list with every 3x3 block in one row
   */
  
  def matrixTransformation(lst:List[List[List[Int]]]) :List[List[List[Int]]] ={
    (for (i <- (0 to 8)) yield
    	(for ( j <- ( 0 to 8)) yield
    		lst(i/3 * 3 + j / 3)(i % 3 *3 + j % 3)).toList).toList
  }
  
  /**solves the sudoke puzzle by analyzing each rows, columns and 3x3 blocks recursively
   * @param lst lst a 2 d list in which each position consisting all the possible numbers in the sudoke puzzle
   * @return a Stream with the first element be the input list and the second one be the result after analyzing each row, columns, 3x3 block one time
   */
  
  def solveSudoku(lst:List[List[List[Int]]]) : Stream[List[List[List[Int]]]] = {
    lst #:: solveSudoku(matrixTransformation(solveOneRow(matrixTransformation((solveOneRow(solveOneRow(lst).transpose)).transpose) ) ) )
  }
 
  /**prints out the 9x9 matrix with unsolved positions "_"
   * @param lst the 2 D list in which each position consisting all the possible numbers in the sudoke puzzle
   */
  
  def display(lst: List[List[List[Int]]]) {
    for (a <- lst ) {
      for (b <- a) {
        b.length match{
          case 1 => print(b head)
          case _ => print('_')
        }
        print("  ")
      }
    	println()
    }
  }
  
  /**prints out all the unsolved positons in the puzzle and shows the possibilities at each unsloved positions
   * @param lst 2 D list in which each position consisting all the possible numbers in the sudoke puzzle
   */
  def showUnsolved(lst: List[List[List[Int]]]) {
    for (a <- (0 to 8))
      for (b <-(0 to 8))
        lst(a)(b).length match{
        	case 1 => 
        	case _ => {
        	  print("Row: " + (a + 1) + " Column: " + (b + 1) + " => " )
        	  lst(a)(b) foreach (x => print(" " + x + " "))
        	  println()

        	}

      }
  }
  
  def main(args: Array[String]){
    
  	val inputFile = readFromFile()
  	
  	val inputFile2 = inputFile match{
  	  	case Some(x) => {  	  	  
  	  	  val input = Source.fromFile(x).filter(x => x == '_' || x.isDigit).mkString             
  	  	  val initialMatrix = initializeMatrix(input).iterator.toList.sliding(9, 9).toList

  	  	  println("This is the puzzle to be solved: ")
  	  	  display(initialMatrix)
  	  	  println()
    
  	  	  val stream1: Stream[List[List[List[Int]]]] = solveSudoku(initialMatrix)
//    stream1 take 10 foreach println
  	  	  val output = stream1.iterator.sliding(2, 2).find(x => x(0).hashCode() == x(1).hashCode)  //recursively solve the input puzzle until result does not change    
  	  	  output match {
  	  	  	case Some(x) => {
  	  	  	  println("The solved puzzle is as follows: ")
  	  	  		display(x(1))
  	  	  		println()
  	  	  		showUnsolved(x(1))
  	  	  	}
  	  	  	case None => 
  	  	  }
  	  	}   	  	
  	  	case None =>
  	}
  }	    	
}
