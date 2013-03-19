import scala.actors.Actor
import java.io.PrintWriter
import scala.collection.immutable.TreeMap
/*
 * lecture collector who receive information from webprocessors 
 * send back final collections sorted at the end to manager.
 * if the file name are the same, keep the latest one.
 * 
 */
class LectureCollector extends Actor {
  val lecPattern2 =
    """http://www.cis.upenn.edu/~matuszek/(cit|cis)[0-9]*-([0-9]*)/([0-9]*-)?(.*.(ppt|pptx))""".r
  val lecPattern3 = """/([0-9]*-)?(.*\.(ppt|pptx))""".r
  var lecList = Set[String]()
  var lecTupleList = List[(String, Int, String)]()
  var lecMap = TreeMap(): TreeMap[String, String]
  def act {
    loop {
      react {
        case x: String => lecList += x
        case y: Int => {
          //          val out = new PrintWriter("lecturelist4.txt")

          for (i <- lecList)
            lecPattern2.findPrefixOf(i) match {
              case Some(lecPattern2(prefix, number, hyphen, name, end)) => {
                lecPattern3.findFirstIn(name) match {
                  //use a list of tuple3 to keep the data, the ._2 is the year this lecture is published
                  case Some(lecPattern3(prefix2, filename, end2)) => lecTupleList ::= (filename, number.toInt, i)
                  case _ =>
                }
              }
              case _ =>
            }
          //sort with file name first and then sort with the year(semester) it is published
          val maps_sorted = lecTupleList.sortBy(a => (a._1, a._2))
          
          //use TreeMap, send the sorted data to TreeMap, it will take the latest one when key collision.
          for (lec <- maps_sorted) lecMap += (lec._1 -> lec._3)
          
          //send the collected data to manager at the end
          Manager ! (1, lecMap)
          exit
        }
      }
    }
  }

}