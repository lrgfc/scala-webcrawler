import scala.actors.Actor
import java.io.PrintWriter
import scala.collection.immutable.TreeSet
import scala.collection.immutable.TreeMap
/*
 * webpage collectors
 * check whether one link is already contained avoiding loops
 * communicate with mannager and send final collection to it 
*/

class WebpageCollector extends Actor {
 val webPattern2 =
    """(http://www.cis.upenn.edu/~matuszek).*/(.*.(html|htm))""".r
    
  var log = List[String]()
  var webMap =List[(String,String)]()
  def act {
    loop {
      react {
        case x: String => {
          //avoiding loops
          if (log contains x) reply("LOOP")
          else {
            //            println(x)
            reply("go ahead")
            log ::= x

          }
        }
        case x: Int => {
          //          debug
          //          println("it ends!!!")
          
          for (i<- log) 
            webPattern2.findPrefixOf(i) match{
            case Some(webPattern2(prefix, name, end)) => webMap ::= (name,i)
            case _ =>
          }
          //send the sorted collection to Manager and terminate itself
          Manager ! webMap.sortBy(_._1 toLowerCase)
          exit
        }
      }
    }
  }
}