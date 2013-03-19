import scala.actors.Actor
import java.io.PrintWriter
import scala.collection.immutable.TreeMap
/*
 * imagecollectors receives img from webproccerssor and send the final collection back to manager actor 
 * use TreeMap to keep the data. TreeMap is implemented via Red Black Tree. It is sorted by key.
*/

class ImageCollector extends Actor {
  val ImgPattern2 =
    """(http://www.cis.upenn.edu/~matuszek).*/(.*(gif|jpg|png))""".r
var imageMap = TreeMap():TreeMap[String,String]
    def act {
    loop {
      react {
        case x: String => {
          ImgPattern2.findPrefixOf(x)  match {
            //use TreeMap to keep the data. TreeMap is implemented via Red Black Tree. It is sorted by key.
            case Some(ImgPattern2(prefix,name,ending)) => imageMap += (name -> x) 
            case _ =>
          }
          
//          imageList += x
          
        }
        case y: Int => {
          //          println(y)
//          val out = new PrintWriter("images.txt")
//          for (img <- imageMap) out.println(img._1 + "	" + img._2)
//          out.close()
          
          //send the final sorted collection to manager
        	Manager ! imageMap
          exit
        }

      }
    }
  }
}