import scala.actors.Actor
import scala.io.Source
/*
 * actor to walk through all the links found in each page until the end recursively.
 * 
 */
class Webproccers(ic: ImageCollector, wc: WebpageCollector, lc: LectureCollector) extends Actor {
  val root = """http://www.cis.upenn.edu/~matuszek.*""".r
  val http = """http://.*""".r
  val HtmlPattern =
    """(?i).*<\s*(a)\s+.*href\s*=\s*['"]?([^'" ]+\.html?)['" >].*""".r
  val ireg = """.*(\?|\#).*""".r
  val ImgPattern =
    """(?i).*<\s*(img)\s+.*src\s*=\s*['"]?([^'" ]+\.(gif|jpg|png))['" >].*""".r
  val lecPattern =
    """(?i).*<\s*(a)\s+.*href\s*=\s*['"]?([^'" ]+\.(ppt|pptx))['" >].*""".r

  def act {
    react {
      case currentURLstring: String => {
        try {
          val currentURL = new java.net.URL(currentURLstring)
          val inStream = currentURL.openStream
          val input = Source.fromInputStream(inStream).mkString
          for (p <- HtmlPattern.findAllIn(input))
            p match {
              case HtmlPattern(tag, link) => {
                //                            println
                //                            println(tag)
                //                            println(link)
                //                            println(name)

                //it matches root = """http://www.cis.upenn.edu/~matuszek""".r
                //it will give it to the next webprocessor
                if (!link.contains("?") && !(link.contains("#")) && !link.startsWith("..") && !link.startsWith("/")) {
                  //construct the absoluteURL
                  val absURLstring = new java.net.URL(currentURL, link).toString

                  root.findPrefixOf(absURLstring.toString) match {
                    case Some(_) => {
                    	//check whether this link has been already exploited
                      //if not, then webpage collector collects this link
                      wc !? absURLstring match {
                        case "go ahead" => {
                          //start a new webproccessor actor to keep check the further link
                          val w2 = new Webproccers(ic, wc, lc)
                          w2.start()
                          //send message to Manager and let manager to add 1 to actor counter
                          Manager ! "NEW"
                          w2 ! absURLstring
                        }
                        case _ =>
                      }

                    }

                    case None =>

                  }
                }
              }
              case _ =>
            }

          for (i <- ImgPattern.findAllIn(input))
            i match {
              case ImgPattern(tag, link, _) => {
                if (!link.startsWith("..") && !link.startsWith("/")) {
                  val absURLstring = new java.net.URL(currentURL, link).toString
                  root.findPrefixOf(absURLstring.toString) match {
                    case Some(_) => ic ! absURLstring   // send this imag found to image collector
                    case _ =>
                  }
                }
              }
              case _ =>
            }

          for (k <- lecPattern.findAllIn(input))
            k match {
              case lecPattern(tag, link, _) => {
                if (!link.startsWith("..") && !link.startsWith("/")) {
                  val absURLstring = new java.net.URL(currentURL, link).toString
                  root.findPrefixOf(absURLstring.toString) match {
                    case Some(_) => lc ! absURLstring //send this lecture found to lecture-collector
                    case _ =>
                  }
                }
              }
              case _ =>
            }

        } catch {
          //          case x:java.io.FileNotFoundException => 
          case x =>
        }
        // tell the manager actor that this actor dies and update the counter
        Manager ! "END"

      }
    }

  }

}