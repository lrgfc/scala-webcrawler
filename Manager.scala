import scala.io.Source
import scala.actors.Actor
import java.io.PrintWriter
import scala.collection.immutable.TreeMap

/*
 * a web crawler to dave's website using Actors
 * @author Chenxi Chauncey Wang
 * @version Dec 9, 2012
 * 
 */
object Manager extends Actor {
  //set up counter to keep track the No. webprocessor is running
  var counter = 1
  val wc = new WebpageCollector
  val ic = new ImageCollector
  val lc = new LectureCollector
  val w = new Webproccers(ic, wc, lc)

  //set up counter to keep track whether the other three collectors has sent back the data
  var filecounter = 0

  val finalresult = new PrintWriter("output.txt")
  var webpages: List[(String, String)] = Nil
  var images = TreeMap(): TreeMap[String, String]
  var lectures = TreeMap(): TreeMap[String, String]

  def main(args: Array[String]) {
    this.start
    wc.start()
    ic.start
    lc.start
    w.start()
    w ! "http://www.cis.upenn.edu/~matuszek"

  }
  /*
 * print out the collected results to final output
 * 
 */
  def printResults() {
    finalresult.println("The webpages are as follows:")

    for (l <- webpages) finalresult.println(l._1 + "	" + l._2)
    finalresult.println()
    finalresult.println()
    finalresult.println()

    finalresult.println("The lectures are as follows:")

    for (lec <- lectures) finalresult.println(lec._1 + "	" + lec._2)
    finalresult.println()
    finalresult.println()
    finalresult.println()

    finalresult.println("The images are as follows:")

    for (img <- images) finalresult.println(img._1 + "	" + img._2)
    finalresult.close
  }

  def act {
    loop {
      react {
        case "NEW" => {
          //populate the counter when a new webproccer actor is turned on
          counter += 1
          //          println(counter)
        }
        case "END" => {
          //a webproccer dies
          counter -= 1
          //          println(counter)
          if (counter == 0) {

            //all webproccer dies, no more links to go to
            //call the other collectors to send data back to Manager
            wc ! -99
            ic ! -99
            lc ! -99

          }
        }
        case x: List[(String, String)] => {
          // receive the collected webpages from webpage collector
          webpages = x
          filecounter += 1

          if (filecounter == 3) {
            printResults()

            exit
          }
        }
        case x: TreeMap[String, String] => {
          images = x
          filecounter += 1
          if (filecounter == 3) {
            printResults
            exit
          }
        }

        case x: (Int, TreeMap[String, String]) => {

          lectures = x._2

          filecounter += 1
          if (filecounter == 3) {
            printResults

            exit
          }
        }
      }
    }
  }
}