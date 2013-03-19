object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val root = """http://.*""".r                    //> root  : scala.util.matching.Regex = http://.*
	val x = root.findPrefixOf("http://www.google.com")
                                                  //> x  : Option[String] = Some(http://www.google.com)
	x match {
	case Some(y) => println(y)}               //> http://www.google.com
	    val ireg = """.*(\?|\#).*""".r        //> ireg  : scala.util.matching.Regex = .*(\?|\#).*
ireg.findPrefixOf("sdagag?")                      //> res0: Option[String] = Some(sdagag?)

}