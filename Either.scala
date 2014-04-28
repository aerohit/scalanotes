import scala.io.Source
import java.net.URL

object ContentReader {
  implicit def convertStringToURL(urlstr: String) = new URL(urlstr)
  def getContent(url: URL): Either[String, Source] = 
    if (url.getHost.contains("google"))
      Left("Too bad, the content is blocked")
    else
      Right(Source.fromURL(url))
}

object Eithering extends App {
  import ContentReader._

  getContent("https://www.google.com") match {
    case Left(msg) => println(msg)
    case Right(content) => content.getLines.foreach(println)
  }
  val aeContent = getContent("http://aerohitsaxena.com")
  val goContent = getContent("http://www.google.com") 

  val contentR: Either[String, Iterator[String]] =
    aeContent.right.map(_.getLines)

  val moreContentR: Either[String, Iterator[String]] =
    goContent.right.map(_.getLines)

  val contentL: Either[Iterator[String], Source] =
    aeContent.left.map(Iterator(_))

  val moreContentL: Either[Iterator[String], Source] =
    goContent.left.map(Iterator(_))

  val part5 = new URL("http://t.co/UR1aalX4")
  val part6 = new URL("http://t.co/6wlKwTmu")

  val avgLines1: Either[String, Int] = getContent(part5).right.flatMap(p5 =>
      getContent(part6).right.map(p6 => 
          ((p5.getLines.size + p6.getLines.size)/2)))
  println(avgLines1)

  val avgLines2: Either[String, Int] =
    for {
      source1 <- getContent(part5).right
      source2 <- getContent(part6).right
    } yield (source1.getLines.size + source2.getLines.size)/2
  println(avgLines2)

  println(aeContent.left.toOption)
  println(aeContent.right.toOption)
  println(goContent.left.toOption)
  println(goContent.right.toOption)
  println(aeContent.left.toSeq)
  println(aeContent.right.toSeq)
  println(goContent.left.toSeq)
  println(goContent.right.toSeq)
}
