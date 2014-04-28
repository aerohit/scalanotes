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

  val contentR: Either[String, Iterator[String]] =
    getContent("http://aerohitsaxena.com").right.map(_.getLines)

  val moreContentR: Either[String, Iterator[String]] =
    getContent("http://google.map").right.map(_.getLines)

  val contentL: Either[Iterator[String], Source] =
    getContent("http://aerohitsaxena.com").left.map(Iterator(_))

  val moreContentL: Either[Iterator[String], Source] =
    getContent("http://google.map").left.map(Iterator(_))

  println(contentR)
  println(moreContentR)
  println(contentL)
  println(moreContentL)
}
