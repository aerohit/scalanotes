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

  println(getContent("http://www.aerohitsaxena.com"))

  getContent("https://www.google.com") match {
    case Left(msg) => println(msg)
    case Right(content) => content.getLines.foreach(println)
  }
}
