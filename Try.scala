import scala.util.{Try, Success, Failure}
import java.net.URL
import java.io.InputStream
import scala.io.Source
import java.net.MalformedURLException
import java.io.FileNotFoundException

case class Customer(age: Int)

class Cigarettes

case class UnderAgeException(message: String) extends Exception(message)

object CigaretteSeller {
  def buyCigarettesSafely(customer: Customer): Try[Cigarettes] =
    Try(buyCigarettes(customer))
  def buyCigarettes(customer: Customer): Cigarettes =
    if (customer.age < 16)
      throw UnderAgeException(s"Customer must be older than 16, but was ${customer.age}")
    else
      new Cigarettes

}

object UrlParser {
  def parseURL(url: String): Try[URL] = Try(new URL(url))
  def parseHttpURL(url: String): Try[URL] = parseURL(url).filter(_.getProtocol == "http")
  def getURLContent(url: String): Try[Iterator[String]] = 
    for {
      url <- parseURL(url)
      conn <- Try(url.openConnection())
      is <- Try(conn.getInputStream)
      source = Source.fromInputStream(is)
    } yield source.getLines()
}

object Trying extends App {
  import CigaretteSeller._
  import UrlParser._
  val customer = Customer(15)
  buyCigarettesSafely(customer) match {
    case Success(c) => println(s"$c bought some cigarettes")
    case Failure(e) => println(e)
  }

  println(parseURL("garbage"))
  println(parseURL("garbage") getOrElse new URL("http://www.google.com"))
  println(parseURL("http://www.google.com").map(_.getProtocol))
  println(parseURL("garbage").map(_.getProtocol))

  val inputstream1: Try[Try[Try[InputStream]]] = parseURL("").map { 
    u: URL => {
      Try(u.openConnection()).map(conn => Try(conn.getInputStream))
    }
  }
  val inputstream2: Try[InputStream] = parseURL("").flatMap {
    u: URL => {
      Try(u.openConnection()).flatMap(conn => Try(conn.getInputStream))
    }
  }
  println(parseHttpURL("http://apache.openmirror.de"))
  println(parseHttpURL("ftp://mirror.netcologne.de/apache.org"))
  parseHttpURL("http://apache.openmirror.de").foreach(println)
  parseHttpURL("ftp://mirror.netcologne.de/apache.org").foreach(println)
  getURLContent("http://aerohitsaxena.com").foreach(content => content.foreach(println))
  println("****************")
  getURLContent("http://aerohitsaxena.com") match {
    case Success(lines) => lines.foreach(println)
    case Failure(e) => println(s"Problem fetching content: ${e.getMessage}")
  }
  println("****************")
  val content1: Try[Iterator[String]] = getURLContent("http://www.google.com") recover {
    case e: MalformedURLException => Iterator("Enter a valid url")
    case e: FileNotFoundException => Iterator("Resource isn't available")
  }
  println(content1)

  val content2: Try[Iterator[String]] = getURLContent("http://www.google.com") recoverWith {
    case e: MalformedURLException => Try(Iterator("Enter a valid url"))
    case e: FileNotFoundException => Try(Iterator("Resource isn't available"))
  }
  println(content2)

  val content3: Try[Iterator[String]] = getURLContent("http://www.google.com") orElse Try(Iterator("Enter a valid url"))

  val content4: Option[Iterator[String]] = getURLContent("http://www.google.com").toOption

  val countLines: Try[Int] = getURLContent("http://www.google.com") transform (s => Try(s.length), th => Try(0))
  println(countLines)
}
