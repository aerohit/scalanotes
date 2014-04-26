import scala.util.Try
import java.net.URL

case class Customer(age: Int)

class Cigarettes

case class UnderAgeException(message: String) extends Exception(message)

object CigaretteSeller {
  def buyCigarettes(customer: Customer): Try[Cigarettes] =
    Try {
      if (customer.age < 16)
        throw UnderAgeException(s"Customer must be older than 16, but was ${customer.age}")
      else
        new Cigarettes
    }
}

object UrlParser {
  def parseURL(url: String): Try[URL] = Try(new URL(url))
}

object Trying extends App {
  import CigaretteSeller._
  import UrlParser._
  println(buyCigarettes(Customer(15)))
  println(parseURL("lajf"))
  println(parseURL("lajf").getOrElse("www.google.com"))
  println(parseURL("http://danielwestheide.com"))
}
