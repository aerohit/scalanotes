package neophytes

import scala.concurrent.future
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random


case class GrindException(msg: String) extends Exception(msg)
case class FrothingException(msg: String) extends Exception(msg)
case class WaterBoilingException(msg: String) extends Exception(msg)
case class BrewingException(msg: String) extends Exception(msg)


object WelcomeToTheFuture extends App {
  type CoffeeBeans = String
  type GroundCoffee = String
  case class Water(temperature: Int)
  type Milk = String
  type FrothedMilk = String
  type Espresso = String
  type Cappuccino = String

  def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
    println("start grinding...")
    Thread.sleep(Random.nextInt(2000))
    if (beans == "baked beans") throw GrindException("are you joking")
    println("finished grinding...")
    s"ground coffee of $beans"
  }

  def heatWater(water: Water): Future[Water] = Future {
    println("heating the water now")
    Thread.sleep(Random.nextInt(2000))
    println("hot, it's hot!")
    water.copy(temperature = 85)
  }

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    println("milk frothing system engaged!")
    Thread.sleep(Random.nextInt(2000))
    println("shutting down milk frothing system")
    s"frothed $milk"
  }

  def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
    println("happy brewing :)")
    Thread.sleep(Random.nextInt(2000))
    println("it's brewed!")
    "espresso"
  }

  def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"

  def temperatureOkay(water: Water): Future[Boolean] = Future {
    (80 to 85).contains(water.temperature)
  }

  val acceptable: Future[Boolean] = for {
    heatedWater <- heatWater(Water(25))
    okay <- temperatureOkay(heatedWater)
  } yield okay

  def prepareCappuccinoSequentially(): Future[Cappuccino] = {
    // water would start heating only after grinding.
    // so always create all the futures before entering the for comprehension.
    for {
      ground <- grind("arabica beans")
      water <- heatWater(Water(20))
      foam <- frothMilk("milk")
      espresso <- brew(ground, water)
    } yield combine(espresso, foam)
  }

  def prepareCappuccino(): Future[Cappuccino] = {
    val groundFuture = grind("arabica beans")
    val heatedWaterFuture = heatWater(Water(20))
    val frothedMilkFuture = frothMilk("milk")
    for {
      ground <- groundFuture
      water <- heatedWaterFuture
      espresso <- brew(ground, water)
      foam <- frothedMilkFuture
    } yield combine(espresso, foam)
  }

  // Why is the following print message not displayed.
  prepareCappuccino().map { _ => println("Done preparing Cappuccino") }
}

