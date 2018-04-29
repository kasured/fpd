import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

////////////////////////
def and(x: Boolean, y: Boolean) = if (x) y else false

assert(and(true, true))

assert(and(false, true) == false)

def loop: Boolean = loop

def and1(x: Boolean, y: => Boolean) = if (x) y else false

assert(and1(false, loop) == false)


////////////////////////
def factorial(n: Int): BigInt = if (n == 1) 1 else n * factorial(n - 1)

factorial(1)
factorial(5)

def factorialTail(n: Int): BigInt = {
  @tailrec
  def recur(i: Int, acc: BigInt): BigInt = if (i == 1) acc else recur(i - 1, acc * i)
  recur(n, 1)
}

factorialTail(1)
factorialTail(5)

/////////////////////////////////////////
val xO = Some(1)
val yO = Some(2)

def sumM(xO: Option[Int], yO: Option[Int]): Option[Int] = xO flatMap (x => yO map (y => x + y))
sumM(xO, yO)

def sumF(xO: Option[Int], yO: Option[Int]): Option[Int] =
  for {
    x <- xO
    y <- yO
  } yield x + y

sumF(xO, yO)

//////////////////////////////////////////
val intsO = List(1, 2, 3)

def sumL(list: List[Int]): Int = (list foldLeft 0)( _ + _)

def sumR(list: List[Int]): Int = (list foldRight 0)( _ + _)

///////////////////////////////////////
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

def futureOne = Future {Thread.sleep(1000); 1}
def futureTwo = Future {Thread.sleep(2000); 2}

def sum(oneF: Future[Int], twoF: Future[Int]): Future[Int] = {
  val f1 = oneF
  val f2 = twoF
  for {
    one <- f1
    two <- f2
  } yield one + two
}

Await.result(sum(futureOne, futureTwo), 5 seconds)

/////////////////////////////////////////
def balance(chars: List[Char]): Boolean = {

  @tailrec
  def _balance(xs: List[Char], diff: Int): Boolean = xs match {
    case Nil => diff == 0
    case ')' :: tail if diff == 0 => false
    case ')' :: tail => _balance(tail, diff - 1)
    case '(' :: tail => _balance(tail, diff + 1)
    case head :: tail => _balance(tail, diff)
  }

  _balance(chars, 0)

}