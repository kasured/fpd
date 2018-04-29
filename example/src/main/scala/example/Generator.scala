package example

/**
  * Created by kasured on 9/29/16.
  */
sealed trait Generator[+T] {

  self =>

  def generate: T

  def map[S](f: T => S) = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }

}


object Generator {
  val integers = new Generator[Int] {
    val rand = new java.util.Random

    override def generate: Int =
      rand.nextInt()
  }
}