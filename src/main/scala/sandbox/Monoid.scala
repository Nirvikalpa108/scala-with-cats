package sandbox

object Monoid {
//Monoids and Semigroups
// allow us to add or combine values
// there are instances for Ints, Strings, Lists, Options and many more
// a monoid is something that has an associative binary addition and an identity element
// integer subtraction is not a monoid because it's not associative (you can't do it in any order)
  trait Monoid2[A] {
    //must obey associative law
    def combine(x: A, y: A): A
    //must obey identity law
    def empty: A
  }
  // a semigroup is the combine part of a monoid, without the empty part.
  // we see this kind of inheritance a lot. It's good because if we define a Monoid, we get a Semigroup for free.
  // also, if a method requires a Semigroup[B], we can pass a Monoid[B] instead.
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
    def combine(x: A, y: A): A
  }
  //Exercise 2.3 - How many monoids can you define for Boolean?
  //***I don't understand - go through this with someone***
  //I think what I don't understand is more to do with boolean logic (and and or) and how that relates to empty / combine
  //go through this exercise again?
  object MonoidInstances {
    implicit val booleanAndMonoid: Monoid[Boolean] =  new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }
    implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false
      override def combine(x: Boolean, y: Boolean): Boolean = ???
    }
  }
  //Exercise 2.4 - What monoids and semigroups are there for sets?
  //**I'm not even sure what to think about this one. I haven't looked at the answers yet.

  //2.5 Monoids - Cats implementation; the type class, the instances and the interface
  import cats.Monoid
  import cats.Semigroup
  import cats.instances.string._
  //Monoid follows the standard Cats pattern for the user interface: the companion object has an apply method
  //that returns the type class instance for a particular type.
  Monoid[String].combine("Hi", "there")
  Monoid[String].empty
  Monoid.apply[String].combine("Hi", "there")
  Monoid.apply[String].empty
  //If we don't need empty, we can use Semigroup instead which just gives us combine
  import cats.Semigroup
  Semigroup[String].combine("Hi", "there")
  import cats.instances.int._
  Monoid[Int].combine(12,10)
  // we can assemble a Monoid[Option[Int]] using the right type class instances
  import cats.instances.option._
  Monoid[Option[Int]].combine(Some(2), Some(3))
  // as always, unless we have a good reason not to, we can just import everything
  import cats._
  import cats.implicits._
  // Cats provides syntax for the combined method using the |+| operator
  // because combine technically comes from Semigroup, we access the syntax with this import:
  import cats.instances.string._ // for Monoid
  import cats.syntax.semigroup._ // for |+|
  val stringResult = "Hi" |+| "there" |+| Monoid[String].empty
  import cats.instances.int._ // for Monoid
  val intResult = 1 |+| 2 |+| Monoid[Int].empty
  //[Wow this is so interesting to have an operator which works on every type that has an instance of Semigroup defined]

  //Exercise 2.5.4
  def add(items: List[Int]): Int = items.foldRight(0)((n, acc) => n |+| acc)
  def add2(items: List[Int]): Int = items.foldLeft(0)(_ + _)
  def add3(items: List[Int]): Int = items.sum
  def add4(items: List[Int]): Int = items.foldLeft(Monoid[Int].empty)(_ + _)
  def add5(items: List[Option[Int]]): Int = items.foldLeft(0){(acc, maybeInt) => maybeInt match {
    case Some(n) => acc |+| n
    case None => 0
    }
  }
  // now there is a use case for Monoids, because we need a single method that adds Ints and Option[Int]
  import cats.Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats._
  import cats.implicits._
  import cats.instances._
  //** why is this not working? It's now literally exactly what they have in the book.
  def add6[A](items: List[A])(implicit m: Monoid[A]): A = items.foldLeft(m.empty)(_ |+| _)
  // using a context bound here to make it a bit shorter
  //****not sure why this is not working? I've tried all the imports.
  def add7[A : Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)()
  // now ensure that add can take an Order
  case class Order(totalCost: Double, quantity: Double)
  // define a Monoid instance for Order
  implicit val MonoidOrder: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0,0)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
  //2.7 Summary
  // a Semigroup represents an additional or combination operation
  // a Monoid extends a Semigroup by adding an identity / zero element
  // to use them, we import the type classes themselves, the type class instances we want and the semigroup syntax to get |+|
  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.semigroup._
}
