package sandbox

object Chapter1 {
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
      override def write(value: String): Json = JsString(value)
    }
    implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }
  }

  //object interface
  //to use this, we import any type class instances we need and call the relevant method
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  import JsonWriterInstances._
  Json.toJson(Person("Amina", "me@amina.com"))//(personWriter) the compiler inserts the implicit for us

  //interface syntax
  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
    }
  }

  //we use interface syntax by importing it alongside the instances for the types we need
  import JsonSyntax._
  Person("Semir", "me@sem.com").toJson

  //scala standard library
  //use to summon any value from implicit scope
  //we provide the type we want and implicitly does the rest
  def implicitly[A](implicit value: A): A = value
  implicitly[JsonWriter[String]]

  // instead of defining type class instances for all As and Option[A] we can just write a method
  // that knows how to handle Option[A]
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = value match {
        case Some(aValue) => writer.write(aValue)
        case None => JsNull
      }
    }

  //exercise 1.3
  //1. Define a typeclass Printable[A] containing a single method format.
  //format should accept a value of type A and return a String.
  trait Printable[A] {
    def format(value: A): String
  }
  //2. Create an object PrintableInstances containing instances of Printable for String and Int.
  object PrintableInstances {
    implicit val printableString = new Printable[String] {
      override def format(input: String) = input
    }
    implicit val printableInt = new Printable[Int] {
      override def format(input: Int) = input.toString
    }
    implicit val printableCat = new Printable[Cat] {
      override def format(cat: Cat) = s"${Printable.format(cat.name)} is a ${Printable.format{cat.age}} year-old ${Printable.format(cat.colour)} cat."
    }
  }
  //3. Define an object Printable with two generic interface methods:
  object Printable {
    //format accepts a value of type A and a Printable of the corresponding type.
    // It uses the relevant Printable to convert the A to a String.
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
    //print accepts the same parameters as format and returns Unit.
    // It prints the formatted A value to the console using println.
    def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
  }
  final case class Cat(name: String, age: Int, colour: String)

  // this defines extension methods to provide better syntax
  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(format)
    }
  }
}

object MeetCats {
  // very similar to the printable type class we wrote for ourselves
  import cats.Show
  // this package provides default instances for Ints and Strings
  import cats.instances.int._
  import cats.instances.string._

  // the companion object of ever Cats type class has an apply method that locates an instance for any type we specify
  // below are two instances of Show. We can use them to print Ints and Strings.
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  // we can make show easier to use by importing the interface syntax
  // this adds an extension method called show to any type for which we have an instance of Show in scope
  import cats.syntax.show._
  val showIntAgain = 123.show
  val showStringAgain = "abc".show

  // advice about which imports to use all the time
  // import cats._ this imports all type classes
  // import cats.implicits._ imports all the standard type class instances and all of the syntax in one go

  //1.4.5 defining custom instances
  // we can define an instance of Show, simply by implementing the trait for a given type
  import java.util.Date
  implicit val dateShow: Show[Date] = new Show[Date] {
    override def show(d: Date): String = s"${d.getTime}ms since the epoch"
  }
  //new Date().show (commenting this out because it didn't know which of the two implicits to use lol
  // cats also provides convenient methods to simplify the process
  // there are two construction methods on the companion object Show that we can use to define instances for our own types
  implicit val dateShowAgain: Show[Date] = Show.show(d => s"${d.getTime}ms since the epoch")
  // Exercise 1.4.6
  import sandbox.Chapter1.Cat
  // I created an instance of Cat for the typeclass Show
  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is ${cat.age} years old and is the colour ${cat.colour}")
  val myCat: Cat = Cat("tiger", 5, "orange")
  // now I can use the show extension method on my cat, becuase I have an instance (the implicit) in scope
  myCat.show
}

object ExampleEq {
  // cats.Eq is designed to support type-safe equality, because there are challenges with Scala's === operator
  import cats.Eq
  // get an instance
  import cats.instances.int._
  val eqInt = Eq[Int]
  // use eqInt directly to test for equality
  eqInt.eqv(123, 123) // if we change one of these to a string we get a compile error, unlike with scala ==
  // import the interface syntax to use ===
  import cats.syntax.eq._
  123 === 123
  123 =!= 124
  //1.5.3 Comparing Options
  // to compare values of type Option[Int] we need to import instances of eq for Option as well
  import cats.instances.option._
  // we need to annotate the arguments
  (Some(1) : Option[Int]) === (None : Option[Int])
  // we can use Option.apply to re-write this
  Option(1) === Option.empty[Int]
  // or we can use cats syntax
  import cats.syntax.option._
  1.some === none[Int]
  //1.5.4 comparing custom types
  // define our own instances of Eq
  import Chapter1.Cat
  import cats.instances.long._

  import java.util.Date
  implicit val dateEq: Eq[Date] = Eq.instance[Date]((x,y) =>  x.getTime === y.getTime)
  val x = new Date()
  val y = new Date()
  x === x
  x === y
  //1.5.5 exercise
  // implement an instance of Eq for cat
  import cats.instances.string._
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (x, y) =>
    x.colour === y.colour && x.name === y.name && x.age === y.age
  }
  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  cat1 === cat2
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  optionCat1 === optionCat2
}

object ControllingInstanceSelection {
  //the relationship between an instance defined on a type (like Option) and its subtypes (Some and None) for example
  //1.6.1 Variance
  // when we define type classes we can add variance annotations to the type parameter to affect the variance of the type class
  // and the compilers ability to select instances during implicit resolution.
  // variance relates to subtypes

  //Covariance trait F[+A]
  //Covariance means that type F[B] is a subtype of type F[A] if B is a subtype of A
  //eg we can use List[Circle] anywhere we expect List[Shape] because Circle is a subtype of Shape
  //generally, covariance is used for data that we can get out of a container like a List

  //Contravariance trait F[-A]
  //Contravariance means that type F[B] is a subtype of F[A] if A is a subtype of B
  //Imagine when we have 2 JsonWriters, one for Shape and one for Circle
  //We can write Circle with either JsonWriter, but we don't want to use the Circle JsonWriter for Shape, because not all shapes are circles

  //Invariance trait F[A]
  //The easy one. This means that F[A] and F[B] are never subtypes of one another.

  //So, we can use variance annotations to control type class instance selection to some extent.
  sealed trait A
  final case object B extends A
  final case object C extends A
  //There are 2 issues that tend to arise
  //1. can we define an instance for A and have it work for values of types B and C?
  //2. if we have instances for both A and B and we have a value of type B, will the instance for B be selected rather than A?
  //A practical point is that if we have an instance defined for A and we have a value of type B, we can add a type annotation to B so the compiler
  //knows it's a subtype of A, eg if have the value Some(1) and the instance is on Option[Int] we can either annotate the Some or use Option's
  //smart constructor eg Option.apply or Option.empty

}