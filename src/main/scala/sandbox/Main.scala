package sandbox

object Main extends App {




  // CHAPTER 1
//  import sandbox.Chapter1._
//  import PrintableInstances._
//  import PrintableSyntax._
//  println("Hello " |+| "Cats!")
//  val cat = Cat("Bobo", 2, "Black")
//  // use the type class to print the cat (object Printable)
//  Printable.print(cat)
//  // use the extension methods to print the example cat (implicit class PrintableOps)
//  cat.print

//  import cats.Eq
//  import cats.instances.long._
//  import cats.syntax.eq._
//
//  import java.util.Date
//  implicit val dateEq: Eq[Date] = Eq.instance[Date]((x,y) =>  x.getTime === y.getTime)
//  val x = new Date()
//  val y = new Date(12245)
//  println(x === x)
//  println(x === y)
//  import cats.instances.string._
//  import cats.instances.int._
//  import cats.instances.option._
//  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (x, y) =>
//    x.colour === y.colour && x.name === y.name && x.age === y.age
//  }
//  val cat1 = Cat("Garfield",   38, "orange and black")
//  val cat2 = Cat("Heathcliff", 33, "orange and black")
//  println(cat1 === cat2)
//  val optionCat1 = Option(cat1)
//  val optionCat2 = Option.empty[Cat]
//  println(optionCat1 === optionCat2)
}
