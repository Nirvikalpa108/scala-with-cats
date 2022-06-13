package sandbox


object Blank {
  trait Printable[A] {
    def format(value: A): String
  }
  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      override def format(value: String): String = value
    }
    implicit val intPrintable: Printable[Int] = _.toString
    implicit val catPrintable: Printable[Cat] = cat =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat"
  }
  object Printable {
    def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
    def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
  }
  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit printable: Printable[A]): String = printable.format(value)
      def print(implicit printable: Printable[A]): Unit = println(format)
    }
  }
  final case class Cat(name: String, age: Int, color: String)
  val bob = Cat("", 1, "")
  import sandbox.Blank.PrintableInstances.catPrintable
  PrintableSyntax.PrintableOps(bob).print

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def combine(x: A, y: A): A
    def empty: A
  }
}
