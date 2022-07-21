package sandbox

object Functors {
  // normally a functor is anything with a map method
  // allows us to represent sequences of operations within a context such as List and Option
  // functors on their own aren't that useful, but special cases such as Monads and Applicative Functors, are very common.

  // note that Futures are not a good example of pure functional programming, because they aren't referentially transparent
  // **** Discuss Futures not being functional
  // Futures compute and cache a result and there's no way for us to tweak this behaviour. We get unpredictable results.
  // they start computations immediately, rather than allowing the user to dictate when the programme should run
  // single argument functions are also functors

  // I really don't understand pages 52-54 on Functors
  // single arg functions are also functors
  // in a way, they're like a lazy queue

//  trait Functor[F[_]] {
//    def map[A, B](fa: F[A])(f: A => B): F[B]
//  }
  //Functor Laws - functors guarantee the same semantics whether we sequence many small operations one by one, or combine them
  // into a larger function before mapping. To ensure this is the case, the following two laws must hold:
  // 1. identity (calling map with A => A, the identity function, is the same as doing nothing)
  //fa.map(a => a) == fa
  // 2. composition (mapping with 2 functions is the same as mapping one and then the next)
  //fa.map(g(f(_))) == fa.map(f).map(g)

  // type constructors and higher-kinded types
  def myMethod[F[_]] = ??? //type constructor. It can take anything like a List, Option

  import cats.Functor // the Functor type class
  import cats.instances.list._
  import cats.instances.option._
  val list1 = List(1,2,3)
  val list2 = Functor[List].map(list1)(_ * 2)
  val option1 = Option(1)
  val option2 = Functor[Option].map(option1)(_.toString())
  // Functor provides a method called lift, which converts a function of type A => B
  // to one that operates over a functor and has type F[A] => F[B]
  // that's pretty cool. We can lift the function into a wrapper.
  val func = (x: Int) => x + 1
  val liftedFunc: Option[Int] => Option[Int] = Functor[Option].lift(func)
  // another method it has, is as
  Functor[List].as(list1, "repeat me") // List("repeat me", "repeat me", "repeat me")

  //3.5.2 Functor Syntax - map is the main one
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for Map
  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"${a}!"
  val func4 = func1.map(func2).map(func3)
  //another example
  //I couldn't write it how he had (page 59), maybe just an IntelliJ issue ***
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(start)(n => n + 1 * 2)
  //here is a simplified definition of cats.syntax.functor map:
  //this is an extension method the compiler can use to create a built-in map method where one does not exist
  //can we go over this function together please? ****
//  implicit class FunctorOps[F[_], A](src: F[A]) {
//    def map[B](func: A => B)(implicit functor: Functor[F]): F[B] = functor.map(src)(func)
//  }

  //3.5.3 Instances for Custom Types
  //Functor[Option] implementation
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
  // but what if we need to inject dependencies into our instances?
  // For example, Functor[Future] needs an implicit execution context
  import scala.concurrent.{Future, ExecutionContext}
  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] = value.map(func)
    }
  // the above means that the compiler will look for futureFunctor implicitly
  // and it will also look for ExecutionContext.

  //3.5.4 Exercise: Branching out with Functors
  // Write a Functor for the following binary tree data type.
  // Verify that the code works as expected on instances of Branch and Leaf:
  sealed trait Tree[+A] //remind myself about what + means => Covariance
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  // remember the functor laws
  // functor can only modify what's inside the container, not the container itself, it's structure etc
  // so if you're given a Branch, you must return a Branch. If you're given a Leaf, return a Leaf.

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  // intro to the section on variance
  // variance affects subtyping; our ability to use a value of one type in place of another
  // standard covariant functor: B is a subtype of A, we have B => A, so we can do F[B] => F[A]
  // contravariant functor: if we have an F[A] and a function B => A, we can convert F[A] => F[B]
  // invariant functor: convert from F[A] to F[B] via functions A => B and B => A.

  //3.6 Contravariant and Invariant Functors
  //let's go over the top of page 62
  //it's not a big deal. I don't really understand this, that's ok.

  //3.6.1 Contravariant Functors and the contramap Method
  // contravariant functor is a type class
  // it provides an operation called contramap
  // contramap represents prepending an operation to a chain
  // it takes an F[B] and A => B and outputs an F[A]
  // this type only makes sense for data types that represent transformations
  // eg we can't use Option because we can't feed Option[B] backwards through a function A => B

  // let's use printable as an example of the contravariant functor typeclass
  trait Printable[A] { self => // this self thing is mostly an oo thing, no worries
    def format(value: A): String
    def contramap[B](f: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String = self.format(f(value))
    }
  }
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s"'${value}'"
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if(value) "yes" else "no"
  }

  final case class Box[A](value: A)
  //this is what I started writing out
  //implicit def boxPrintable[A](value: A): Printable[Box] = new Printable[Box]
  //???hat knowledge/understanding am I missing?
  // understanding the difference between functor which takes F[_] which could take Box
  // and then things like List[A] and then we could do List[Box[A]] OR Printable[A]
  //Box needed it's type param A
  // when I see a case class that has a type param, that has to follow it about, it can't exist without it
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = new Printable[Box[A]] {
    override def format(b: Box[A]): String = p.format(b.value)
  }
  // create your instance from an existing instance using contramap
  //...trying to understand...
  // I have existing instances (I have String and Boolean Printables)
  // the Printable typeclass has a contramap method on it, I just need to supply it a function
  // so this is the answer. ok I was on the COMPLETE WRONG TRACK with understanding this question.
  //implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap(_.value)

  // 3.6.2 Invariant functors (and the imap method)
  // imap is informally equivalent to a combination of map and contramap
  // rememeber that map generates new type class instances by appending a function to a chain **** (make sure I understand this)
  // contramap generates new type class instances by prepending an operation to a chain ****
  // imap generates them via a pair of bi-directional transformations eg encoding and decoding

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
  }
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): String = value
  }

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  //implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap(s => Box(s), b => b.value)
  implicit def boxCodecAgain[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_), _.value)

  //3.7 Contravariant and Invariant in Cats
//  trait Contravariant[F[_]] {
//    // ***** this feels SO COUNTER INTUITIVE
//    def contramap[A, B](fa: F[A])(f: B => A): F[B]
//  }
//  trait Invariant[F[_]] {
//    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
//  }
  // summon instances of Contravariant using the Contravariant.apply method
  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
  showSymbol.show(Symbol("amina"))
  // more conveniently we can use the extension method
  import cats.syntax.contravariant._ // for contramap
  showString.contramap[Symbol](sym => s"'${sym.name}").show(Symbol("amina"))

  //3.7.2 Invariant in Cats
  // Cats provides an instance of Invariant for Monoid
  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }
  // imagine we want to produce a Monoid for Scala's Symbol type
  // Cats doesn't provide this, but it does have one for a similar type, String
  // page 69 *** go through this with Tim **** 3.7.2

  //3.8 Partial Unification *** please can we discuss the overview to this topic
  //polymorphic function - takes a type parameter, so I can change the types

  //3.9 Summary
  // functors represent sequencing behaviours
  // we looked at 3 types of functors
  // regular COVARIANT FUNCTORS with a map method.
  // successive calls are applied in sequence; accepting the result of its predecessor as a parameter.
  // CONTRAVARIANT FUNCTORS, with their contramap method
  // represent the ability to prepend functions. successive calls sequence in the opposite order of map ******??!!???!!
  // INVARIANT FUNCTORS with imap, bidrectional transformation.
  // regular functors are by far the most common, but even these are rarely used on their own
  // functors provide a foundational block for more interesting abstractions such as monads and applicative functors
  // functors for collections are extremely important as they transform each element indepdently from the rest
  // this allows us to parallelise or distribute transformations on large collections
  // we will revisit Contravariant and Invariant type classes in the Semigroupal typeclass.
}
