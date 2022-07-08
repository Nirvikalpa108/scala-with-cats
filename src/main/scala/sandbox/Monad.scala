package sandbox

object Monads {
  //4.1 Monad
  // Monad laws p.81
  // 4.1.2 Exercise Getting Func-y
  trait Monad[F[_]] { self =>
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = self.flatMap(value)(a => pure(func(a)))
  }

  //4.2 Monad in Cats
  import cats.Monad
  import cats.instances.option._

  // cats provides instances for all the monads in the standard library (Option, List, Vector etc)
  val opt1 = Monad[Option].pure(3) //Some(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2)) //Some(5)
  val opt3 = Monad[Option].map(opt2)(a => 100 * a) //Some(500)

  // the syntax for monads comes from three places in cats. Here are the imports:
  // cats.syntax.flatMap provides syntax for flatMap
  // cats.syntax.functor provides syntax for map
  // cats.syntax.applicative provides syntax for pure
  // it's often easier to just import everything in one go from cats.implicits !

  // we can use pure to construct instances of a monad. We often need to specify the type parameter.
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure
  1.pure[Option] //Some(1)
  1.pure[List] //List(1)

  // in order to demonstrate flatMap and map, we'll write a generic function
  // that performs a calculation on parameters that come wrapped in a Monad of the user's choice
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = ???

  //4.3 The Identity Monad
  // 4.3.1 Exercise: Monadic Secret Identities
  type Id[A] = A
  def pure[A](a: A): Id[A] = a // Id[A] is the same as a!
  def map[A, B](as: Id[A])(f: A => B): Id[B] = f(as)
  def flatMap[A, B](as: Id[A])(f: A => Id[B]): Id[B] = f(as)
  // so above we can see that the implementations for map and flatMap are identical.
  // this shows that functors and monads are sequencing type classes

  //4.4 Either
  import cats.syntax.either._
  // using asRight we can make an Either instead of just a Left or Right
  val a = 3.asRight // a: Either[String, Int] = Right(3)
  val b = 4.asRight[String] //b: Either[String, Int] = Right(4)
  // Cats has lots of useful extension methods on Either too
  Either.catchOnly[NumberFormatException]("foo".toInt)
  Either.catchNonFatal(sys.error("Something not great"))
  Either.fromTry(scala.util.Try("foo".toInt))
  Either.fromOption[String, Int](None, "Not greatness")
  1.asRight[String].ensure("Must be positive!")(_ > 0)
  "error".asLeft[Int].recover{ case _: String => -1}
  "error".asLeft[Int].recoverWith{ case _ : String => Right(-1)}
  "foo".asLeft[Int].leftMap(_.reverse) //Left("oof")
  6.asRight[String].bimap(_.reverse, _ * 7) //Right(42)
  "bar".asLeft[Int].bimap(_.reverse, _ * 7) //Left("rab")
  123.asRight[String] //Right(123)
  123.asRight[String].swap //Left(123)

  //***4.4.5 talk through this with tim

  //4.5 Aside: Error Handling and MonadError
  //MonadError is a typeclass, which abstracts over Either-like data types that are used for error handling.
  //It provides extra operations for raising and handling errors.
  trait MonadError[F[_], E] extends Monad[F] {
    //**** please can we talk about these signatures and in particular
    // how I can start knowing what the comments tell me without reading them, but just reading the sigs?

    // Lift an error into the F context (more important)
    // this is essentially the identity function; it's just the same as pure
    def raiseError[A](e: E): F[A]
    // Handle an error, potentially recovering from it (more important)
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    // Handle all errors, recovering from them
    def handleError[A](fa: F[A])(f: E => A): F[A]
    // Test an instance of F, fail if the predicate is not satisfied
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]

    // Here is how we instantiate the typeclass for Either
    import cats.MonadError
    import cats.instances.either._
    type ErrorOr[A] = Either[String, A] //*** why did I only start reading the type keyword when I came to Permutive?
    val monadError = MonadError[ErrorOr, String]
  }

  //4.5.4
  //When passed an age greater than or equal to 18 it should return that value as a success.
  //Otherwise it should return an error represented as an IllegalArgumentException.
  //I did this right :)))
  //**Why do I get a dead code compile error?
//  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable ]): F[Int] = {
//    age match {
//      case a if a >= 18 => me.pure(a)
//      case _ => me.raiseError(throw new IllegalArgumentException)
//    }
//    if (age >= 18) me.pure(age) else me.raiseError(throw new IllegalArgumentException)
//  }

  //4.6 The Eval Monad
  //cats.Eval is a Monad that allows us to abstract over different models of evaluation.

  // eager, memoized - call by value - now
  // lazy, not memoized - call by name - always
  // lazy, memoized - call by need - later
  import cats.Eval
  val x = Eval.now(Math.random)
  val y = Eval.always(Math.random)
  val z = Eval.later(Math.random)
  //We have to call Eval's value method to actually run it's map and flatMap methods.
  // map and flatMap methods are trampolined, which means we can nest calls to map and flatMap without consuming stack frames.
  // this is called stack safety. We can use Eval.defer() for this

  //4.6.5 Exercise: Safer Folding using Eval
  // this is not stack safe, make it so:
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail => fn(head, foldRight(tail, acc)(fn))
    case Nil => acc
  }

  //RIGHT AGAIN - YESSSSS
  def foldRightStackSafe[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) =>  Eval[B]): Eval[B] = as match {
    case head :: tail => Eval.defer(fn(head, foldRightStackSafe(tail, acc)(fn)))
    case Nil => acc
  }

  //****DISCUSS, because this was not intuitive to me. Re-write this.
  // How do we redefine foldRight simply in terms of foldRightEval so that it's stack safe?
  def foldRightImplementedWithStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    foldRightStackSafe(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }
  }.value

  //4.10 Defining Custom Monads (overview)
  // Oliver - understand how Monad is implemented, just understand the design decisions

  // we can define a Monad for a custom type by providing implementations of flatMap, pure and tailRecM.
  //tailRecM is an optimisation used in Cats to limit the amount of stack space consumed by nested flatMap calls.

  //4.10.1 Exercise *** Pair on this as I didn't have time to do it myself

}
