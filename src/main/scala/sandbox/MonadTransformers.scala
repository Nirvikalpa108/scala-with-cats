package sandbox

import cats.Monad
import cats.data.EitherT

import scala.concurrent.ExecutionContext.Implicits.global

object MonadTransformers {
  //what does composition mean in maths?
  //according to google, this is called function composition
  //two functions generate a new function eg. h(x) = g(f(x))

  //for functors and applicatives, you can take stack them up and have a functor or applicative
  //but we can't do that with monads, unless we use OptionT or EitherT and then we can stack them with any monad

  //5.2 A Transformative Example
  // Cats provides transformers for many monads, each named with a T suffix
  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ //for Pure
  //we can create instances using the OptionT constructor, or just by using pure
  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  //5.3.2 Building Monad Stacks
  // All the monad transformers follow the same convention
  // The transformer itself is the inner monad in the stack,
  // whilst the first parameter is the outer (eg List is the parameter, but it's the outer Monad)

  //5.4 Exercise [this is quite a long exercise, so take some time for this]
  import scala.concurrent.Future
  //responses are represented as a stack of monads
  type Response[A] = Future[Either[String, A]]
  type ResponseAgain[A] = EitherT[Future, String, A]
  implicit def futureMonad: Monad[Future] = ???
  def getPowerLevel(autobot: String): ResponseAgain[Int] = {
    val powerLevels = Map(
      "Jazz"      -> 6,
      "Bumblebee" -> 8,
      "Hot Rod"   -> 10
    )
    val result = powerLevels.get(autobot).toRight(s"error $autobot unreachable")
    EitherT(Future(result))
  }

  def canSpecialMove(ally1: String, ally2: String): ResponseAgain[Boolean] = {
    for {
      ally1Power <- getPowerLevel(ally1)
      ally2Power <- getPowerLevel(ally2)
    } yield ally1Power + ally2Power > 15
  }

  def tacticalReport(ally1: String, ally2: String): ResponseAgain[String] = {
    for {
     result <- canSpecialMove(ally1, ally2)
    } yield result match {
      case true => "true"
      case false => "false"
    }
  }

}
