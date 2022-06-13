package sandbox

object Functors {
  // allows us to represent sequences of operations within a context such as List and Option
  // functors on their own aren't that useful, but special cases such as Monads and Applicative Functors, are very common.
  // normally a functor is anything with a map method
  // note that Futures are not a good example of pure functional programming, because they aren't referentially transparent
  // **** Discuss Futures not being functional
  // Futures compute and cache a result and there's no way for us to tweak this behaviour. We get unpredictable results.
  // they start computations immediately, rather than allowing the user to dictate when the programme should run
  // single argument functions are also functors
  //*** I really don't understand pages 52-54 on Functors

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  //Functor Laws
  // 1. identity
  // 2. composition 
  // type constructors and higher-kinded types

}
