package io.constellationnetwork.metagraph_sdk.json_logic

import cats.{Applicative, FlatMap, Id, Monad}

trait ResultContext[Result[_]] {
  def pure[A](value: A): Result[A]

  def map[A, B](ra: Result[A])(f: A => B): Result[B]

  def flatMap[A, B](ra: Result[A])(f: A => Result[B]): Result[B]

  def sequence[A](results: List[Result[A]]): Result[List[A]]
}

object ResultContext {
  def apply[Result[_]](implicit ev: ResultContext[Result]): ResultContext[Result] = ev

  implicit def resultContextMonad[Result[_]](implicit RC: ResultContext[Result]): Monad[Result] =
    new Monad[Result] {
      def pure[A](x: A): Result[A] = RC.pure(x)

      def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = RC.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Result[Either[A, B]]): Result[B] = {
        // Default stack-unsafe implementation - subclasses can override
        flatMap(f(a)) {
          case Left(a1) => tailRecM(a1)(f)
          case Right(b) => pure(b)
        }
      }
    }

  implicit def resultContextFlatMap[Result[_]](implicit RC: ResultContext[Result]): FlatMap[Result] =
    resultContextMonad[Result]

  implicit def resultContextApplicative[Result[_]](implicit RC: ResultContext[Result]): Applicative[Result] =
    resultContextMonad[Result]

  implicit val idContext: ResultContext[Id] = new ResultContext[Id] {
    def pure[A](value: A): A = value

    def map[A, B](ra: A)(f: A => B): B = f(ra)

    def flatMap[A, B](ra: A)(f: A => B): B = f(ra)

    def sequence[A](results: List[A]): List[A] = results
  }

  type WithGas[A] = (A, GasMetrics)

  implicit val gasContext: ResultContext[WithGas] = new ResultContext[WithGas] {
    def pure[A](value: A): (A, GasMetrics) = (value, GasMetrics.zero)

    def map[A, B](ra: (A, GasMetrics))(f: A => B): (B, GasMetrics) = {
      val (a, metrics) = ra
      (f(a), metrics)
    }

    def flatMap[A, B](ra: (A, GasMetrics))(f: A => (B, GasMetrics)): (B, GasMetrics) = {
      val (a, metrics1) = ra
      val (b, metrics2) = f(a)
      (b, metrics1.combine(metrics2))
    }

    def sequence[A](results: List[(A, GasMetrics)]): (List[A], GasMetrics) = {
      val (values, metrics) = results.unzip
      (values, metrics.foldLeft(GasMetrics.zero)(_.combine(_)))
    }
  }
}