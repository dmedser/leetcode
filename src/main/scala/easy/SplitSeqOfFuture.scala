package easy

object SplitSeqOfFuture extends App {
  /* На вход Seq[Future[String]]
  Получить Future[(Seq[String], Seq[Throwable]) - результат агрегации выполненых Future и исключений
   */

  import scala.concurrent.duration.Duration
  import scala.concurrent.{Await, ExecutionContext, Future}

  given ExecutionContext = ExecutionContext.global

  val talk =
    Seq(
      Future {
        Thread.sleep(1000)
        "red"
      },
      Future.failed(new RuntimeException("exception1")),
      Future.successful("blue"),
      Future.failed(new RuntimeException("exception2")),
      Future.successful("green"),
      Future.failed(new RuntimeException("exception3"))
    )

  val resultFuture: Future[(Seq[Throwable], Seq[String])] = {
    Future.sequence {
      talk.map { f =>
        f
          .map(s => Right[Throwable, String](s))
          .recover { case t => Left[Throwable, String](t) }
      }
    }
  }.map { seq =>
    seq.foldLeft((Seq.empty[Throwable], Seq.empty[String])) { case ((ts, ss), tOrS) =>
      tOrS match {
        case Left(t)  => (ts :+ t) -> ss
        case Right(s) => ts        -> (ss :+ s)
      }
    }
  }

  val result = Await.result(resultFuture, Duration.Inf)
  println(result)

}
