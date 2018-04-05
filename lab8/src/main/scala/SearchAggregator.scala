import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._

object SearchAggregator extends App {
  implicit val timeout: Timeout = 5 second

  val system: ActorSystem = ActorSystem("search-aggregator")

  var continue = true
  while (continue) {
    val supervisor = system.actorOf(Supervisor.props)
    print("query> ")
    val query = scala.io.StdIn.readLine()
    val future = (supervisor ? SearchQuery(query)).mapTo[List[MergedResponse]]
    val t0 = System.nanoTime()
    val enginesResults = Await.result(future, timeout.duration)
    val t1 = System.nanoTime()
    val runningTime = (t1 - t0) / 1e9
    println(f"(response took $runningTime%.3f seconds)")

    for (MergedResponse(engine, results) <- enginesResults) {
      println(engine.capitalize)
      for (UnitResponse(title, url) <- results) {
        println(s"  | $title\n  | $url\n  |")
      }
    }
  }
}