import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}
import akka.util.Timeout

import scala.concurrent.duration._

object Supervisor {
  def props: Props = Props[Supervisor]
}

class Supervisor extends Actor {
  val props = List(Bing.props, Google.props, Yandex.props)
  var client: ActorRef = _
  var children: List[ActorRef] = _
  var results: List[MergedResponse] = List()

  val timeout: Timeout = 1 second

  override def receive: Receive = {
    case q @ SearchQuery(_) =>
      client = sender

      children = props.map { context.actorOf }
      children.foreach {
        (x: ActorRef) => {
          x ! q
        }
      }
      context.setReceiveTimeout(timeout.duration)

    case r @ MergedResponse(_, _) =>
      results = results :+ r
      if (results.length == props.length) {
        replyAndStop()
      }

    case ReceiveTimeout =>
      replyAndStop()
  }

  def replyAndStop(): Unit = {
    context.setReceiveTimeout(Duration.Undefined)
    client ! results
    context.stop(self)
  }
}