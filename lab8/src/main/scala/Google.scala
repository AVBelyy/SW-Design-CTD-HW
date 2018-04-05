import akka.actor.Props

object Google {
  def props: Props = Props[Google]
}

final class Google extends StubEngine("google")