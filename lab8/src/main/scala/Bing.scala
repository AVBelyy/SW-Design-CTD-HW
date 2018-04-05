import akka.actor.Props

object Bing {
  def props: Props = Props[Bing]
}

final class Bing extends StubEngine("bing")