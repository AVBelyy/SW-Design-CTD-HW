import akka.actor.Props

object Yandex {
  def props: Props = Props[Yandex]
}

final class Yandex extends StubEngine("yandex")