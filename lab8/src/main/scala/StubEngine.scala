import java.io.FileInputStream

import akka.actor.{Actor, Cancellable}
import play.api.libs.json._

import scala.concurrent.duration._
import scala.collection.mutable

class StubEngine(engineName: String) extends Actor {
  val results: mutable.Map[String, List[UnitResponse]] = mutable.Map()

  val stream = new FileInputStream(s"stub_responses/$engineName.json")
  val json: JsValue = Json.parse(stream)
  val response: JsResult[List[EngineResponse]] = Json.fromJson[List[EngineResponse]](json)
  var cancellable: Option[Cancellable] = None

  response match {
    case JsSuccess(engineResponses, _) =>
      for (engineResponse <- engineResponses) {
        results(engineResponse.query) = engineResponse.results
      }
    case JsError(e) =>
      throw new IllegalStateException("Stub JSON error: " + e.toString())
  }

  override def receive: PartialFunction[Any, Unit] = {
    case SearchQuery(query) =>
      import context.dispatcher

      val sleepDuration = if (math.random < 0.5) 0 else 3
      val response = MergedResponse(engineName, results getOrElse (query, List()))

      cancellable = Option(context.system.scheduler.scheduleOnce(sleepDuration seconds, sender, response))
  }

  override def postStop(): Unit = {
    cancellable.foreach(_.cancel())
    cancellable = None
  }
}