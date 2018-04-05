import play.api.libs.json.{Json, OFormat}

final case class SearchQuery(query: String)

final case class UnitResponse(title: String, url: String)

final case class MergedResponse(engineName: String, results: List[UnitResponse])

final case class EngineResponse(query: String, results: List[UnitResponse])

object UnitResponse {
  implicit val unitResponseFormat: OFormat[UnitResponse] = Json.format[UnitResponse]
}

object EngineResponse {
  implicit val engineResponseFormat: OFormat[EngineResponse] = Json.format[EngineResponse]
}