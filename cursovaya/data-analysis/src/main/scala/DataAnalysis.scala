import play.api.libs.json.{Json, JsValue}
import scalaj.http.{HttpResponse, Http}


object DataAnalysis {

  def vacanciesJson: HttpResponse[String] = Http("https://api.hh.ru/vacancies?specializations=24").asString

  def specializationsJson: HttpResponse[String] = Http("https://api.hh.ru/specializations").asString

  val specializationsNames = {
    val json: JsValue = Json.parse(specializationsJson.body)
    val specializations = json \\ "specializations"
    specializations.flatMap(_.\\( "name").map(_.as[String])).toList
  }
}

object Main extends App {
  val specNames = DataAnalysis.specializationsNames

  print(DataAnalysis.vacanciesJson)


}