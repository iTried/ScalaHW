import play.api.libs.json.{Json, JsValue}
import scalaj.http.{HttpResponse, Http}


object DataAnalysis {

  // app on superjob.ru
  val appId = 277
  val secretKey = "v1.r0736a8436d8b72376fd7da040eb6c707f0bbcc9e5b786754da28ff9a18edfc1d1342210a.d21fcdb8521a5298bb2aa3b70723bd3a11e3e63c"

  def vacanciesHH: HttpResponse[String] = Http("https://api.hh.ru/vacancies").asString

  def vacanciesByIdHH(id : String): HttpResponse[String] = Http("https://api.hh.ru/vacancies")
    .param("specializations", id)
    .asString

  def specializationsHH: HttpResponse[String] = Http("https://api.hh.ru/specializations").asString

  def specializationsSuperJobJson: HttpResponse[String] = Http("https://api.superjob.ru/2.0/catalogues/").asString

  def getVacanciesByIdSJ(id : Int) :HttpResponse[String] = Http("https://api.superjob.ru/2.0/vacancies/")
    .header("X-Api-App-Id", secretKey)
    .param("catalogues", id.toString)
    .asString

  def specializationsNamesHH = {
    val json: JsValue = Json.parse(specializationsHH.body)
    val specializations = json \\ "specializations"
    specializations.flatMap(_.\\( "name").map(_.as[String])).toList
  }

  def specializationsNamesSJ = {
    val json: JsValue = Json.parse(specializationsSuperJobJson.body)
    val specializations = json \\ "positions"
    specializations.flatMap( s  =>  s.\\( "title_rus").map(_.as[String]) zip s.\\( "key").map(_.as[Int]) ).toMap
  }

}

object Main extends App {
  //val specNames = DataAnalysis.specializationsNamesHH

  print(DataAnalysis.vacanciesByIdHH("Инвестиции"))

  //print(DataAnalysis.specializationsSuperJobJson)
}