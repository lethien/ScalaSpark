
abstract class JSON

case class JSONObj(bindings: Map[String, JSON]) extends JSON
case class JSONSeq(elements: List[JSONObj]) extends JSON
case class JSONStr(str: String) extends JSON
case class JSONNum(num: Double) extends JSON
case class JSONBool(b: Boolean) extends JSON
case object JSONNil extends JSON

def show(json: JSON): String = json match {
  case JSONSeq(elems) => "[" + (elems map show mkString ", ") + "]"
  case JSONObj(bindings) => {
    val assocs = bindings map {
      case (key, value) => "\"" + key + "\": " + show(value)
    }
    "{" + (assocs mkString ", " ) + "}"
  }
  case JSONStr(str) => "\"" + str + "\""
  case JSONNum(num) => num.toString
  case JSONBool(b) => b.toString
  case JSONNil => "null"
}

val data = new JSONObj(Map(
  "firstname" -> new JSONStr("John"),
  "lastname" -> new JSONStr("Bacon"),
  "address" -> new JSONObj(Map(
    "streetaddress" -> new JSONStr("123 50 Ave"),
    "state" -> new JSONStr("OR"),
    "postal" -> new JSONNum(12358453)
  )),
  "contacts" -> new JSONSeq(List(
    new JSONObj(Map(
      "phone" -> new JSONStr("123-888-5269")
    )),
    new JSONObj(Map(
      "email" -> new JSONStr("jBacon@comp.com")
    ))
  ))
))

println(show(data))