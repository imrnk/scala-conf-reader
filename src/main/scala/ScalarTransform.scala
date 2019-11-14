
/**
  * key,scenario,simulation,t1,t2,t3,t4
  * A,0,t11,t12,t13,t14
  * A,1,t21,t22,t23,t24
  * B,0,p11,p12,p13,p14
  * B,1,p21,p22,p23,p24
  * to
  *
  * A,0,t11
  * A,0,t12
  * A,0,t13
  * A,0,t14
  * A,1,t21
  * ....
  * B,0,p11
  * ...
  * B,1,p24
  */

case class Header(key: String, scenarioHeader: String, tenorNames : Seq[String])
case class ScenarioTenorCombination(security: String, scenario :Int, tenorName: String, tenor : String)
class ScalarTransform {
  def transpose(line : String, header : Header): List[ScenarioTenorCombination] = {
    val tokens = line.split(",").toList
    val security = tokens(0)
    val scenario = tokens(1)
    for(tok <- tokens; if tokens.indexOf(tok) > 1) yield {
      val headerIndex = tokens.indexOf(tok)
      val tenorName = header.tenorNames(headerIndex-2)
      ScenarioTenorCombination(security, scenario.toInt, tenorName, tok)
    }
  }
  def fillHeader(headers : List[String]) : Header = headers match {
    case List(k,s,tenors @ _*) => Header(k,s,tenors)
  }
  def transpose(lines : List[String]) : List[ScenarioTenorCombination] = {
    val firstLine = lines(0)
    val header = fillHeader(firstLine.split(",").toList)
    lines.drop(1).flatMap(transpose(_, header))
  }
  def stringify(scenarioTenorCombi : List[ScenarioTenorCombination]) : String =
    scenarioTenorCombi.map(st => s"${st.security},${st.scenario},${st.tenorName},${st.tenor}").mkString("\n")
}
object test {
  val scalar = new ScalarTransform
  def main(args: Array[String]): Unit = {
    import scalar._
    val lines = List("key,scenario,simulation,20190923,20190924,20190926",
                    "A,0,t11,t12,t13,t14",
                    "A,1,t21,t22,t23,t24",
                    "B,0,p11,p12,p13,p14",
                    "B,1,p21,p22,p23,p24")
    val result = transpose(lines)
   // result.foreach(println)
    assert(result.size == 16)
    val str = stringify(result)
    println(str)

  }
}

