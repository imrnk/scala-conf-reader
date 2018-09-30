
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.jackson.JsonMethods


case class Sql(dbName : String, query : String)

class QueryPreparation {

  //load conf file - if no filename is passed load the application.conf file
  //or else try to load the passed file name
  def loadConfigFile(filenameOpt : Option[String] = None): Option[Config] =
    Option(filenameOpt.fold(ifEmpty = ConfigFactory.load())(ConfigFactory.load(_)))


  //extract the query file conf names
  def queryFileNames(config : Config) : Option[List[String]] =
    Option(config.getString("task-config.query-files"))
      .flatMap(qfile => Option(qfile.split(",").toList))


  def parseQueryJson(config : Config): Option[JValue] = {
    val json: String = config
      .getValue("task-config")
      .render(ConfigRenderOptions.concise())

    Option(JsonMethods.parse(json))

  }

  def prepareSqlObj (jobj : JValue) : Option[List[Sql]] = {
    val sqls = for {
     JObject(child) <- jobj
     JField("db_name", JString(dbName))  <- child
     JField("sql", JString(sql))<- child
     JField("db_name", JString(s)) <- child
     subDbName = if (s.trim.isEmpty) dbName else s
    } yield Sql(subDbName, sql)
    Option(sqls)
  }
}


object QueryPreparation extends App {

  val qp = new QueryPreparation
  val jobjsO  = qp.loadConfigFile(None).flatMap(qp.queryFileNames)
      .map(fnames => fnames.flatMap(fname => qp.loadConfigFile(Some(fname))))
      .map(configs => configs.flatMap(qp.parseQueryJson))

  val sqlsO: Option[List[Sql]] =  jobjsO
    .map(jobjs => jobjs.flatMap(qp.prepareSqlObj))
    .map(sqlss => sqlss.flatMap(sqls => sqls.flatMap(sql => Option(sql))))

  sqlsO.foreach(print _)

}