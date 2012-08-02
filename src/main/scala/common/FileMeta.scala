package common
import java.io.File
import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.util.parsing.json.JSON._
import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.JSONArray
import FileMeta.HistoryItem

object FileMeta {
    def fromFile(file: File) = {
        val scanner = new Scanner(file)
        val firstLine = scanner.nextLine()

        val jsonMetaLine = """^%\s*meta\s*:\s*(\{.*\})$""".r

        val jsonMap: Option[Map[String, Any]] = firstLine match {
            case jsonMetaLine(json) => parseFull(json).asInstanceOf[Option[Map[String, Any]]]
            case _ => None
        }

        jsonMap match {
            case Some(map) => Some(new FileMeta(
                map("corpus").asInstanceOf[String],
                map("train-target").asInstanceOf[String],
                map("concidered-attributes").asInstanceOf[List[String]],
                map("history").asInstanceOf[List[HistoryItem]]
            ))
            case None => None
        }
    }
    
    object HistoryItem {
        def apply(jsonMap: Map[String, Any]) {
            new HistoryItem(
                jsonMap("algorithm").asInstanceOf[String], 
                jsonMap("params").asInstanceOf[Map[String, String]]
            )
        }
    }
    
    class HistoryItem(val algorithm: String, val params: Map[String, String]) {
        def toJson = new JSONObject(Map(
            "algorithm" -> algorithm,
            "params" -> new JSONObject(params)
        ))
        
        override def toString = "HistoryItem( algorithm: " + algorithm + ", params: " + params + " )"
    }
}

class FileMeta(val corpus: String, val trainTarget: String, val concideredAttributes: List[String], val history: List[HistoryItem]) {
    def toJson = new JSONObject(Map(
        "corpus" -> corpus,
        "train-target" -> trainTarget,
        "concidered-attributes" -> new JSONArray(concideredAttributes),
        "history" -> new JSONArray(history.map(_.toJson))
    ))
    
    def addHistoryItem(item: HistoryItem) = {
        new FileMeta(
            corpus,
            trainTarget,
            concideredAttributes,
            history :+ item
        )
    }
}




