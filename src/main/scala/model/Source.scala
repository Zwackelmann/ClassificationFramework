package model

import net.sf.json.JSONObject
import net.sf.json.JSONArray

object Source {
    def apply(jsonObj: JSONObject) = {
        jsonObj.get("type").asInstanceOf[String] match {
            case "DetailledSource" => new DetailledSource(
                jsonObj.get("journal").asInstanceOf[String],
                jsonObj.get("volume").asInstanceOf[Int],
                jsonObj.get("nrFrom").asInstanceOf[Int],
                jsonObj.get("nrTo").asInstanceOf[Int],
                jsonObj.get("pageFrom").asInstanceOf[Int],
                jsonObj.get("pageTo").asInstanceOf[Int],
                jsonObj.get("year").asInstanceOf[Int]
            )
            case "OtherSource" => new OtherSource(
                jsonObj.get("src").asInstanceOf[String]
            )
        }
    }
}

abstract class Source extends JSONable

class DetailledSource(val journal: String, val volume: Int, val nrFrom: Int, val nrTo: Int, val pageFrom: Int, val pageTo: Int, val year: Int) extends Source {
    def toJson = {
        val source = new JSONObject()
        source.put("type", "DetailledSource")
        source.put("journal", journal)
        source.put("volume", volume)
        source.put("nrFrom", nrFrom)
        source.put("nrTo", nrTo)
        source.put("pageFrom", pageFrom)
        source.put("pageTo", pageTo)
        source.put("year", year)
        
        source
    }
    
    override def toString = "DetailledSource(" + journal + 
        (if(nrFrom != -1) " No. " + nrFrom + (if(nrTo != nrFrom) "-" + nrTo else "")) +
        (if(pageFrom != -1) " " + pageFrom + (if(pageTo != pageFrom) "-" + pageTo else "")) + 
        (if(year != -1) " (" + year + ")" else "")
}
case class OtherSource(val src: String) extends Source {
    def toJson = {
        val source = new JSONObject()
        source.put("type", "OtherSource")
        source.put("src", src)
        source
    }
}