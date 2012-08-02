package model

import net.sf.json.JSONObject
import net.sf.json.JSONArray
import common.DB
import scala.collection.mutable.ListBuffer
import net.sf.json.JSONObject
import net.sf.json.JSONSerializer
import net.sf.json.JSONNull

trait JSONable {
    def toJson: JSONObject
}

object Paper {
    val db = new DB(
        dbms = "postgres",
        host = "localhose",
        user = "postgres",
        pw = "0118999"
    )
    
    def apply(jsonObj: JSONObject) = {
        val an = Pair(jsonObj.get("an1").asInstanceOf[Int], jsonObj.get("an2").asInstanceOf[Int])
        val title = jsonObj.get("title").asInstanceOf[String]
        
        val authorArray = jsonObj.get("authors").asInstanceOf[JSONArray]
        val authors = (for(i <- 0 until authorArray.size) yield Author(authorArray.get(i).asInstanceOf[JSONObject])).toList
        
        val sourceArray = jsonObj.get("sources").asInstanceOf[JSONArray]
        val sources = (for(i <- 0 until sourceArray.size) yield Source(sourceArray.get(i).asInstanceOf[JSONObject])).toList
        
        val mscClassArray = jsonObj.get("mscClasses").asInstanceOf[JSONArray]
        val mscClasses = (for(i <- 0 until mscClassArray.size) yield mscClassArray.get(i).asInstanceOf[String]).toList
        
        val termArray = jsonObj.get("terms").asInstanceOf[JSONArray]
        val terms = (for(i <- 0 until termArray.size) yield termArray.get(i) match {
            case n: JSONNull => "null"
            case s: String => s
        }).toList
        
        val abstractText = jsonObj.get("abstract").asInstanceOf[String]
        
        new Paper(an, title, authors, sources, mscClasses, terms, abstractText)
    }
    
    def find(an: Pair[Int, Int]): Paper = {
        val paperStmt = db.connection.prepareStatement("SELECT * FROM json_paper WHERE paper_an1=? AND paper_an2=?")
        paperStmt.setInt(1, an._1)
        paperStmt.setInt(2, an._2)
        
        val res = paperStmt.executeQuery()
        
        if(res.next()) {
            val jsonObj = JSONSerializer.toJSON(res.getString(3)).asInstanceOf[JSONObject]
            val paper = Paper(jsonObj)
            paper
        } else {
            throw new RuntimeException("The Paper with the id " + an._1 + "." + an._2 + " could not be found")
        }
    }
    
    def find(an: String): Paper = {
        val anParts = an.split('.')
        if(anParts.length != 2) {
            throw new IllegalArgumentException("Invalid an string")
        } else {
            find(Pair(anParts(0).toInt, anParts(1).toInt))
        }
    }
}

class Paper (
	val an: Pair[Int, Int] = null,
	val title: String = null,
	val authors: List[Author] = null,
	val sources: List[Source] = null,
	val mscClasses: List[String] = null,
	val terms: List[String] = null,
	val abstractText: String = null
) extends JSONable {
    def toJson = {
        val paper = new JSONObject()
        paper.put("an1", an._1)
        paper.put("an2", an._2)
        paper.put("title", title)
        
        val authorArray = new JSONArray()
        for(author <- authors) {
            authorArray.add(author.toJson)
        }
        paper.put("authors", authorArray)
        
        val sourceArray = new JSONArray()
        for(source <- sources) {
            sourceArray.add(source.toJson)
        }
        paper.put("sources", sourceArray)
        
        val mscClassArray = new JSONArray()
        for(mscClass <- mscClasses) {
            mscClassArray.add(mscClass)
        }
        paper.put("mscClasses", mscClassArray)
        
        val termArray = new JSONArray()
        for(term <- terms) {
            termArray.add(term)
        }
        paper.put("terms", termArray)
        
        paper.put("abstract", abstractText)
        
        paper
    }
    
    override def toString = "Paper(" + 
        "an: " + an._1 + "." + an._2 + ", " +
        "title: " + (if(title.length > 20) (title.substring(0, 20) + "...") else title) + ", " + 
        "authors: [" + authors.mkString(", ") + "], " + 
        "sources: [" + sources.mkString(", ") + "], " + 
        "mscClasses: [" + mscClasses.mkString(", ") + "], " + 
        "terms: [" + terms.mkString(", ") + "], " + 
        "abstract: " + (if(abstractText.length > 20) (abstractText.substring(0, 20) + "...") else abstractText) + 
    ")"
}





