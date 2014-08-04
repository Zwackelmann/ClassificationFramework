package model

import common.DB
import scala.collection.mutable.ListBuffer
import com.google.gson.JsonObject
import com.google.gson.JsonArray
import com.google.gson.JsonPrimitive
import com.google.gson.JsonParser
import com.google.gson.JsonNull

trait JSONable {
    def toJson: JsonObject
}

object Paper {
    val db = new DB(
        dbms = "postgres",
        host = "localhose",
        user = "postgres",
        pw = "0118999"
    )
    
    def apply(jsonObj: JsonObject) = {
        val an = Pair(jsonObj.get("an1").asInstanceOf[Int], jsonObj.get("an2").asInstanceOf[Int])
        val title = jsonObj.get("title").asInstanceOf[String]
        
        val authorArray = jsonObj.get("authors").asInstanceOf[JsonArray]
        val authors = (for(i <- 0 until authorArray.size) yield Author(authorArray.get(i).asInstanceOf[JsonObject])).toList
        
        val sourceArray = jsonObj.get("sources").asInstanceOf[JsonArray]
        val sources = (for(i <- 0 until sourceArray.size) yield Source(sourceArray.get(i).asInstanceOf[JsonObject])).toList
        
        val mscClassArray = jsonObj.get("mscClasses").asInstanceOf[JsonArray]
        val mscClasses = (for(i <- 0 until mscClassArray.size) yield mscClassArray.get(i).asInstanceOf[String]).toList
        
        val termArray = jsonObj.get("terms").asInstanceOf[JsonArray]
        val terms = (for(i <- 0 until termArray.size) yield {
            val term = termArray.get(i)
            
            if(term.isJsonNull()) "null"
            else term.asInstanceOf[JsonPrimitive].getAsString()
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
            val jsonObj = new JsonParser().parse(res.getString(3)).asInstanceOf[JsonObject]
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
        val paper = new JsonObject()
        paper.addProperty("an1", an._1)
        paper.addProperty("an2", an._2)
        paper.addProperty("title", title)
        
        val authorArray = new JsonArray()
        for(author <- authors) {
            authorArray.add(author.toJson)
        }
        paper.add("authors", authorArray)
        
        val sourceArray = new JsonArray()
        for(source <- sources) {
            sourceArray.add(source.toJson)
        }
        paper.add("sources", sourceArray)
        
        val mscClassArray = new JsonArray()
        for(mscClass <- mscClasses) {
            mscClassArray.add(new JsonPrimitive(mscClass))
        }
        paper.add("mscClasses", mscClassArray)
        
        val termArray = new JsonArray()
        for(term <- terms) {
            termArray.add(new JsonPrimitive(term))
        }
        paper.add("terms", termArray)
        
        paper.addProperty("abstract", abstractText)
        
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





