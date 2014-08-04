package model

import com.google.gson.JsonObject

object Author {
    val validChars = List(
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', ' '
    )
    
    def unifyName(name: String) = {
        name.toLowerCase.filter(c => validChars.contains(c))
    }
    
    def apply(jsonObj: JsonObject) = {
        jsonObj.get("type").asInstanceOf[String] match {
            case "HumanAuthor" => new HumanAuthor(
                jsonObj.get("firstname").asInstanceOf[String],
                jsonObj.get("lastname").asInstanceOf[String]
            )
            case "OtherAuthor" => new OtherAuthor(
                jsonObj.get("name").asInstanceOf[String]
            )
        }
    }
}
abstract class Author extends JSONable {
    def unifiedName: String
}
case class HumanAuthor(val firstname: String, val lastname: String) extends Author {
    import Author.unifyName
    
    override def toString = "Author(" + lastname + ", " + firstname + ")"
    def unifiedName = unifyName(lastname) + ", " + unifyName(firstname)
    
    def toJson = {
        val author = new JsonObject()
        author.addProperty("type", "HumanAuthor")
        author.addProperty("firstname", firstname)
        author.addProperty("lastname", lastname)
        
        author
    }
}
case class OtherAuthor(val name: String) extends Author {
    import Author.unifyName
    def unifiedName = unifyName(name)
    
    def toJson = {
        val author = new JsonObject()
        author.addProperty("type", "OtherAuthor")
        author.addProperty("name", "name")
        
        author
    }
}