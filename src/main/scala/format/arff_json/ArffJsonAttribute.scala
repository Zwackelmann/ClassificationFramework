package format.arff_json

import weka.core.Attribute
import weka.core.FastVector
import net.sf.json.JSONObject
import scala.collection.mutable.ListBuffer
import common.Common.jsonToScalaType
import common.Common.escape
import net.sf.json.JSONNull

object ArffJsonAttribute {
    def apply(attribute: Attribute) = {
        val name = attribute.name
        
        if(attribute.isNumeric()) 
            new NumericArffJsonAttribute(name)
        else if(attribute.isString()) 
            new StringArffJsonAttribute(name)
        else if(attribute.isNominal()) {
            val values = attribute.enumerateValues()
            val buffer = new ListBuffer[String]
            while(values.hasMoreElements()) {
                buffer += (values.nextElement().asInstanceOf[String])
            }
            
            new NominalArffJsonAttribute(name, buffer.toList)
        } else 
            throw new RuntimeException
    }
    
    def jsonToArffJsonAttribute(att: JSONObject) = {
        att.get("type") match {
            case "string" => new StringArffJsonAttribute(att.get("name").asInstanceOf[String])
            case "numeric" => new NumericArffJsonAttribute((att.get("name") match {
                case n: JSONNull => "null"
                case x => x
            }).asInstanceOf[String])
            case "nominal" => new NominalArffJsonAttribute(att.get("name").asInstanceOf[String], jsonToScalaType(att.get("possible-values")).asInstanceOf[List[String]])
            case _ => throw new RuntimeException("Invalid attribute type")
        }
    }
}

abstract class ArffJsonAttribute(val name: String) {
    def toArffAttribute(index: Int) = {
        this match {
            case numAtt: NumericArffJsonAttribute => new Attribute(name)
            case strAtt: StringArffJsonAttribute => new Attribute(name, (null).asInstanceOf[FastVector], index)
            case nomAtt: NominalArffJsonAttribute => {
                val values = new FastVector()
                for(possibleValue <- nomAtt.possibleValues) {
                    values.addElement(possibleValue)
                }
                new Attribute(name, values, index)
            }
            case _ => throw new RuntimeException("No valid attribute type")
        }
    }
    
    def toJson: String
}

class NumericArffJsonAttribute(name: String) extends ArffJsonAttribute(name) {
    def toJson = """{"name" : "%s", "type" : "numeric"}""".format(escape(name))
    override def equals(a: Any) = a match {
        case na: NumericArffJsonAttribute => na.name == this.name
        case _ => false
    }
}
class StringArffJsonAttribute(name: String) extends ArffJsonAttribute(escape(name)) {
    def toJson = """{"name" : "%s", "type" : "string"}""".format(escape(name))
    override def equals(a: Any) = a match {
        case sa: StringArffJsonAttribute => sa.name == this.name
        case _ => false
    }
}
class NominalArffJsonAttribute(name: String, val possibleValues: List[String]) extends ArffJsonAttribute(name) {
    def toJson = """{"name" : "%s", "type" : "nominal", "possible-values" : [%s]}""".format(name, possibleValues.map(v => "\"" + v + "\"").mkString(","))
    override def equals(a: Any) = a match {
        case na: NominalArffJsonAttribute => na.name == this.name
        case _ => false
    }
}




