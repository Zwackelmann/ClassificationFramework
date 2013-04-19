package format.arff_json

import common.Common.escape
import weka.core.Instances
import com.alibaba.fastjson.parser.DefaultJSONParser
import com.alibaba.fastjson.JSONObject
import com.alibaba.fastjson.JSONArray

object ArffJsonHeader {
    def jsonToArffJsonHeader(str: String): ArffJsonHeader = {
        val json = try {
            new DefaultJSONParser(str).parse match {
                case o: JSONObject => o
                case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
            }
        }
        
        jsonToArffJsonHeader(json)
    }
    
    def jsonToArffJsonHeader(obj: JSONObject): ArffJsonHeader = {
        def assertAtt(name: String) = {
            obj.get(name) match {
                case null => throw new RuntimeException("The File does not contain a valid ArffJson header")
                case o => o
            }
        }
        
        val relationName = assertAtt("relation-name").asInstanceOf[String]
        val jsonAttributes = assertAtt("attributes").asInstanceOf[JSONArray]

        val attributes = (for(i <- 0 until jsonAttributes.size()) yield {
            ArffJsonAttribute.jsonToArffJsonAttribute(
                jsonAttributes.get(i).asInstanceOf[JSONObject]
            )
        }).toList
        
        new ArffJsonHeader(relationName, attributes)
    }
}

class ArffJsonHeader(var relationName: String, var attributes: List[ArffJsonAttribute]) {
    def toJson = "{" +
        "\"relation-name\" : \"" + escape(relationName) + "\", " +
        "\"attributes\" : [" + attributes.map(_.toJson).mkString(", ") + "]" +
    "}"
        
    def adeptToInstances(instances: Instances) {
        relationName = instances.relationName()
        attributes = (for(i <- 0 until instances.numAttributes()) yield ArffJsonAttribute(instances.attribute(i))).toList
    }
    
    override def equals(a: Any) = a match {
        case h: ArffJsonHeader => h.relationName == this.relationName && h.attributes == this.attributes
        case _ => false
    }
}








