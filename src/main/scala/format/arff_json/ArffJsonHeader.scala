package format.arff_json

import net.sf.json.JSONObject
import net.sf.json.JSONArray
import common.Common.escape
import weka.core.Instances

object ArffJsonHeader {
    def jsonToArffJsonHeader(obj: JSONObject) = {
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
}








