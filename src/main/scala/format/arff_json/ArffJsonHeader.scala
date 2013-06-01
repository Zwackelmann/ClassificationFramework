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
        def getAtt(name: String) = {
            obj.get(name) match {
                case null => None
                case o => Some(o)
            }
        }
        
        val relationName = getAtt("relation-name")
        val attributes = getAtt("attributes")
        val numAttributes = getAtt("num-attributes")
        
        (relationName, attributes, numAttributes) match {
            case (Some(name: String), Some(jArr: JSONArray), None) => {
                val attributes = (for(i <- 0 until jArr.size()) yield {
                    ArffJsonAttribute.jsonToArffJsonAttribute(
                        jArr.get(i).asInstanceOf[JSONObject]
                    )
                }).toList
                
                ArffJsonHeader(name, attributes)
            }
            
            case (Some(name: String), None, Some(numAtts: Integer)) => {
                ArffJsonHeader(name, numAtts)
            }
            case (a, b, c) => throw new RuntimeException("could not match " + (a, b, c))
        }
    }
    
    def apply(_relationName: String, _attributes: Iterable[ArffJsonAttribute]) = new ArffJsonHeader {
        val relationName = _relationName
        val atts = _attributes.toIndexedSeq
        def attribute(index: Int) = atts(index)
        val attributes = _attributes
        lazy val numAttributes = _attributes.size
        val explicitAttributes = true
        
        def toJson = "{" +
            "\"relation-name\" : \"" + escape(relationName) + "\", " +
            "\"attributes\" : [" + _attributes.map(_.toJson).mkString(", ") + "]" +
        "}"
    }
    
    def apply(_relationName: String, _numAttributes: Int) = new ArffJsonHeader {
        val relationName = _relationName
        val numAttributes = _numAttributes
        def attributes = (0 until numAttributes) map {i => new NumericArffJsonAttribute(i.toString)}
        def attribute(index: Int) = new NumericArffJsonAttribute(index.toString)
        val explicitAttributes = false
        
        def toJson = "{" +
            "\"relation-name\" : \"" + escape(relationName) + "\", " +
            "\"num-attributes\" : " + numAttributes +
        "}"
    }
    
    def apply(_numAttributes: Int): ArffJsonHeader = apply("relation-" + (common.Common.randomStream.take(10).mkString), _numAttributes)
}

trait ArffJsonHeader {
    def relationName: String
    def attributes: Iterable[ArffJsonAttribute]
    def attribute(index: Int): ArffJsonAttribute
    def numAttributes: Int
    def toJson: String
    val explicitAttributes: Boolean
    
    def adeptToInstances(instances: Instances) = {
        ArffJsonHeader(
            instances.relationName(),
            (for(i <- 0 until instances.numAttributes()) yield ArffJsonAttribute(instances.attribute(i))).toList
        )
    }
    
    override def equals(a: Any) = a match {
        case h: ArffJsonHeader => h.relationName == this.relationName && h.attributes == this.attributes
        case _ => false
    }
}








