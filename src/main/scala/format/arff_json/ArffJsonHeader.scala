package format.arff_json

import common.Common.escape
import weka.core.Instances
import com.google.gson.JsonObject
import com.google.gson.JsonArray
import com.google.gson.JsonParser
import com.google.gson.JsonPrimitive
import common.Gson
import com.google.gson.reflect.TypeToken

object ArffJsonHeader {
    def apply(_relationName: String, _attributes: Iterable[ArffJsonAttribute]) = new ArffJsonHeader {
        val relationName = _relationName
        val atts = _attributes.toIndexedSeq
        def attribute(index: Int) = atts(index)
        val attributes = _attributes
        lazy val numAttributes = _attributes.size
        val explicitAttributes = true
    }
    
    def apply(_relationName: String, _numAttributes: Int) = new ArffJsonHeader {
        val relationName = _relationName
        val numAttributes = _numAttributes
        def attributes = (0 until numAttributes) map {i => new NumericArffJsonAttribute(i.toString)}
        def attribute(index: Int) = new NumericArffJsonAttribute(index.toString)
        val explicitAttributes = false
    }
    
    def apply(_numAttributes: Int): ArffJsonHeader = apply("relation-" + (common.Common.randomStream.take(10).mkString), _numAttributes)
}

trait ArffJsonHeader {
    def relationName: String
    def attributes: Iterable[ArffJsonAttribute]
    def attribute(index: Int): ArffJsonAttribute
    def numAttributes: Int
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








