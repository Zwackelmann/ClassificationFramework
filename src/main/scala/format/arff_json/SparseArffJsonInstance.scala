package format.arff_json

import common.Common.escape

class SparseArffJsonInstance(id: String, mscClasses: List[String], val dataMap: Map[Int, Any], _numAttributes: Int) extends ArffJsonInstance(id, mscClasses) {
    import ArffJsonInstance.dataToJson
    
    def toJson = 
        "[" + 
            "[" + 
                (List(id, mscClasses).map(dataToJson(_))).mkString(",") + 
            "],{" + 
                (dataMap.toList.sortBy(_._1).map(x => "\"" + x._1 + "\":" + (dataToJson(x._2))).mkString(",")) + 
            "}" +
        "]"
            
    def dataAt(index: Int) = dataMap.getOrElse(index, 0.0)
    def numAttributes() = _numAttributes
    
    override def toString = toJson
    
    def data = dataMap.asInstanceOf[Map[Int, Double]].toList.sortBy(_._1).map(_._2)
    def sparseData = dataMap.asInstanceOf[Map[Int, Double]]
}