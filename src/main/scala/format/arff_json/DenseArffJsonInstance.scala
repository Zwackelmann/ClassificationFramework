package format.arff_json

import common.Common.escape
import net.sf.json.JSONArray
import net.sf.json.JSONObject

class DenseArffJsonInstance(id: String, mscClasses: List[String], val dataList: List[Any]) extends ArffJsonInstance(id, mscClasses) {
    import ArffJsonInstance.dataToJson
    
    def toJson = "[" + 
        "[" + (List(id, mscClasses).map(dataToJson(_))).mkString(",") + "]," + 
        "[" + (dataList.map(dataToJson(_))).mkString(",") + "]" + 
    "]"
        
    
    def dataAt(index: Int) = dataList(index)
    def numAttributes() = dataList.size
    override def toString = toJson
    def data = dataList.asInstanceOf[List[Double]]
    def sparseData = (0 until dataList.length).zip(dataList.asInstanceOf[List[Double]]).filter(_._2 != 0).toMap
}
