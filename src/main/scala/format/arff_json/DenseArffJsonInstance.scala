package format.arff_json

import common.Common.escape

trait DenseData extends ArffJsonInstance {
    def dataList: List[Any]
    import ArffJsonInstance.dataToJson
    
    def toJson = "[" + 
        "[" + (List(id, categories).map(dataToJson(_))).mkString(",") + "]," + 
        "[" + (dataList.map(dataToJson(_))).mkString(",") + "]" + 
    "]"
    
    def dataAt(index: Int) = dataList(index)
    def numAttributes() = dataList.size
    override def toString = toJson
    def data = dataList.asInstanceOf[List[Double]]
    def sparseData = (0 until dataList.length).zip(dataList.asInstanceOf[List[Double]]).filter(_._2 != 0).toMap
    def numNonZeroValues = dataList.count(_ != 0)
}
