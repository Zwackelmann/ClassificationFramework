package script.formulas

import formulas.index.Index
import java.io.StringReader
import scala.collection.mutable

object ReadIndex {
    def main(args: Array[String]) {
        val indexStr = """{1:[2,3], 2:[], 3:[4,5,6], 4:[], 5:[], 6:[7,8], 7:[], 8:[]}"""
        val document = Map( 1 -> 3.0, 2 -> 1.0, 3 -> 2.0, 4 -> 1.0, 6 -> 1.0, 7 -> 1.0 )
        
        val i = Index(new StringReader(indexStr))
        val reWeightedDocument = i.reweight(document, 0.5)
        
        println(reWeightedDocument)
    }
}











