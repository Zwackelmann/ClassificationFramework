package script
import filter.VectorFromDictFilter
import parser.ArffJsonInstancesFile
import java.io.File
import filter.feature_scoreing.FeatureScoreing
import filter.feature_scoreing.OddsRatio
import external.JoachimsSVMLearnApplier
import external.JoachimsSVMClassifyApplier
import java.io.BufferedReader
import java.io.FileReader
import common.Common.FileConversion._
import model.RawClassification
import format.arff_json.ArffJsonHeader

object Test3 {
    def main(args: Array[String]) {
        val inst = new ArffJsonInstancesFile("final", "train", List())
        
        val targetInst = inst.map(inst => inst.flatMap(elem => 
                if(elem.mscClasses.exists(_.substring(0,2) == "15")) List(elem) else List()
            ),
            (header: ArffJsonHeader) => header,
            "mapped"
        )
        
        val numInst = targetInst.numInstances
        
        
    }
}










