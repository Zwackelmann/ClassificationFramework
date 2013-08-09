package script

import common.Path
import parser.ArffJsonInstancesSource
import common.FileManager
import com.alibaba.fastjson.parser.DefaultJSONParser
import com.alibaba.fastjson.JSONArray
import scala.io.Source
import parser.History
import classifier.BalancedTrainSetSelection

object Test {
    
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource("C:/Users/Simon/Projekte/javascala/workspace/ClassificationFramework/confidenceSpaceCorpus.json")
        println(corpus.numInstances)
        
        val l = SvmLightJniLearner(
            new History() {},
            BalancedTrainSetSelection(Some(10000))
        )
        
        
    }
}




















