package script
import parser.ArffJsonParser
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import scala.collection.mutable
import java.util.{TreeSet => JTreeSet}
import format.arff_json.ArffJsonInstance
import java.util.Comparator
import parser.ArffJsonInstancesFile2
import java.io.File
import filter.OddsRatioFilter
import filter.feature_scoreing.OddsRatio
import classifier.TopClassIs
import filter.VectorFromDictFilter
import filter.ProjectionFilter
import format.arff_json.HistoryItem
import parser.ContentDescription
import parser.ArffJsonInstancesFile
import filter.NominalizeFilter
import filter.FlattenFilter

object Test5 {
    def main(args: Array[String]) {
        println("something")
        val trainset = new ArffJsonInstancesFile(ContentDescription("exp", ContentDescription.TrainSet, List()))
        val testset = new ArffJsonInstancesFile(ContentDescription("exp", ContentDescription.TestSet, List()))
        
        val projectionFilter = new ProjectionFilter(List(1), HistoryItem("proj-abs"))
        
        testset.applyFilter(projectionFilter).save
        
        
        //val vectorFilter = new VectorFromDictFilter.Conf2(HistoryItem("vec-conf2"))
        //vectorFilter.buildDict(trainset.applyFilter(projectionFilter))
        
        // val orFilter = new OddsRatioFilter(trainset, TopClassIs(15), 1.5, val historyAppendix: HistoryItem)
        // val nominalizeFilter = new NominalizeFilter.Conf1(HistoryItem("nominalized"))
        // nominalizeFilter.expandDict(trainset.applyFilter(projectionFilter))
        
        //val mappedInst = trainset.applyFilter(projectionFilter).applyFilter(vectorFilter)
        //val or = new OddsRatio(mappedInst, TopClassIs("15"))
        
        //println(or.rankedFeatureList().takeWhile(d => d._2 > 1.0).size)
    }
}




























