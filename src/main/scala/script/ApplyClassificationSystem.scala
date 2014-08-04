package script

import classifier.CategoryIs
import classifier.CategorizationHierarchy
import parser.History
import filter.CategorySelectionFilter
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.BalancedTrainSetSelection
import format.arff_json.ArffJsonHeader
import common.TrainTuningTestSetSelection
import parser.ArffJsonInstancesSource
import classifier.CategoryIsMscSome
import classifier.ClassificationSystem
import script.experiment.JaccardMatrix
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import common.Gson

object ApplyClassificationSystem {
    def main(args: Array[String]) {
        common.Path.rootFolder = "data"
        val corpus = ArffJsonInstancesSource(common.Path.rootFolder + "/arffJson/corpus.json")
        val minOccurences = 100
        
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(minOccurences, "prod", corpus, (0.7, 0.3, 0.0))
        val header = Gson.fromJson("""{"relation-name" : "final_format", "attributes" : [{"name" : "title", "type" : "string"}, {"name" : "abstract", "type" : "string"}, {"name" : "journals", "type" : "string"}, {"name" : "terms", "type" : "string"}]}""", classOf[ArffJsonHeader])
        val catsForLevel = List(1, 2, 3).map(i => consideredCategories(corpus, i, minOccurences))
        
        val learnerForLevel = (level: Int) => {
            level match {
                case 0 => 
                    (cat: (CategoryIs with CategorizationHierarchy)) => SvmLightJniLearner(
                            new History() 
                                with AbstractTitleConcat
                                with CategorySelectionFilter.Appendix
                                with VectorFromDictFilter.Appendix
                                with TfIdfFilter.Appendix
                                with NormalizeVectorFilter.Appendix { 
                            val confName = "conf9"
                            val selection = cat.parent
                            override val minOcc = 3
                        },
                        BalancedTrainSetSelection(Some(10000))
                    )
                    
                case 1 | 2 => 
                    (cat: (CategoryIs with CategorizationHierarchy)) => SvmLightJniLearner(
                            new History() 
                                with AbstractTitleConcat
                                with CategorySelectionFilter.Appendix
                                with VectorFromDictFilter.Appendix
                                with TfIdfFilter.Appendix
                                with NormalizeVectorFilter.Appendix { 
                            val confName = "conf9"
                            val selection = cat.parent
                            override val minOcc = 1
                        },
                        BalancedTrainSetSelection(Some(10000))
                    )
            }
        }
        
        val clSys = ClassificationSystem(
            trainSet,
            tuningSet,
            header,
            learnerForLevel,
            (i: Int) => consideredCategories(corpus, 1, minOccurences),
            0,
            0.0
        )
        
        val fileWriter = new BufferedWriter(new FileWriter(new File("annotated-documents.json")))
        val normedJaccardMatrix = JaccardMatrix.loadMatrixFromFile(new File("jaccard_matrix2"))
        val mainClassThreshold = 0.90
        val secondaryClassPosThreshold = 0.7
        val secondaryClassNegThreshold = 0.3
        val secondaryClassJaccardThreshold = 0.02
        
        for(cls <- common.Common.topClasses) {
            val jaccardValues = normedJaccardMatrix(cls)
            println(cls)
            println(jaccardValues.iterator.toList.sortBy(_._2).reverse)
            println(filterJaccardListWithFlatThreshold(jaccardValues.toList.sortBy(_._2).reverse, secondaryClassJaccardThreshold))
            
            println()
        }
        
        for(inst <- corpus.iterator) {
            val classifications = clSys.classify(inst).map(x => (x._1.substring(3, 5), x._2)).toMap
            
            if(classifications.maxBy(_._2)._2 > mainClassThreshold) {
                val mainClass = classifications.toList.maxBy(_._2)._1
                val trueClasses = inst.categories
                
                val possibleSecondaryClasses = {
                    val jaccardValues = normedJaccardMatrix(mainClass)
                    filterJaccardListWithFlatThreshold(jaccardValues.toList.sortBy(_._2).reverse, secondaryClassJaccardThreshold)
                }
                
                println("instance: " + inst.id)
                println("main class: " + mainClass)
                println("poss. 2nd classes: " + possibleSecondaryClasses.sortBy(s => s))
                println("positive classes: " + classifications.toList.tail.filter(_._2 >= 0.5).sortBy(_._2).reverse)
                println("negative classes: " + classifications.toList.tail.filter(_._2 < 0.5).sortBy(_._2).reverse)
                println("true classes: " + trueClasses)
                println()
                
                if(possibleSecondaryClasses.toList.forall(cls => classifications.contains(cls) && (classifications(cls)>=secondaryClassPosThreshold || classifications(cls)<secondaryClassNegThreshold))) {
                    val secondaryClasses = possibleSecondaryClasses.filter(cls => classifications(cls) >= secondaryClassPosThreshold).sortBy(s => s)
                    println("ANNOTATED")
                    println("main: " + mainClass)
                    println("secondary: " + secondaryClasses.mkString)
                    
                    fileWriter.write(inst.id + ";" + mainClass + ";" + secondaryClasses.mkString(",") + ";" + trueClasses.map(s => s.substring(0, 2)).distinct.mkString(",") + "\n")
                    fileWriter.flush()
                }
                println()
                println()
            }
        }
        
        fileWriter.close()
    }
    
    def filterJaccardListByMaxTotalError(l: List[(String, Double)], maxTotalError: Double) = {
        val indSeq = l.sortBy(_._2).reverse.toIndexedSeq
        
        var ind = l.size-1
        var errorSum = 0.0
        while(ind > 0 && errorSum < maxTotalError) {
            errorSum += indSeq(ind)._2
            ind -= 1
        }
        
        val numEntries = math.min(l.size-1, ind+1)
        
        indSeq.take(numEntries).tail.map(_._1)
    }
    
    def filterJaccardListWithFlatThreshold(l: List[(String, Double)], threshold: Double) = {
        l.filter(_._2 > threshold).sortBy(_._2).reverse.tail.map(_._1).toList
    }
    
    def consideredCategories(corpus: ArffJsonInstancesSource, level: Int, minOccurences: Int) = {
        val (categoryCondition, categorySubstring, categoryMapping, independenceGrouping) = level match {
            case 1 => (
                ((c: String) => true), 
                ((c: String) => c.substring(0, 2)), 
                ((c: String) => CategoryIsMscSome.top(c.substring(0, 2))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 2 => (
                ((c: String) => c.substring(2, 3) != "-"), 
                ((c: String) => c.substring(0, 3)), 
                ((c: String) => CategoryIsMscSome.topAndMiddle(c.substring(0, 2), c.substring(2, 3))),
                ((c: String) => c.substring(0, 2))
            )
            
            case 3 => (
                ((c: String) => c.substring(2, 3) != "-" && c.substring(3, 5).toLowerCase != "xx"), 
                ((c: String) => c), 
                ((c: String) => CategoryIsMscSome.topMiddleAndLeave(c.substring(0, 2), c.substring(2, 3), c.substring(3, 5))),
                ((c: String) => c.substring(0, 3))
            )
        }
        
        val consideredCategories = ApplyFinalClassifier.findConsideredCategories(corpus, categoryCondition, categorySubstring, minOccurences)
        consideredCategories.map(c => categoryMapping(c))
    }
}




































