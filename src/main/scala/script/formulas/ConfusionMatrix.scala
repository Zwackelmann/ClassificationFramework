package script.formulas

import classifier.CategoryIs
import parser.ArffJsonInstancesSource
import common.TrainTuningTestSetSelection
import common.FileManager
import script.SvmLightJniLearner
import parser.History
import filter.TfIdfFilter
import classifier.BalancedTrainSetSelection
import model.RawClassification
import scala.collection.mutable
import mutable.{HashMap, ListBuffer}
import classifier.Learner
import script.AbstractTitleConcat
import filter.CategorySelectionFilter
import filter.VectorFromDictFilter
import filter.NormalizeVectorFilter
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.alg._
import java.io.File
import scala.collection.JavaConversions._
import classifier.CategoryIsMSC

object ConfusionMatrix {
    object ConfusionMetadata {
        def apply(trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, learner: Learner, categories: List[CategoryIsMSC]) = {
            val _documentClassificationsMap = new HashMap[String, (ListBuffer[String], ListBuffer[String])] {
                override def default(key: String) = (new ListBuffer[String], new ListBuffer[String])
            }
            
            for(cat <- categories) {
                val r = learner.classifications(trainSet, testSet, cat)
                val t = RawClassification.findBestThreshold(r, 1.0)
                val r2 = RawClassification.withThreshold(r, t)
                
                for(c <- r2) {
                    if(c.isPositive) {
                        val x = _documentClassificationsMap(c.id)
                        x._1 += cat.topClass.get
                        _documentClassificationsMap(c.id) = x
                    }
                    if(c.isClassifiedPositive) {
                        val x = _documentClassificationsMap(c.id)
                        x._2 += cat.topClass.get
                        _documentClassificationsMap(c.id) = x
                    }
                }
            }
            
            new ConfusionMetadata() {
                val documentClassificationsMap = _documentClassificationsMap.mapValues(v => (v._1.toList, v._2.toList)).toMap
            }
        }
    }
    
    trait ConfusionMetadata {
        val documentClassificationsMap: Map[String, (List[String], List[String])]
        
        val confusionCache = new HashMap[(String, String), Double]
        /*def confusion(c1: String, c2: String) = confusionCache.getOrElseUpdate((c1, c2), {
            var a = 0
            var b = 0
            for((id, (trueClasses, predictedClasses)) <- documentClassificationsMap) {
                val c1inTrueClasses = trueClasses.contains(c1)
                val c2inTrueClasses = trueClasses.contains(c2)
                val c1inPredictedClasses = predictedClasses.contains(c1)
                val c2inPredictedClasses = predictedClasses.contains(c2)
                
                if(!(c1inTrueClasses && c2inTrueClasses) && (c1inTrueClasses || c2inTrueClasses)) {
                    b += 1
                    
                    if((c1inTrueClasses && !c1inPredictedClasses) || (!c1inTrueClasses && c1inPredictedClasses) || 
                            (c2inTrueClasses && !c2inPredictedClasses) || (!c2inTrueClasses && c2inPredictedClasses)) {
                        a += 1
                    }
                }
            }
            
            a.toDouble / b
        })*/
        
        def confusion(c1: String, c2: String) = confusionCache.getOrElseUpdate((c1, c2), {
            var a = 0
            var b = 0
            for((id, (trueClasses, predictedClasses)) <- documentClassificationsMap) {
                val c1inTrueClasses = trueClasses.contains(c1)
                val c2inTrueClasses = trueClasses.contains(c2)
                val c1inPredictedClasses = predictedClasses.contains(c1)
                val c2inPredictedClasses = predictedClasses.contains(c2)
                
                if(c1inTrueClasses && !c2inTrueClasses) {
                    b += 1
                    
                    if(c2inPredictedClasses) {
                        a += 1
                    }
                }
            }
            
            a.toDouble / b
        })
    }
    
    def main2(args: Array[String]) {
        /*
         *                      (#docs : prediction != real class AND prediction is c1 or c2 AND real class is c1 or c2)
         * Confusion(c1, c2) =  ---------------------------------------------------------------------------------------- 
         *                                         (#docs : prediction is c1 or c2 AND real class is c1 or c2)
         */
        try {
            /*common.Path.rootFolder = "text-math-corpus"
            
            val corpus = ArffJsonInstancesSource("text-math-corpus/arffJson/merged-corpus-discrete.json")
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "merged-descrete", corpus, (0.7, 0.3, 0.0))
            val cats = common.Common.topClasses map (c => CategoryIs.top(c))
            
            val learner = SvmLightJniLearner(
                new History() with TfIdfFilter.Appendix,
                BalancedTrainSetSelection(Some(10000))
            )
            */
            
            common.Path.rootFolder = "data_try_train_set_selection2"
            
            val corpus = ArffJsonInstancesSource("data_try_train_set_selection2/arffJson/corpus.json")
            val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
            val cats = common.Common.topClasses map (c => CategoryIsMSC.top(c))
            
            val learner = SvmLightJniLearner(
                    new History() 
                        with AbstractTitleConcat
                        // with CategorySelectionFilter.Appendix
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        /*with NormalizeVectorFilter.Appendix*/ { 
                    val confName = "conf9"
                    override val minOcc = 3
                    // val selection = CategoryIsMSC(None, None, None)
                },
                BalancedTrainSetSelection(Some(10000))
            )
            
            val confusionMeta = ConfusionMetadata(trainSet, tuningSet, learner, cats)
            val confusionMatrix = (for(c1 <- cats.map(_.topClass.get); c2 <- cats.map(_.topClass.get)) yield {
                (c1, c2) -> confusionMeta.confusion(c1, c2)
            }).toMap
            
            
            for(threshold <- (0.0 to 1.0 by 0.05)) {
                val graph = confusionGraphVariant1(confusionMeta, threshold, cats.map(_.topClass.get))
                common.ObjectToFile.writeObjectToFile(graph, new File("confusion_graphs_variant_1/graph_" + threshold))
                
                val graph2 = confusionGraphVariant2(confusionMeta, threshold, cats.map(_.topClass.get))
                common.ObjectToFile.writeObjectToFile(graph2, new File("confusion_graphs_variant_2/graph_" + threshold))
                
                val graph3 = confusionGraphVariant3(confusionMeta, threshold, cats.map(_.topClass.get))
                common.ObjectToFile.writeObjectToFile(graph3, new File("confusion_graphs_variant_3/graph_" + threshold))
            }
            
            val wg = weightedConfusionGraph(confusionMeta, cats.map(_.topClass.get))
            common.ObjectToFile.writeObjectToFile(wg, new File("confusion_graphs/weighted_confusion_graph"))
            
            println(confusionMatrix.groupBy(_._1._1).toList.sortBy(_._1).map(kv => (kv._1 + "\t" + kv._2.toList.sortBy(_._1._2).map(_._2).mkString("\t"))).mkString("\n"))
            
        } finally {
            FileManager.quit
        }
    }
    
    def main(args: Array[String]) {
        analyzeConfusionGraphs("confusion_graphs_variant_3")
        // main2(args)
    }
    
    def weightedConfusionGraph(confusionMeta: ConfusionMetadata, categories: List[String]) = {
        val g = new SimpleWeightedGraph[String, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
        for(c <- categories) {
            g.addVertex(c)
        }
        
        for(c1 <- categories; c2 <- categories if c1 != c2) {
            val e = g.addEdge(c1, c2)
            if(e != null) {
                println(c1 + ", " + c2 + " : " + e)
                g.setEdgeWeight(e, confusionMeta.confusion(c1, c2))
            } else {
                println(c1 + ", " + c2 + " : null")
            }
        }
        
        g
    }
    
    /*
     * variant1: include an edge if the confusion of c1 to c2 OR c2 to c1 exceeds the threshold 
     */
    def confusionGraphVariant1(confusionMeta: ConfusionMetadata, minConfusionThreshold: Double, categories: List[String]) = {
        confusionGraph(
            ((c1, c2) => (confusionMeta.confusion(c1, c2) > minConfusionThreshold || confusionMeta.confusion(c2, c1) > minConfusionThreshold)), 
            categories
        )
    }
    
    /*
     * variant2: include an edge if the confusion of c1 to c2 AND c2 to c1 exceeds the threshold 
     */
    def confusionGraphVariant2(confusionMeta: ConfusionMetadata, minConfusionThreshold: Double, categories: List[String]) = {
        confusionGraph(
            ((c1, c2) => (confusionMeta.confusion(c1, c2) > minConfusionThreshold && confusionMeta.confusion(c2, c1) > minConfusionThreshold)), 
            categories
        )
    }
    
    /*
     * variant3: include an edge if the SUM OF CONFUSIONS of c1 to c2 and c2 to c1 exceeds the threshold 
     */
    def confusionGraphVariant3(confusionMeta: ConfusionMetadata, minConfusionThreshold: Double, categories: List[String]) = {
        confusionGraph(
            ((c1, c2) => ((confusionMeta.confusion(c1, c2) + confusionMeta.confusion(c2, c1)) > minConfusionThreshold)), 
            categories
        )
    }
    
    def confusionGraph(edgeCondition: (String, String) => Boolean, categories: List[String]) = {
        val g: UndirectedGraph[String, DefaultEdge] = 
            new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])
        
        for(c <- categories) {
            g.addVertex(c)
        }
        
        for(c1 <- categories; c2 <- categories if c1 != c2) {
            if(edgeCondition(c1, c2)) {
                g.addEdge(c1, c2)
            } 
        }
        
        g
    }
    
    def analyzeConfusionGraphs(folderName: String) {
        val confusionGraphsDirectory = new File(folderName)
        
        val fileRe = "graph_([0-9]+\\.[0-9]+)".r
        val graphs = (for(file <- confusionGraphsDirectory.listFiles()) yield {
            file.getName() match {
                case fileRe(threshold) => List((common.ObjectToFile.readObjectFromFile(file).asInstanceOf[UndirectedGraph[String, DefaultEdge]], threshold))
                case _ => List()
            }
        }).flatten
        
        val maximumCliques = (for((graph, t) <- graphs) yield {
            val cliqueFinder = new BronKerboschCliqueFinder(graph)
            val maxCliques = cliqueFinder.getAllMaximalCliques()
            val avgCliqueSize = {
                val x = maxCliques.toList.map(_.size)
                x.reduceLeft(_ + _).toDouble / x.size
            }
            
            (avgCliqueSize, t, maxCliques.toList.map(_.toList.sortBy(s => s)))
        }).toList
        
        println(maximumCliques.mkString("\n"))
        // maximumCliquesToDisjointHighlyConnectedSubgraphs(maximumCliques(11)._3, graphs(11)._1)
    }
    
    def maximumCliquesToDisjointHighlyConnectedSubgraphs(maximumCliques: List[List[String]], graph: SimpleWeightedGraph[String, DefaultWeightedEdge], categories: List[String]) = {
        for(cat <- categories) {
            
        }
    }
}








