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
import classifier.CategoryIsMsc
import evaluation.ConfusionMetadata
import evaluation.ConfusionMatrix
import javax.imageio.ImageIO
import classifier.CategoryIsMscSome

object CreateConfusionMatrix {
    def main3(args: Array[String]) = try {
        common.Path.rootFolder = "data_try_train_set_selection2"
        val corpus = ArffJsonInstancesSource("data_try_train_set_selection2/arffJson/corpus.json")
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
        val cats = common.Common.topClasses map (c => CategoryIsMscSome.top(c))
        
        val learner = SvmLightJniLearner(
                    new History() 
                        with AbstractTitleConcat
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix {
                    val confName = "conf9"
                    override val minOcc = 3
                },
                BalancedTrainSetSelection(Some(10000))
            )
            
        val confusionMeta = ConfusionMetadata(trainSet, tuningSet, learner, cats)
        
        val configs = for(
            hb <- List(0.25, 0.30, 0.35, 0.40, 0.45, 0.50); 
            (_colors, _colorSchema) <- List(
                List((111.0, 185.0, 0.0), (255.0, 255.0, 0.0), (255.0, 0.0, 0.0)),
                List((0.0, 128.0, 0.0), (255.0, 255.0, 0.0), (255.0, 0.0, 0.0)),  
                List((255.0, 255.0, 255.0), (127.0, 127.0, 127.0), (0.0, 0.0, 0.0))
            ).zipWithIndex
        ) yield {
            new ConfusionMatrix.ConfusionMatrixImageConfig() {
                val cellWidth = 4.0
                val cellHeight = 4.0

                val lowBound = 0.0
                val highBound = hb
                
                val colors = _colors
                
                val colorSchema = _colorSchema
            }
        }
        
        for(conf <- configs) {
            val image = ConfusionMatrix.confusionToImage(confusionMeta, cats, conf)
            ImageIO.write(image, "jpg", new File("confusionMatrixColor2-" + conf.colorSchema + "-" + conf.highBound + ".jpg"))
        }
        
        println(ConfusionMatrix.confusionToExcelString(confusionMeta, cats))
        
    } finally {
        FileManager.quit
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
            val cats = common.Common.topClasses map (c => CategoryIsMscSome.top(c))
            
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
        // analyzeConfusionGraphs("confusion_graphs_variant_3")
        main3(args)
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








