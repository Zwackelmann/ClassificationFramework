package script

import parser.ArffJsonInstancesSource
import java.io.File
import parser.History
import filter.CategorySelectionFilter
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.NoTrainSetSelection
import common.TrainTuningTestSetSelection
import classifier.CategoryIs
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import classifier.Learner
import parser.ContentDescribable
import classifier.Classifier
import model.CertaintyToThresholdFunction
import classifier.BalancedTrainSetSelection
import scala.collection.mutable
import filter.Filter
import filter.SelectionFilter
import classifier.CategoryIsMsc
import classifier.CategorizationHierarchy
import classifier.CategoryIsMscSome
import common.Gson

object AllClassesForDocument {
    def main(args: Array[String]) {
        common.Path.rootFolder = "data_try_train_set_selection"
        val toolset = AllClassesForDocument(0)
        val x = toolset.findCategories("geometric proof of Jungs theorem on factorisation of automorphisms", "theory of algebraic surfaces theorem on factorisation of automorphisms simple combinatorial argument birational transformations")
        println(x)
    }
    
    val loadedFilters = new mutable.HashMap[(String, String), Filter]
    def mapperCached(learner: Learner, trainSet: ArffJsonInstancesSource, cat: CategoryIs) = {
        val necessaryFilters = learner.filters(trainSet, cat).filter(f => !f.isInstanceOf[SelectionFilter])
        
        val f2 = for(filter <- necessaryFilters) yield {
            filter.trainingParams match {
                case Some(trainingParams) => loadedFilters.getOrElseUpdate(trainingParams, filter)
                case None => filter
            }
        }
        
        (base: ArffJsonInstancesSource) => {
            ((base /: f2)((oldBase, f) => f.applyFilter(oldBase, cat)))
        }
    }
    
    def apply(depth: Int) = { // depth examples: 0 => only first layer, 2 => down to the third layer
        val corpus = ArffJsonInstancesSource(common.Path.rootFolder + "/arffJson/corpus.json")
        
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
        val minOccurences = 100
        
        val catsForLevel = List(1, 2, 3).map(i => {
            val x = consideredCategories(corpus, i, minOccurences)
            if(i == 1) {
                x
            } else {
                x
            }
        })
        
        val toolsets = categoryToolsets(
            ((layer, cat) => layer match {
                case 0 => 
                    SvmLightJniLearner(
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
                    SvmLightJniLearner(
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
            }),
            CategoryIsMscSome(None, None, None),
            catsForLevel,
            trainSet,
            tuningSet,
            0,
            depth
        )
        
        val header = Gson.fromJson("""{"relation-name" : "final_format", "attributes" : [{"name" : "title", "type" : "string"}, {"name" : "abstract", "type" : "string"}, {"name" : "journals", "type" : "string"}, {"name" : "terms", "type" : "string"}]}""", classOf[ArffJsonHeader])
        new AllClassesForDocument(toolsets, header)
    }

    def categoryToolsets(
        learnerFun: ((Int, CategoryIs with CategorizationHierarchy) => Learner),
        parentCat: CategoryIs with CategorizationHierarchy,
        catsForLevel: List[List[CategoryIs with CategorizationHierarchy]],
        trainSet: ArffJsonInstancesSource with ContentDescribable,
        tuningSet: ArffJsonInstancesSource with ContentDescribable,
        layer: Int,
        depth: Int
    ): Map[CategoryIs with CategorizationHierarchy, CategoryToolset] = {
        println(parentCat)
        (for(cat <- catsForLevel(layer).filter(cat => cat.parent.filenameExtension == parentCat.filenameExtension)) yield {
            println(cat.filenameExtension + ", " + layer)
            val learner = learnerFun(layer, cat)
            val mapper = mapperCached(learner, trainSet, cat)
            val classifier = learner.classifier(trainSet, cat)
            
            val children = if(layer < depth) {
                categoryToolsets(learnerFun, cat, catsForLevel, trainSet, tuningSet, layer+1, depth)
            } else {
                Map[CategoryIs with CategorizationHierarchy, CategoryToolset]()
            }
            
            cat -> CategoryToolset(mapper, classifier, CertaintyToThresholdFunction(classifier, trainSet, tuningSet), children)
        }).toMap
    }

    def consideredCategories(corpus: ArffJsonInstancesSource, layer: Int, minOccurences: Int) = {
        val (categoryCondition, categorySubstring, categoryMapping, independenceGrouping) = layer match {
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
    
    case class CategoryToolset(
        val mapper: (ArffJsonInstancesSource => ArffJsonInstancesSource), 
        val classifier: Classifier, 
        val thresholdFunction: CertaintyToThresholdFunction, 
        val children: Map[CategoryIs with CategorizationHierarchy, CategoryToolset]
    )
}

class AllClassesForDocument(toolset: Map[CategoryIs with CategorizationHierarchy, AllClassesForDocument.CategoryToolset], header: ArffJsonHeader) {
    def findCategories(title: String, abstractText: String) = {
        def escape(str: String) = str.replaceAll("\\s+", " ").replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")
        
        val doc = ArffJsonInstance(
            """[["",[]],["%s","%s",[],[]]]""".format(escape(title), escape(abstractText)),
            header
        )
        
        def classifyRecursive(toolsets: Map[CategoryIs with CategorizationHierarchy, AllClassesForDocument.CategoryToolset], inst: ArffJsonInstance, header: ArffJsonHeader): List[Pair[CategoryIs, Double]] = {
            val positives = (for((cat, toolset) <- toolsets) yield {
                val source = ArffJsonInstancesSource(List(doc), header)
                
                val mappedSource = toolset.mapper(source)
                val classification = toolset.classifier.calculateClassifications(mappedSource).head
                
                val certainty = toolset.thresholdFunction.classificationToCertainty(classification.classification)
                if(certainty >= 0.5) List((cat, certainty)) ++ classifyRecursive(toolset.children, inst, header)
                else List()
            }).flatten
            
            positives.toList
        }
        
        classifyRecursive(toolset, doc, header).map(pair => (pair._1.filenameExtension, pair._2))
    }
    
    def categoryCertainies(title: String, abstractText: String): List[(String, Double)] = {
        def escape  (str: String) = str.replaceAll("\\s+", " ").replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")
        
        categoryCertainies(ArffJsonInstance(
            """[["",[]],["%s","%s",[],[]]]""".format(escape(title), escape(abstractText)),
            header
        ))
    }
        
    def categoryCertainies(doc: ArffJsonInstance): List[(String, Double)] = {
        def f(toolsets: Map[CategoryIs with CategorizationHierarchy, AllClassesForDocument.CategoryToolset], inst: ArffJsonInstance, header: ArffJsonHeader): List[Pair[CategoryIs, Double]] = {
            val certainties = (for((cat, toolset) <- toolsets) yield {
                val source = ArffJsonInstancesSource(List(doc), header)
                
                val mappedSource = toolset.mapper(source)
                val classification = toolset.classifier.calculateClassifications(mappedSource).head
                
                val certainty = toolset.thresholdFunction.classificationToCertainty(classification.classification)
                List((cat, certainty)) ++ f(toolset.children, inst, header)
            }).flatten
            
            certainties.toList
        }
        
        f(toolset, doc, header).map(pair => (pair._1.filenameExtension, pair._2))
    }
}

















