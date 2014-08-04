package classifier

import scala.collection.mutable
import filter.Filter
import parser.ArffJsonInstancesSource
import filter.SelectionFilter
import format.arff_json.ArffJsonHeader
import parser.ContentDescribable
import model.CertaintyToThresholdFunction
import format.arff_json.ArffJsonInstance

object ClassificationSystem {
    def apply(
            trainSet: ArffJsonInstancesSource with ContentDescribable,
            tuningSet: ArffJsonInstancesSource with ContentDescribable,
            header: ArffJsonHeader,
            learnerForLevel: (Int => ((CategoryIs with CategorizationHierarchy) => Learner)),
            catsForLevel: (Int => List[CategoryIs with CategorizationHierarchy]),
            depth: Int,
            certaintyThreshold: Double
    ) = {
        val classifierHierarchy = buildClassifierHierarchy(
            learnerForLevel,
            CategoryIsMscSome(None, None, None),
            catsForLevel,
            trainSet,
            tuningSet,
            0,
            depth
        )
        
        new ClassificationSystem(classifierHierarchy, certaintyThreshold, header)
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
    
    def buildClassifierHierarchy(
        learnerForLevel: (Int => ((CategoryIs with CategorizationHierarchy) => Learner)),
        parentCat: CategoryIs with CategorizationHierarchy,
        catsForLevel: (Int => List[CategoryIs with CategorizationHierarchy]),
        trainSet: ArffJsonInstancesSource with ContentDescribable,
        tuningSet: ArffJsonInstancesSource with ContentDescribable,
        level: Int,
        depth: Int
    ): Map[CategoryIs with CategorizationHierarchy, ClassificationSystemNode] = {
        println(parentCat)
        (for(cat <- catsForLevel(level).filter(cat => cat.parent.filenameExtension == parentCat.filenameExtension)) yield {
            println(cat.filenameExtension + ", " + level)
            val learner = learnerForLevel(level)(cat)
            val mapper = mapperCached(learner, trainSet, cat)
            val classifier = learner.classifier(trainSet, cat)
            
            val children = if(level < depth) {
                buildClassifierHierarchy(learnerForLevel, cat, catsForLevel, trainSet, tuningSet, level+1, depth)
            } else {
                Map[CategoryIs with CategorizationHierarchy, ClassificationSystemNode]()
            }
            
            cat -> ClassificationSystemNode(mapper, classifier, CertaintyToThresholdFunction(classifier, trainSet, tuningSet), children)
        }).toMap
    }

    case class ClassificationSystemNode(
        val mapper: (ArffJsonInstancesSource => ArffJsonInstancesSource), 
        val classifier: Classifier, 
        val certaintyFunction: CertaintyToThresholdFunction, 
        val children: Map[CategoryIs with CategorizationHierarchy, ClassificationSystemNode]
    )
}

class ClassificationSystem(
        classifierHierarchy: Map[CategoryIs with CategorizationHierarchy, ClassificationSystem.ClassificationSystemNode],
        certaintyThreshold: Double,
        header: ArffJsonHeader
    ) {
    
    def classify(doc: ArffJsonInstance) = 
        classifyRecursive(classifierHierarchy, ArffJsonInstancesSource(List(doc), header), certaintyThreshold, header).map(pair => (pair._1.filenameExtension, pair._2))
    
    private def classifyRecursive(
            classifierHierarchy: Map[CategoryIs with CategorizationHierarchy, ClassificationSystem.ClassificationSystemNode], 
            inst: ArffJsonInstancesSource, 
            certaintyThreshold: Double, 
            header: ArffJsonHeader
    ): List[Pair[CategoryIs, Double]] = {
        val positives = (for((cat, classificationSystemNode) <- classifierHierarchy) yield {
            val mappedSource = classificationSystemNode.mapper(inst)
            val classification = classificationSystemNode.classifier.calculateClassifications(mappedSource).head
            val certainty = classificationSystemNode.certaintyFunction.classificationToCertainty(classification.classification)
            if(certainty >= certaintyThreshold) List((cat, certainty)) ++ classifyRecursive(classificationSystemNode.children, inst, certaintyThreshold, header)
            else List()
        }).flatten
        
        positives.toList
    }
}








