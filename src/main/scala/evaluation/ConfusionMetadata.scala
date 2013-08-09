package evaluation

import parser.ArffJsonInstancesSource
import classifier.Learner
import model.RawClassification
import scala.collection.mutable.{HashMap, ListBuffer}
import classifier.CategoryIsMsc

object ConfusionMetadata {
    def apply(trainSet: ArffJsonInstancesSource, testSet: ArffJsonInstancesSource, learner: Learner, categories: List[CategoryIsMsc]) = {
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
    def confusion(c1: String, c2: String) = confusionCache.getOrElseUpdate((c1, c2), {
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
    })
    
    /*def confusion(c1: String, c2: String) = confusionCache.getOrElseUpdate((c1, c2), {
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
    })*/
}