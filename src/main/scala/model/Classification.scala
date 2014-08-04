package model

import format.arff_json.ArffJsonInstance
import java.io.File
import common.Common.FileConversion._
import java.io.BufferedWriter
import java.io.FileWriter
import classifier.Classifier
import java.io.BufferedReader
import java.io.FileReader
import scala.io.{ Source => ScalaSource }
import classifier.Classifier
import classifier.Learner
import parser.ArffJsonInstancesSource
import parser.ContentDescribable
import parser.ArffJsonInstancesSource
import common.Path
import common.FileManager
import FileManager.Protocol._
import com.google.gson.JsonParser
import com.google.gson.JsonArray
import com.google.gson.JsonPrimitive

object RawClassification {
    class Category(val name: String) {
        override def toString = name
    }
    val TRUE_POSITIVE = new Category("True Positive")
    val FALSE_POSITIVE = new Category("False Positive")
    val TRUE_NEGATIVE = new Category("True Negative")
    val FALSE_NEGATIVE = new Category("False Negative")
    
    def apply(str: String) = {
        val a = new JsonParser().parse(str).asInstanceOf[JsonArray]
        
        val classification: Double = {
            val jsonPrimitive = a.get(1).asInstanceOf[JsonPrimitive]
            if(jsonPrimitive.isNumber()) {
                jsonPrimitive.getAsDouble()
            } else {
                throw new RuntimeException("Value at position 1 in string \"" + a + "\" cannot be interpreted as Double")
            }
        }
        
        val trueClass: Double = {
            val jsonPrimitive = a.get(2).asInstanceOf[JsonPrimitive]
            if(jsonPrimitive.isNumber()) {
                jsonPrimitive.getAsDouble()
            } else {
                throw new RuntimeException("Value at position 2 in string \"" + a + "\" cannot be interpreted as Double")
            }
        }
        
        new RawClassification(a.get(0).asInstanceOf[String], classification, trueClass)
    }
    
    def fromFile(fullFilename: String) = {
        (FileManager !? ReceiveFile(fullFilename)) match {
            case AcceptReceiveFile(file) => {
                ScalaSource.fromFile(file).getLines.map(RawClassification(_)).toList
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " file does not exist")
            case Error(msg) => throw new RuntimeException(msg)
        }
    }
    
    def save(classifications: Iterable[RawClassification], file: Path) {
        (FileManager !? CreateFile(file)) match {
            case AcceptCreateFile(fileHandle) => {
                val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                for(classification <- classifications) {
                    writer.write(classification.toJson + "\n")
                }
                writer.close
                fileHandle.close
            }
            case RejectCreateFile => 
        }
    }
    
    def findBestThresholdWithPrecison(classifications: List[RawClassification], targetPrecision: Double) = {
        val sortedClassifications = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        
        var truePositives = 0
        var falsePositives = 0
        var trueNegatives = sortedClassifications.count(c => c.realValue < 0)
        var falseNegatives = sortedClassifications.count(c => c.realValue >= 0)
        
        def prec(truePos: Int, falsePos: Int, trueNeg: Int, falseNeg: Int) = truePos.toDouble / (truePos + falsePos)
        def rec(truePos: Int, falsePos: Int, trueNeg: Int, falseNeg: Int) = truePos.toDouble / (truePos + falseNeg)
        def fmeasure(prec: Double, rec: Double, alpha: Double) = ((1 + alpha) * prec * rec) / ((alpha * prec) + rec) 
        
        // if no suitable threshold can be found because the precision cannot be reached, fall back to maximum threshold
        val fallbackThreshold = sortedClassifications(0).classification
        
        val bestn = sortedClassifications.flatMap(cl => {
            if(cl.realValue >= 0) {
                truePositives += 1
                falseNegatives -= 1
            } else {
                falsePositives += 1
                trueNegatives -= 1
            }
            
            val p = prec(truePositives, falsePositives, trueNegatives, falseNegatives)
            val r = rec(truePositives, falsePositives, trueNegatives, falseNegatives)
            
            if(p.isNaN() || r.isNaN()) List()
            else List(Tuple3(cl.classification, p, r))
        })
        
        val candidates = bestn.filter(_._2 >= targetPrecision)
        
        val bestThreshold = 
            if(candidates.isEmpty) fallbackThreshold
            else candidates.maxBy(_._3)._1
        
        bestThreshold
    }
    
    def findBestThreshold1(classifications: List[RawClassification], alpha: Double) = {
        val sortedClassifications = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        
        var truePositives = 0
        var falsePositives = 0
        var trueNegatives = sortedClassifications.count(c => c.realValue < 0)
        var falseNegatives = sortedClassifications.count(c => c.realValue >= 0)
        
        def prec(truePos: Int, falsePos: Int, trueNeg: Int, falseNeg: Int) = truePos.toDouble / (truePos + falsePos)
        def rec(truePos: Int, falsePos: Int, trueNeg: Int, falseNeg: Int) = truePos.toDouble / (truePos + falseNeg)
        def fmeasure(prec: Double, rec: Double, alpha: Double) = ((1 + alpha) * prec * rec) / ((alpha * prec) + rec) 
        
        val bestn = sortedClassifications.flatMap(cl => {
            if(cl.realValue >= 0) {
                truePositives += 1
                falseNegatives -= 1
            } else {
                falsePositives += 1
                trueNegatives -= 1
            }
            
            val p = prec(truePositives, falsePositives, trueNegatives, falseNegatives)
            val r = rec(truePositives, falsePositives, trueNegatives, falseNegatives)
            val f = fmeasure(p, r, alpha)
            
            if(p.isNaN() || r.isNaN() || f.isNaN()) List()
            else List(Tuple4(cl.classification, p, r, f))
        })
        
        val best = bestn.maxBy(_._4)
        
        (best._1, best._4)
    }
    
    def findBestThreshold2(classifications: List[RawClassification], alpha: Double) = {
        val meanPositives = {
            val x = classifications.filter(c => c.realValue > 0).map(c => c.classification) 
            x.reduceLeft(_ + _) / x.size
        }
        
        val meanNegatives = {
            val x = classifications.filter(c => c.realValue < 0).map(c => c.classification) 
            x.reduceLeft(_ + _) / x.size
        }
        
        if(meanPositives <= meanNegatives) {
            // classifications cannot be normalized properly, because the center of classifions for positive examples is lower or equal to the center of classifications for negative examples
            None
        }
        
        var stepWidth = math.abs(meanPositives - meanNegatives) / 2
        var t = (meanPositives + meanNegatives) / 2
        var opt: Pair[Double, Double] = null
        
        def fmeasure(prec: Double, rec: Double, alpha: Double) = ((1 + alpha) * prec * rec) / ((alpha * prec) + rec) 
        
        for(i <- 0 until 100) {
            val p = Classifier.precision(classifications.view.map(c => new RawClassification(c.id, c.classification - t, c.realValue)))
            val r = Classifier.recall(classifications.view.map(c => new RawClassification(c.id, c.classification - t, c.realValue)))
            
            // set new max if f-measure is better
            if(!fmeasure(p, r, alpha).isNaN() && (opt == null || opt._2 > fmeasure(p, r, alpha))) {
                opt = (t, fmeasure(p, r, alpha))
            }
            
            if(p > r || p.isNaN()) {
                t = t - stepWidth
            } else {
                t = t + stepWidth
            }
            
            stepWidth = stepWidth / 1.5
        }
        
        if(opt == null) {
            (0.0, 0.0)
        } else {
        	opt
        }
    }
    
    def findBestThreshold(classifications: List[RawClassification], alpha: Double = 1.0) = {
        val (t1, f1) = findBestThreshold1(classifications, alpha)
        val (t2, f2) = try {findBestThreshold2(classifications, alpha)} catch { case _: Throwable => (0.0, 0.0)}
        
        if(f2.isNaN() || f1 > f2) t1
        else t2
    }
    
    def toBreakEven(classifications: List[RawClassification], alpha: Double = 1.0) = {
        val t = try {
            findBestThreshold(classifications, alpha)
        } catch {
            case _: Throwable => 0.0
        }
        
        classifications.map(c => {
            new RawClassification(c.id, (c.classification - t), c.realValue)
        })
    }
    
    def withThreshold(classifications: List[RawClassification], t: Double) = classifications.map(c => {
        new RawClassification(c.id, (c.classification - t), c.realValue)
    })
    
    def precRecGraphPoints(classifications: List[RawClassification]) = {
        val sorted = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        val numPositives = classifications.count(c => c.realValue >= 0)
        
		// println(sorted.take(500).mkString("\n"))
        val l = (List[Int]() /: sorted)((old, elem) => {
            if(elem.realValue >= 0.0) {
                if(old.isEmpty) List(1)
                else (old.head + 1) :: old
            } else {
                if(old.isEmpty) List(0)
                else old.head :: old 
            } 
        }).reverse
        
        for(percentPoints <- (1 to 100)) yield {
            val targetNumDocs = math.max(((numPositives * percentPoints) / 100), 1)
            val docsToTake = l.takeWhile(_ <= targetNumDocs).size
            
            ((percentPoints.toDouble / 100), (targetNumDocs.toDouble / docsToTake))
        }
    }
    
    def microPrecRecGraphPoints(classifications: List[RawClassification]) = {
        val sorted = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        val numPositives = classifications.count(c => c.realValue >= 0)
        
        val l = (List[Int]() /: sorted.iterator)((old, elem) => {
            if(elem.realValue >= 0.0) {
                if(old.isEmpty) List(1)
                else (old.head + 1) :: old
            } else {
                if(old.isEmpty) List(0)
                else old.head :: old 
            } 
        }).reverse
        
        for(percentPoints <- (1 to 100)) yield {
            val targetNumDocs = ((numPositives * percentPoints) / 100)
            val docsToTake = if(targetNumDocs == 0) 0 else l.takeWhile(_ <= targetNumDocs).size
            
            (targetNumDocs, (docsToTake - targetNumDocs))
        }
    }
    
    def normalize(classifications: List[RawClassification]) = {
        if(classifications.isEmpty) classifications
        else {
            val avgAbsClassification = classifications.map(c => math.abs(c.classification)).reduceLeft(_ + _) / classifications.size
			
            classifications.map(c => new RawClassification(c.id, c.classification / avgAbsClassification, c.realValue))
        }
    }
    
    def adaBoost(pool: List[Iterable[RawClassification]]) = {
            
        val weights = pool(0).map(_.id -> 1.0).toMap
        def error(classifications: Iterable[RawClassification]) = 
            ((0.0, 0.0) /: classifications)((old, c) => if(c.hit) (old._1 + weights(c.id), old._2) else (old._1, old._2 + weights(c.id)))
        
        def alpha(classificatons: Iterable[RawClassification]) = {
            val e = error(classificatons)
            math.log(e._1 / e._2) / 2
        }
        
        def boostStep(
            pool: List[Iterable[RawClassification]], 
            weights: Map[String, Double],
            classifiersAndWeights: List[Pair[Iterable[RawClassification], Double]]
        ): List[Pair[Iterable[RawClassification], Double]]  = {
            if(pool.size != 0) {
                val (bestC, e) = {
                    val x = pool.map(c => (c, error(c))).minBy(_._2._2)
                    (x._1, x._2)
                }
                
                val a = alpha(bestC)
                val newWeights = bestC.map(c => c.id -> (if(c.hit) weights(c.id) * math.sqrt(e._1/e._2) else weights(c.id) * math.sqrt(e._2/e._1))).toMap  
                
                boostStep(pool diff List(bestC), newWeights, Pair(bestC, a) :: classifiersAndWeights)
            } else {
                classifiersAndWeights
            }
        }
        
        val x = boostStep(pool, weights, List())
        
        val r = x.map(_._1)
        val w = x.map(_._2)
        
        r.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip w))(
                    (old, clw) => old + clw._1.classification * clw._2
                )),
                classificationList(0).realValue)
        })
    }
    
    def weightedSum(pool: List[List[RawClassification]]) = {
        val resultsAndWeights = pool.map(
            res => {
                val results = normalize(res)
                val weight = {
                    val m = Classifier.fMeasure(res, 1.0)
                    if(m.isNaN()) 0 else m
                }
                (results, weight) 
            }
        )
        
        val results = resultsAndWeights.map(_._1)
        val weights = resultsAndWeights.map(_._2)
        val normedWeights = weights.map(w => w / weights.reduceLeft(_ + _))
        
        val finalRes = results.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList(0).id, 
                ((0.0 /: (classificationList zip normedWeights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )),
                classificationList(0).realValue)
        })
        
        finalRes
    }
    
    def weightedSumWithCoofficients(pool: List[Iterable[RawClassification]], coofficients: List[Double]) = {
        val normedWeights = coofficients.map(w => w / coofficients.reduceLeft(_ + _))
        
        val finalRes = pool.transpose.map(classificationList => {
            require(classificationList.forall(c => c.id == classificationList(0).id))
            new RawClassification(
                classificationList.head.id, 
                ((0.0 /: (classificationList zip normedWeights))(
                    (old, clw) => old + (if(clw._1.classification.isNaN) 0 else clw._1.classification) * clw._2
                )),
                classificationList.head.realValue)
        })
        
        finalRes
    }
}

class RawClassification(val id: String, val classification: Double, val realValue: Double) {
    import RawClassification._
    
    def truePositive = classification >= 0 && realValue >= 0
    def falsePositive = classification >= 0 && realValue < 0
    def trueNegative = classification < 0 && realValue < 0
    def falseNegative = classification < 0 && realValue >= 0
    
    def isPositive = realValue >= 0
    def isNegative = !isPositive
    
    def isClassifiedPositive = classification >= 0
    def isClassifiedNegative = !isClassifiedPositive
    
    def hit = truePositive || trueNegative
    def miss = falsePositive || falseNegative
    
    def cat = if(truePositive) TRUE_POSITIVE
        else if(falsePositive) FALSE_POSITIVE
        else if(trueNegative) TRUE_NEGATIVE
        else if(falseNegative) FALSE_NEGATIVE
        else throw new RuntimeException("Invalid category")
    
    override def toString = "RawClassification(%s, %3.4f, %1.1f)".format(id, classification, realValue)
    def toJson = "[\"" + id + "\", " + classification + ", " + realValue + "]"
}

object CertaintyToThresholdFunction {
    def apply(classifier: Classifier, trainSet: ArffJsonInstancesSource with ContentDescribable, tuningSet: ArffJsonInstancesSource with ContentDescribable): CertaintyToThresholdFunction  = {
        val path = for(
            cd <- classifier.trainBaseContentDescription;
            parent <- classifier.parent
        ) yield {
            common.Path.tuningPath / Learner.classifierFilename(cd, classifier.targetClassDef, Some(parent))
        }
        
        path match {
            case Some(path) => {
                (FileManager !? DoesFileExist(path)) match {
                    case FileExists => {
                        CertaintyToThresholdFunction.load(path)
                    }
                    case FileNotExists => {
                        val fun = classifier2CertaintyFunction(classifier, trainSet, tuningSet)
                        fun.save(path)
                        fun
                    }
                }
            }
            case None => classifier2CertaintyFunction(classifier, trainSet, tuningSet)
        }
    } 
    
    private def classifier2CertaintyFunction(classifier: Classifier, trainBase: ArffJsonInstancesSource with ContentDescribable, tuningSet: ArffJsonInstancesSource with ContentDescribable) = CertaintyToThresholdFunction(
        classifier.parent match {
            case Some(parent) => parent.classifications(trainBase, tuningSet, classifier.targetClassDef)
            case None => throw new RuntimeException("cannot map tuning set to generate classificaitons because there is no learner associate with the given classifier")
        }
    )
    
    def apply(classifications: List[RawClassification]) = {
        val sorted = classifications.sortWith((c1, c2) => c1.classification > c2.classification)
        val numPositives = classifications.count(c => c.realValue >= 0)
        
        val l = (List[Pair[Int, Double]]() /: sorted.iterator)((old, elem) => {
            if(elem.realValue >= 0.0) {
                if(old.isEmpty) List((1, elem.classification))
                else ((old.head._1 + 1, elem.classification)) :: old
            } else {
                if(old.isEmpty) List((0, elem.classification))
                else (old.head._1, elem.classification) :: old
            } 
        }).reverse.toIndexedSeq
        
        val points = for(percentPoints <- (1 to 100)) yield {
            val targetNumDocs = math.max(((numPositives * percentPoints) / 100), 1)
            val docsToTake = l.takeWhile(_._1 <= targetNumDocs).size
            val threshold = l(docsToTake - 1)._2
            
            ((targetNumDocs.toDouble / docsToTake), (percentPoints.toDouble / 100), threshold)
        }
        
        val skylinePoints = (for(p <- points) yield {
            if(points.exists(p2 => p2._1 > p._1 && p2._2 > p._2)) List()
            else List(p)
        }).flatten.toList
        
        new CertaintyToThresholdFunction(skylinePoints.map(p => (p._1, p._3)))
    }
    
    def load(fullFilename: String) = {
        (FileManager !? ReceiveFile(fullFilename)) match {
            case AcceptReceiveFile(file) => {
                val lines = ScalaSource.fromFile(file).getLines()
                
                new CertaintyToThresholdFunction(
                    lines.map(line => {
                        val jarr = new JsonParser().parse(line).asInstanceOf[JsonArray]
                        def toDouble(a: Object) = a match {
                            case d: java.lang.Double => d.toDouble
                            case bd: java.math.BigDecimal => bd.doubleValue
                            case bd: scala.math.BigDecimal => bd.doubleValue
                        }
                        (toDouble(jarr.get(0)), toDouble(jarr.get(1)))
                    }).toList
                )
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " does not exist")
        }
    }
}

class CertaintyToThresholdFunction(val precThdPairs: List[Pair[Double, Double]]) {
    def certaintyToThreshold(targetPrecision: Double) = if(precThdPairs.size >= 2) {
        val nearestPoints = precThdPairs
        		.sortWith((pr1, pr2) => math.abs(pr1._1 - targetPrecision) < math.abs(pr2._1 - targetPrecision))
        		.take(2)
        
        val point1 = nearestPoints.minBy(_._1)
        val point2 = nearestPoints.maxBy(_._1)
        
        val m = (point2._2 - point1._2) / (point2._1 - point1._1)
        val b = (point1._2 - m*point1._1)
        m*targetPrecision + b
    } else {
        Double.MaxValue
    }
    
    def classificationToCertainty(classificationValue: Double) = if(precThdPairs.size >= 2) {
        val nearestPoints = precThdPairs
        		.sortWith((pr1, pr2) => math.abs(pr1._2 - classificationValue) < math.abs(pr2._2 - classificationValue))
        		.take(2)
        
        val point1 = nearestPoints.minBy(_._2)
        val point2 = nearestPoints.maxBy(_._2)
        
        val m = (point2._1 - point1._1) / (point2._2 - point1._2)
        val b = (point1._1 - m*point1._2)
        m*classificationValue + b
    } else {
        0.0
    }
    
    def save(fullFilename: String) {
        (FileManager !? CreateFile(fullFilename)) match {
            case AcceptCreateFile(fileHandle) => {
                val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                for(precThdPair <- precThdPairs) {
                    writer.write("[" + precThdPair._1 + "," + precThdPair._2 + "]\n")
                }
                writer.close()
                fileHandle.close
            }
            case RejectCreateFile => throw new RuntimeException("write file " + fullFilename + " rejected")
        }
    }
}













