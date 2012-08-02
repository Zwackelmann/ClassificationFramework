package model

import net.sf.json.JSONSerializer
import net.sf.json.JSONArray
import format.arff_json.ArffJsonInstance
import java.io.File
import common.Common.FileConversion._
import java.io.BufferedWriter
import java.io.FileWriter
import classifier.Classifier

object Classification {
    def apply(str: String): Classification = {
        val classification = JSONSerializer.toJSON(str).asInstanceOf[JSONArray]
        
        Classification(classification.get(0).asInstanceOf[String], classification.get(1).asInstanceOf[Double], classification.get(2).asInstanceOf[Int])
    }
    
    def apply(paperId: String, classification: Double, trueClass: Double): Classification = {
        if(classification >= 0 && trueClass >= 0) {
            new TruePositive(paperId, classification)
        } else if(classification >= 0 && trueClass < 0.0) {
            new FalsePositive(paperId, classification)
        } else if(classification < 0 && trueClass < 0.0) {
            new TrueNegative(paperId, -classification)
        } else if(classification < 0 && trueClass >= 0) {
            new FalseNegative(paperId, -classification)
        } else {
            throw new RuntimeException()
        }
    }
}

abstract class Classification(val paperId: String, val classification: Double, val trueClass: Double) extends Serializable {
    val certainty: Double
    override def toString = {
        val a = new JSONArray()
        a.add(paperId)
        a.add(classification.toDouble)
        a.add(trueClass)
        a.toString
    }
}

object RawClassification {
    class Category(val name: String) {
        override def toString = name
    }
    val TRUE_POSITIVE = new Category("True Positive")
    val FALSE_POSITIVE = new Category("False Positive")
    val TRUE_NEGATIVE = new Category("True Negative")
    val FALSE_NEGATIVE = new Category("False Negative")
    
    def apply(str: String) = {
        val a = JSONSerializer.toJSON(str).asInstanceOf[JSONArray]
        
        val classification: Double = a.get(1) match {
            case bigDecimal: java.math.BigDecimal => bigDecimal.doubleValue()
            case d: java.lang.Double => d
            case i: java.lang.Integer => new java.lang.Double(i.toString)
        }
        val trueClass = a.get(2) match {
            case d: java.lang.Double => d
            case i: java.lang.Integer => new java.lang.Double(i.toString)
        }
        
        new RawClassification(a.get(0).asInstanceOf[String], classification, trueClass)
    }
    
    def fromFile(file: File) = file.lines.map(RawClassification(_)).toList
    
    def save(classifications: Iterable[RawClassification], file: File) {
        val w = new BufferedWriter(new FileWriter(file))
        for(classification <- classifications) {
            w.write(classification.toJson + "\n")
        }
        w.close
    }
    
    def normalize(classifications: List[RawClassification]) = {
        val meanPositives = {
            val x = classifications.filter(c => c.realValue > 0).map(c => c.classification) 
            x.reduceLeft(_ + _) / x.size
        }
        
        val meanNegatives = {
            val x = classifications.filter(c => c.realValue < 0).map(c => c.classification) 
            x.reduceLeft(_ + _) / x.size
        }
        
        if(meanPositives <= meanNegatives) {
            throw new RuntimeException("classifications cannot be normalized properly, because the center of classifions for positive examples is lower or equal to the center of classifications for negative examples")
        }
        
        var stepWidth = math.abs(meanPositives - meanNegatives) / 2
        var t = (meanPositives + meanNegatives) / 2
        def f(p: Double, r: Double) = p*0.5 + r*0.5
        var max: Pair[Double, Double] = null
        
        for(i <- 0 until 20) {
            val p = Classifier.precision(classifications, t, 0)
            val r = Classifier.recall(classifications, t, 0)
            
            // set new max if f-measure is better
            if(max == null || max._2 < f(p, r)) {
                max = (t, f(p, r))
            }
            
            if(p > r) {
                t = t - stepWidth
            } else {
                t = t + stepWidth
            }
            
            stepWidth = stepWidth / 2
        }
        
        t = max._1
        
        val meanDiffToThreshold = classifications.map(c => math.abs(c.classification - t)).reduceLeft(_ + _) / classifications.size
        
        classifications.map(c => {
            new RawClassification(c.id, (c.classification - t) / meanDiffToThreshold, c.realValue)
        })
    }
}

class RawClassification(val id: String, val classification: Double, val realValue: Double) {
    import RawClassification._
    
    def truePositive(threshold: Double) = classification > threshold && realValue > 0
    def falsePositive(threshold: Double) = classification > threshold && realValue <= 0
    def trueNegative(threshold: Double) = classification <= threshold && realValue <= 0
    def falseNegative(threshold: Double) = classification <= threshold && realValue > 0
    
    def cat(threshold: Double) = if(truePositive(threshold)) TRUE_POSITIVE
        else if(falsePositive(threshold)) FALSE_POSITIVE
        else if(trueNegative(threshold)) TRUE_NEGATIVE
        else if(falseNegative(threshold)) FALSE_NEGATIVE
        else throw new RuntimeException("Invalid category")
    
    override def toString = "RawClassification(%s, %3.4f, %1.1f)".format(id, classification, realValue)
    def toJson = "[\"" + id + "\", " + classification + ", " + realValue + "]"
}

case class TruePositive(_paperId: String, val certainty: Double) extends Classification(_paperId, 0.5 + (certainty / 2), 1.0)
case class FalsePositive(_paperId: String, val certainty: Double) extends Classification(_paperId, 0.5 + (certainty / 2), -1.0)
case class TrueNegative(_paperId: String, val certainty: Double) extends Classification(_paperId, 0.5 - (certainty / 2), -1.0)
case class FalseNegative(_paperId: String, val certainty: Double) extends Classification(_paperId, 0.5 - (certainty / 2), 1.0)



