package model

import net.sf.json.JSONSerializer
import net.sf.json.JSONArray
import format.arff_json.ArffJsonInstance
import java.io.File
import common.Common.FileConversion._
import java.io.BufferedWriter
import java.io.FileWriter
import classifier.Classifier

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
    
    def toBreakEven1(classifications: List[RawClassification]) = {
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
        
        for(i <- 0 until 100) {
            val p = Classifier.precision(classifications.view.map(c => new RawClassification(c.id, c.classification - t, c.realValue)), 0)
            val r = Classifier.recall(classifications.view.map(c => new RawClassification(c.id, c.classification - t, c.realValue)), 0)
            
            // set new max if f-measure is better
            if(!math.abs(p-r).isNaN() && (opt == null || opt._2 > math.abs(p-r))) {
                opt = (t, math.abs(p-r))
            }
            
            if(p > r || p.isNaN()) {
                t = t - stepWidth
            } else {
                t = t + stepWidth
            }
            
            stepWidth = stepWidth / 1.5
        }
        
        t = opt._1
        
        Some(classifications.map(c => {
            new RawClassification(c.id, (c.classification - t), c.realValue)
        }))
    }
    
    def toBreakEven2(classifications: List[RawClassification], alpha: Double) = {
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
        
        classifications.map(c => {
            new RawClassification(c.id, (c.classification - best._1), c.realValue)
        })
    }
    
    def toBreakEven(classificatinos: List[RawClassification], alpha: Double = 1.0) = {
        val br1 = toBreakEven1(classificatinos)
        val br2 = toBreakEven2(classificatinos, alpha)
        
        val f1 = if(br1 == None) 0.0 else Classifier.fMeasure(br1.get, alpha)
        val f2 = Classifier.fMeasure(br2, alpha)
        
        if((f2.isNaN() || f1 > f2) && br1.isDefined) br1.get
        else br2
    }
    
    def normalize(classifications: Iterable[RawClassification]) = {
        val avgAbsClassification = classifications.map(c => math.abs(c.classification)).reduceLeft(_ + _) / classifications.size
        classifications.map(c => new RawClassification(c.id, c.classification / avgAbsClassification, c.realValue))
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
                
                boostStep(pool - bestC, newWeights, Pair(bestC, a) :: classifiersAndWeights)
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
    
    def weightedSum(pool: List[Iterable[RawClassification]]) = {
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
                classificationList(0).id, 
                ((0.0 /: (classificationList zip normedWeights))(
                    (old, clw) => old + clw._1.classification * clw._2
                )),
                classificationList(0).realValue)
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















