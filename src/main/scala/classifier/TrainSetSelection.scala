package classifier
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import scala.collection.mutable
import format.arff_json.ArffJsonInstance
import java.util.{TreeSet => JTreeSet}
import java.util.Comparator
import scala.collection.JavaConversions._
import common.Path.arffJsonPath
import java.io.BufferedWriter
import java.io.FileWriter
import parser.ContentDescribable

object TrainSetSelection {
    def preFilter(inst: ArffJsonInstance) = true
    
    def file(source: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, trainSetSelectionDef: TrainSetSelectionDefinition) = arffJsonPath / (
        source.contentDescription.filenameAppendix + "-" + 
        trainSetSelectionDef.filenameAppendix + "-" + 
        targetClassDef.filenameExtension +  
        ".json"
    )
    
    def applySelection(source: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, trainSetSelectionDef: TrainSetSelectionDefinition) = {
        val targetFile = file(source, targetClassDef, trainSetSelectionDef)
        
        if(!targetFile.exists) {
            trainSetSelectionDef match {
                case FixedTrainSetSelection(numTargetInst, numOtherInst) => {
                    val categoryIs = targetClassDef.asInstanceOf[CategoryIs]
                    
                    val groupFun = (if(categoryIs.targetLevel == 1) {
                        ((l: List[String]) => l.map(_.substring(0, 2)).distinct)
                    } else if(categoryIs.targetLevel == 2) {
                        ((l: List[String]) => l.map(_.substring(0, 3)).distinct)
                    } else if(categoryIs.targetLevel == 3) {
                        ((l: List[String]) => l.map(_.substring(0, 5)).distinct)
                    } else {
                        error("")
                    })
                    
                    println("collecting instances data... ")
                    val totalInstByGroup = {
                        val map = new mutable.HashMap[String, Int] {
                            override def default(key: String) = 0
                        }
                        
                        for(inst <- source; if preFilter(inst)) {
                            val groups = groupFun(inst.categories).distinct
                            for(group <- groups) {
                                map(group) = map(group) + 1
                            }
                        }
                        
                        map.toMap
                    }
                    println("done")
                    
                    println("select training instances... ")
                    val writer = new BufferedWriter(new FileWriter(targetFile))
                    writer.write(source.header.toJson + "\n")
                    
                    var numTrainingInst = 0
                    val trainingInst = {
                        val currentInstByGroup = new mutable.HashMap[String, Int] {
                            override def default(key: String) = 0
                        }
                        
                        for(inst <- source; if preFilter(inst)) {
                            val targetNumInst = 
                                if(targetClassDef(inst.categories)) numTargetInst
                                else numOtherInst
                            
                            val groups = groupFun(inst.categories).distinct
                            
                            val probForInst = {
                                if(groups.isEmpty) {
                                    0
                                } else {
                                    val probByGroup = targetNumInst match {
                                        case Some(number) => groups.map(group => number.toDouble / totalInstByGroup(group))
                                        case None => List(groups.size.toDouble)
                                    }
                                    probByGroup.reduce(_ + _) / groups.size
                                }
                            }
                            if(math.random < probForInst) {
                                writer.write(inst.toJson + "\n")
                                numTrainingInst += 1
                                
                                val groups = groupFun(inst.categories)
                                for(group <- groups) {
                                    currentInstByGroup(group) = currentInstByGroup(group) + 1
                                }
                            }
                        }
                        
                        // println("average numInst by group: " + (currentInstByGroup.values.reduceLeft(_ + _).toDouble / currentInstByGroup.values.size))
                        // println(currentInstByGroup.mkString("\n"))
                        // buffer.toList
                    }
                    writer.close()
                    
                    println("done")
                    println("total instances for training: " + numTrainingInst)
                }
                
                case BalancedTrainSetSelection(maxInst) => {
                    val (targetInst, otherInst) = source.filter(preFilter).partition(i => targetClassDef(i.categories))
                    val (numTargetInst, numOtherInst) = (targetInst.size, otherInst.size)
                    
                    val targetSize = {
                        val tmp = math.min(numTargetInst, numOtherInst)
                        if(maxInst.isDefined) math.min(maxInst.get, tmp)
                        else tmp
                    }
                    
                    val instancesIterator = targetInst.iterator.filter(
                            i => math.random < (targetSize.toDouble/numTargetInst)
                        ) ++ otherInst.iterator.filter(
                            i => math.random < (targetSize.toDouble/numOtherInst)
                        )
                    
                    val writer = new BufferedWriter(new FileWriter(targetFile))
                    writer.write(source.header.toJson + "\n")
                    for(inst <- instancesIterator) {
                        writer.write(inst.toJson + "\n")
                    }
                    writer.close()
                }
                
                case MaxForEachSet(maxPositives, maxNegatives) => {
                    val (positives, negatives) = source.filter(preFilter).partition(i => targetClassDef(i.categories))
                    val (numPositives, numNegatives) = (positives.size, negatives.size)
                    
                    val positivesIterator = 
                        if(!maxPositives.isDefined) positives.iterator
                        else positives.iterator.filter(i => math.random < (maxPositives.get.toDouble/numPositives))
                    
                    val negativesIterator = 
                        if(!maxNegatives.isDefined) negatives.iterator
                        else negatives.iterator.filter(i => math.random < (maxNegatives.get.toDouble/numNegatives))
                        
                    val instancesIterator = positivesIterator ++ negativesIterator
                    
                    val writer = new BufferedWriter(new FileWriter(targetFile))
                    writer.write(source.header.toJson + "\n")
                    for(inst <- instancesIterator) {
                        writer.write(inst.toJson + "\n")
                    }
                    writer.close()
                }
                
                case PrioritizeMainClass(targetNumInstances) => {
                    val minimumRateNegatives = 0.25
                    val cat = targetClassDef.asInstanceOf[CategoryIs]
                    val parentCategory = cat.parent
                    val catSubstring = cat.targetLevel match {
                        case 1 => (s: String) => s.substring(0, 2)
                        case 2 => (s: String) => s.substring(0, 3)
                        case 3 => (s: String) => s
                    }	
                    
                    val (numPositivesMainClass, numPositivesSecondaryClass, numNegatives) = (
                        ((0, 0, 0) /: source)((currentTuple, currentInstance) => {
                            if(currentInstance.categories.isEmpty) (currentTuple._1, currentTuple._2, currentTuple._3)
                            else if(targetClassDef(List(currentInstance.categories(0)))) (currentTuple._1 + 1, currentTuple._2, currentTuple._3)
                            else if(targetClassDef(currentInstance.categories)) (currentTuple._1, currentTuple._2 + 1, currentTuple._3)
                            else (currentTuple._1, currentTuple._2, currentTuple._3 + 1)
                        })
                    )
                    
                    val (negativesDistribution, numNegativeCategories) = {
                        val map = new mutable.HashMap[String, Int]() {
                            override def default(key: String) = 0
                        }
                        
                        for(inst <- source.filter(i => !targetClassDef(i.categories)); cat <- inst.categories.filter(c => parentCategory(List(c))).map(c => catSubstring(c)).distinct) {
                            map(cat) += 1
                        }
                        
                        (map.toMap, map.toList.size)
                    }
                    
                    println((numPositivesMainClass, numPositivesSecondaryClass, numNegatives))
                    val (targetNumPositivesMainClass, targetNumPositivesSecondaryClass, targetNumNegatives) = {
                        if(numPositivesMainClass < (1-minimumRateNegatives)*targetNumInstances) {
                            // take all positivesMainClass
                            if(numPositivesMainClass + numPositivesSecondaryClass < (1-minimumRateNegatives)*targetNumInstances) {
                                // take all positives
                                if(numPositivesMainClass + numPositivesSecondaryClass + numNegatives < targetNumInstances) {
                                    // take all instances
                                    (numPositivesMainClass, numPositivesSecondaryClass, numNegatives)
                                } else {
                                    // take all positives and fill with negatives
                                    (numPositivesMainClass, numPositivesSecondaryClass, targetNumInstances - (numPositivesMainClass + numPositivesSecondaryClass))
                                }
                            } else {
                                // take all positivesMainClass and some positivesSecondaryClass
                                val sec = ((1-minimumRateNegatives) * targetNumInstances).toInt - numPositivesMainClass
                                if(numPositivesMainClass + sec + numNegatives < targetNumInstances) {
                                    (numPositivesMainClass, sec, numNegatives)
                                } else {
                                    (numPositivesMainClass, sec, targetNumInstances - (numPositivesMainClass + sec))
                                }
                            }
                        } else {
                            val main = ((1-minimumRateNegatives) * targetNumInstances).toInt
                            if(main + numNegatives < targetNumInstances) {
                                (main, 0, numNegatives)
                            } else {
                                (main, 0, targetNumInstances - main)
                            }
                        }
                    }
                    
                    var i = 0
                    var j = 0
                    var k = 0
                    
                    val targetNumPerNegativeCategory = if(numNegativeCategories != 0) targetNumNegatives / numNegativeCategories else 0
                    
                    println("target: " + targetNumPerNegativeCategory)
                    
                    def propForNegatives(categories: List[String]) = {
                        categories.filter(c => parentCategory(List(c))).map(c => (targetNumPerNegativeCategory.toDouble / negativesDistribution(catSubstring(c)))).max
                    }
                    
                    val instancesIterator = if((numPositivesMainClass, numPositivesSecondaryClass, numNegatives) == (targetNumPositivesMainClass, targetNumPositivesSecondaryClass, targetNumNegatives)) {
                        source.iterator
                    } else { 
                        source.iterator.filter(inst => {
                            if(inst.categories.isEmpty) { 
                                false 
                            } else { 
	                            if(targetClassDef(List(inst.categories(0)))) (if(math.random < (targetNumPositivesMainClass.toDouble / numPositivesMainClass)) {i += 1; true} else false)
	                            else if(targetClassDef(inst.categories)) (if(math.random < (targetNumPositivesSecondaryClass.toDouble / numPositivesSecondaryClass)) {j += 1; true} else false)
	                            else (if(math.random < propForNegatives(inst.categories)) {k += 1; true} else false)
	                        }
                        })
                    }
                    
                    val writer = new BufferedWriter(new FileWriter(targetFile))
                    writer.write(source.header.toJson + "\n")
                    for(inst <- instancesIterator) {
                        writer.write(inst.toJson + "\n")
                    }
                    writer.close()
                    
                    println("written instances: " + (i, j, k))
                }
                
                case NoTrainSetSelection => throw new RuntimeException("Should not occur")
            }
        }
        
        ArffJsonInstancesSource(
            targetFile,
            source.contentDescription
        )
    }
}

trait TrainSetSelection extends Learner {
    val trainSetSelectionDef: TrainSetSelectionDefinition
    
    abstract override def mapInstances(inst: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition, set: Option[ContentDescription.Set] = None) = {
        val mappedInst = super.mapInstances(inst, targetClassDef, set)
        if(set == Some(ContentDescription.TrainSet) && trainSetSelectionDef != NoTrainSetSelection) {
            applySelection(mappedInst, targetClassDef)
        } else {
            mappedInst
        }
    }
    
    def applySelection(source: ArffJsonInstancesSource with ContentDescribable, targetClassDef: TargetClassDefinition) = TrainSetSelection.applySelection(source, targetClassDef, trainSetSelectionDef)
}







