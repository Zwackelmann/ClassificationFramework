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
                    val (groupFun, targetGroup) = targetClassDef match {
                        case topClassIs: TopClassIs => ((l: List[String]) => l.map(_.substring(0, 2)).distinct, topClassIs.targetClass)
                    }
                    
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
                
                case BalancedTrainSetSelection => {
                    val (targetInst, otherInst) = source.filter(preFilter).partition(i => targetClassDef(i.categories))
                    val (numTargetInst, numOtherInst) = (targetInst.size, otherInst.size)
                    
                    val targetSize = math.min(numTargetInst, numOtherInst)
                    
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







