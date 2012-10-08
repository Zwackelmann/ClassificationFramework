package classifier
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import scala.collection.mutable
import format.arff_json.ArffJsonInstance
import parser.ArffJsonInstancesMapping
import parser.ArffJsonInstancesFile
import java.util.{TreeSet => JTreeSet}
import java.util.Comparator
import scala.collection.JavaConversions._
import common.Path.arffJsonPath
import parser.ArffJsonInstancesFile2
import java.io.BufferedWriter
import java.io.FileWriter

object TrainSetSelection {
    def preFilter(inst: ArffJsonInstance) = true
    
    def file(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, numTargetInst: Option[Int], numOtherInst: Option[Int]) = 
        arffJsonPath / (
            source.contentDescription.filenameAppendix + "-tss-" + 
            targetClassDef.filenameExtension + "-" +
            (if(numTargetInst.isDefined) numTargetInst.get else "all") + "-" + 
            (if(numTargetInst.isDefined) numOtherInst.get else "all") + 
            ".json"
        )
    
    def applySelection(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, numTargetInst: Option[Int], numOtherInst: Option[Int]) = {
        val targetFile = file(source, targetClassDef, numTargetInst, numOtherInst)
        
        if(!targetFile.exists) {
            /*print("calculate centroids... ")
            val centroids = ArffJsonInstancesSource.centroids(source)
            println("done")
            */
            val (groupFun, targetGroup) = targetClassDef match {
                case topClassIs: TopClassIs => ((l: List[String]) => l.map(_.substring(0, 2)).distinct, topClassIs.targetClass)
            }
            
            print("collecting instances data... ")
            val totalInstByGroup = {
                val map = new mutable.HashMap[String, Int] {
                    override def default(key: String) = 0
                }
                
                for(inst <- source; if preFilter(inst)) {
                    val groups = groupFun(inst.mscClasses).distinct
                    for(group <- groups) {
                        map(group) = map(group) + 1
                    }
                }
                
                map.toMap
            }
            println("done")
            
            print("select training instances... ")
            /*val trainingInst = {
                val buffer = new mutable.HashMap[Int, JTreeSet[ArffJsonInstance]] {
                    override def default(key: Int) = new JTreeSet[ArffJsonInstance](
                        new Comparator[ArffJsonInstance]() {
                            def compare(inst1: ArffJsonInstance, inst2: ArffJsonInstance) = 
                                math.signum((inst1 - centroids(15)).norm - (inst2 - centroids(15)).norm).toInt
                        }
                    )
                }
                
                for(inst <- source) {
                    val groups = groupFun(inst.mscClasses)
                    for(group <- groups) {
                        val set = buffer(group.toInt)
                        set.add(inst)
                        
                        val maxInst = if(group.toInt == 15) numTargetInst else numOtherInst
                        
                        if(maxInst.isDefined && set.size > maxInst.get) {
                            set.remove(set.last)
                        }
                        buffer(group.toInt) = set
                    }
                }
                println(buffer.map(kv => (kv._1, kv._2.size)))
                buffer.values.flatten.toList
            }*/
            
           
            val writer = new BufferedWriter(new FileWriter(targetFile))
            writer.write(source.header.toJson + "\n")
            
            var numTrainingInst = 0
            val trainingInst = {
                // val buffer = new mutable.ListBuffer[ArffJsonInstance]
                val currentInstByGroup = new mutable.HashMap[String, Int] {
                    override def default(key: String) = 0
                }
                
                for(inst <- source; if preFilter(inst)) {
                    val targetNumInst = 
                        if(targetClassDef(inst.mscClasses)) numTargetInst
                        else numOtherInst
                    
                    val groups = groupFun(inst.mscClasses).distinct
                    
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
                        
                        val groups = groupFun(inst.mscClasses)
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
        
        new ArffJsonInstancesFile2(
            targetFile,
            source.contentDescription
        )
    }
}

trait TrainSetSelection extends Learner {
    val numTargetInst: Option[Int]
    val numOtherInst: Option[Int]
    
    abstract override def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, set: Option[ContentDescription.Set] = None) = {
        val mappedInst = super.mapInstances(inst, targetClassDef, set)
        if(set == Some(ContentDescription.TrainSet) && numTargetInst.isDefined && numOtherInst.isDefined) {
            applySelection(mappedInst, targetClassDef)
        } else {
            mappedInst
        }
    }
    
    def applySelection(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = TrainSetSelection.applySelection(source, targetClassDef, numTargetInst, numOtherInst)
}







