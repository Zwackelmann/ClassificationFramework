package classifier
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import scala.collection.mutable
import format.arff_json.ArffJsonInstance

trait TrainSetSelection extends Learner {
    val numTargetInst: Option[Int]
    val numOtherInst: Option[Int]
    
    abstract override def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, set: Option[ContentDescription.Set] = None) = {
        val mappedInst = super.mapInstances(inst, targetClassDef, set)
        if(set == Some(ContentDescription.TrainSet)) {
            applySelection(mappedInst, targetClassDef)
        } else {
            mappedInst
        }
    }
    
    def applySelection(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        println("train-set-selection: (" + numTargetInst + ", " + numOtherInst + ")")
        // val validClasses = List(18, 19, 80, 13, 16, 17, 55, 97, 22, 20, 57, 1, 12, 14, 85, 74, 76, 54, 93, 53, 15)
        
        val (groupFun, targetGroup) = targetClassDef match {
            case topClassIs: TopClassIs => ((l: List[String]) => l.map(_.substring(0, 2)).distinct, topClassIs.targetClass)
        }
        val totalInstByGroup = {
            val map = new mutable.HashMap[String, Int] {
                override def default(key: String) = 0
            }
            
            for(inst <- source) {
                val groups = groupFun(inst.mscClasses).distinct
                for(group <- groups) {
                    map(group) = map(group) + 1
                }
            }
            
            map.toMap
        }
        
        val trainingInst = {
            val iter = source.iterator
            val buffer = new mutable.ListBuffer[ArffJsonInstance]
            val currentInstByGroup = new mutable.HashMap[String, Int] {
                override def default(key: String) = 0
            }
            
            for(inst <- source) {
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
                    buffer += inst
                    
                    val groups = groupFun(inst.mscClasses)
                    for(group <- groups) {
                        currentInstByGroup(group) = currentInstByGroup(group) + 1
                    }
                }
            }
            
            // println("average numInst by group: " + (currentInstByGroup.values.reduceLeft(_ + _).toDouble / currentInstByGroup.values.size))
            // println("total instances for training: " + buffer.size)
            // println(currentInstByGroup.mkString("\n"))
            buffer.toList
        }
        
        ArffJsonInstancesSource(
            trainingInst,
            source.header,
            source.contentDescription
        )
    }
}







