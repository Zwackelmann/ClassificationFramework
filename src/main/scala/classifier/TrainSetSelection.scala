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
import common.FileManager
import FileManager.Protocol._
import common.Gson

object TrainSetSelection {
    import common.Common.verbosity
    
    def preFilter(inst: ArffJsonInstance) = true
    
    def file(source: ArffJsonInstancesSource with ContentDescribable, categoryIs: CategoryIs, trainSetSelectionDef: TrainSetSelectionDefinition) = arffJsonPath / (
        source.contentDescription.filename + "-" + 
        trainSetSelectionDef.filenameAppendix + "-" + 
        categoryIs.filenameExtension +  
        ".json"
    )
    
    def applySelection(source: ArffJsonInstancesSource, categoryIs: CategoryIs, trainSetSelectionDef: TrainSetSelectionDefinition) = {
        val (targetFile, sourceCd) = source match {
            case contentDescribableSource: ContentDescribable => (file(contentDescribableSource, categoryIs, trainSetSelectionDef), contentDescribableSource.contentDescription)
            case source => throw new RuntimeException("train set selection with not content describable instances is not implemented yet")
        }
        
        (FileManager !? CreateOrReceiveFile(targetFile)) match {
            case AcceptCreateFile(fileHandle) => {
                trainSetSelectionDef match {
                    case FixedTrainSetSelection(numTargetInst, numOtherInst) => {
                        if(!categoryIs.isInstanceOf[CategoryIsMsc]) {
                            throw new RuntimeException("FixedTranSetSelection is only implemented for CategoryIsMSC categories")
                            // TODO ...
                        } else {
                            val mscCat = categoryIs.asInstanceOf[CategoryIsMsc]
                            
                            val groupFun = (if(mscCat.targetLevel == 1) {
                                ((l: List[String]) => l.map(_.substring(0, 2)).distinct)
                            } else if(mscCat.targetLevel == 2) {
                                ((l: List[String]) => l.map(_.substring(0, 3)).distinct)
                            } else if(mscCat.targetLevel == 3) {
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
                            
                            val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                            
                            var numTrainingInst = 0
                            val trainingInst = {
                                val currentInstByGroup = new mutable.HashMap[String, Int] {
                                    override def default(key: String) = 0
                                }
                                
                                for(inst <- source; if preFilter(inst)) {
                                    val targetNumInst = 
                                        if(categoryIs.matchesForTraining(inst.categories).get) numTargetInst
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
                            }
                            writer.close()
                            fileHandle.close()
                            println("done")
                            println("total instances for training: " + numTrainingInst)
                        }
                    }
                    
                    case BalancedTrainSetSelection(maxInst) => {
                        if(verbosity >= 1) println("apply balanced train set selection")
                        
                        var numTargetInst = 0
                        var numOtherInst = 0
                        for(inst <- source.iterator) {
                            if(preFilter(inst)) {
                                val isCat = categoryIs.matchesForTraining(inst.categories)
                                if(isCat.isDefined && isCat.get) numTargetInst += 1
                                else if(isCat.isDefined) numOtherInst += 1
                            }
                        }
                        
                        if(verbosity >= 2) println("numTargetInst: " + numTargetInst + ", numOtherInst: " + numOtherInst)
                        
                        val targetSize = {
                            val tmp = math.min(numTargetInst, numOtherInst)
                            if(maxInst.isDefined) math.min(maxInst.get, tmp)
                            else tmp
                        }
                        
                        if(verbosity >= 2) println("target size per category: " + targetSize)
                        
                        var writtenInstances1 = 0
                        var writtenInstances2 = 0
                        
                        val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                        
                        writer.write(Gson.toJson(source.header) + "\n")
                        for(inst <- source.iterator.filter(preFilter).filter(inst => categoryIs.matchesForTraining(inst.categories).isDefined)) {
                            val isCat = categoryIs.matchesForTraining(inst.categories)
                            if(isCat.isDefined && isCat.get && math.random < (targetSize.toDouble/numTargetInst)) {
                                writer.write(inst.toJson + "\n")
                                writtenInstances1 += 1
                            }
                            else if(isCat.isDefined && !isCat.get && math.random < (targetSize.toDouble/numOtherInst)) {
                                writer.write(inst.toJson + "\n")
                                writtenInstances2 += 1
                            }
                        }
                        writer.close()
                        fileHandle.close()
                        
                        if(verbosity >= 2) println("written instances: pos: " + writtenInstances1 + ",  neg: " + writtenInstances2 + ", total: " + (writtenInstances1 + writtenInstances2))
                    }
                    
                    case MaxForEachSet(maxPositives, maxNegatives) => {
                        val (positives, negatives) = source.filter(preFilter).partition(i => categoryIs.matchesForTraining(i.categories).get)
                        val (numPositives, numNegatives) = (positives.size, negatives.size)
                        
                        val positivesIterator = 
                            if(!maxPositives.isDefined) positives.iterator
                            else positives.iterator.filter(i => math.random < (maxPositives.get.toDouble/numPositives))
                        
                        val negativesIterator = 
                            if(!maxNegatives.isDefined) negatives.iterator
                            else negatives.iterator.filter(i => math.random < (maxNegatives.get.toDouble/numNegatives))
                            
                        val instancesIterator = positivesIterator ++ negativesIterator
                        
                        val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                        
                        writer.write(Gson.toJson(source.header) + "\n")
                        for(inst <- instancesIterator) {
                            writer.write(inst.toJson + "\n")
                        }
                        writer.close()
                        fileHandle.close()
                    }
                    
                    case PrioritizeMainClass(targetNumInstances) => {
                        val minimumRateNegatives = 0.25
                        val cat = categoryIs.asInstanceOf[CategoryIs with CategorizationHierarchy]
                        val parentCategory = cat.parent
                        val catSubstring = cat.targetLevel match {
                            case 1 => (s: String) => s.substring(0, 2)
                            case 2 => (s: String) => s.substring(0, 3)
                            case 3 => (s: String) => s
                        }	
                        
                        val (numPositivesMainClass, numPositivesSecondaryClass, numNegatives) = (
                            ((0, 0, 0) /: source)((currentTuple, currentInstance) => {
                                if(currentInstance.categories.isEmpty) (currentTuple._1, currentTuple._2, currentTuple._3)
                                else if(categoryIs.matchesForTraining(List(currentInstance.categories(0))).get) (currentTuple._1 + 1, currentTuple._2, currentTuple._3)
                                else if(categoryIs.matchesForTraining(currentInstance.categories).get) (currentTuple._1, currentTuple._2 + 1, currentTuple._3)
                                else (currentTuple._1, currentTuple._2, currentTuple._3 + 1)
                            })
                        )
                        
                        val (negativesDistribution, numNegativeCategories) = {
                            val map = new mutable.HashMap[String, Int]() {
                                override def default(key: String) = 0
                            }
                            
                            for(inst <- source.filter(i => !categoryIs.matchesForTraining(i.categories).get); cat <- inst.categories.filter(c => parentCategory.matchesForTraining(List(c)).get).map(c => catSubstring(c)).distinct) {
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
                            categories.filter(c => parentCategory.matchesForTraining(List(c)).get).map(c => (targetNumPerNegativeCategory.toDouble / negativesDistribution(catSubstring(c)))).max
                        }
                        
                        val instancesIterator = if((numPositivesMainClass, numPositivesSecondaryClass, numNegatives) == (targetNumPositivesMainClass, targetNumPositivesSecondaryClass, targetNumNegatives)) {
                            source.iterator
                        } else { 
                            source.iterator.filter(inst => {
                                if(inst.categories.isEmpty) { 
                                    false 
                                } else { 
    	                            if(categoryIs.matchesForTraining(List(inst.categories(0))).get) (if(math.random < (targetNumPositivesMainClass.toDouble / numPositivesMainClass)) {i += 1; true} else false)
    	                            else if(categoryIs.matchesForTraining(inst.categories).get) (if(math.random < (targetNumPositivesSecondaryClass.toDouble / numPositivesSecondaryClass)) {j += 1; true} else false)
    	                            else (if(math.random < propForNegatives(inst.categories)) {k += 1; true} else false)
    	                        }
                            })
                        }
                        
                        val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                        
                        writer.write(Gson.toJson(source.header) + "\n")
                        for(inst <- instancesIterator) {
                            writer.write(inst.toJson + "\n")
                        }
                        writer.close()
                        fileHandle.close
                    }
                    
                    case NoTrainSetSelection => throw new RuntimeException("Should not occur")
                }
            }
            
            case AcceptReceiveFile(file) =>
        }
        
        ArffJsonInstancesSource(
            targetFile,
            sourceCd
        )
    }
}

trait TrainSetSelection extends Learner {
    val trainSetSelectionDef: TrainSetSelectionDefinition
    
    abstract override def mapInstances(trainInst: ArffJsonInstancesSource, tuningInst: ArffJsonInstancesSource, cat: CategoryIs) = {
        val mappedInst = super.mapInstances(trainInst, tuningInst, cat)
        if(trainInst == tuningInst && trainSetSelectionDef != NoTrainSetSelection) {
            applySelection(mappedInst, cat)
        } else {
            mappedInst
        }
    }
    
    def applySelection(source: ArffJsonInstancesSource, categoryIs: CategoryIs) = TrainSetSelection.applySelection(source, categoryIs, trainSetSelectionDef)
}







