package script
import java.io.File
import model.RawClassification
import parser.ArffJsonInstancesSource
import filter.CategorySelectionFilter
import classifier.CategoryIs
import classifier.TargetClassDefinition
import scala.collection.mutable
import java.io.BufferedWriter
import java.io.FileWriter

object Test {
    def main(args: Array[String]) {
        val inst = ArffJsonInstancesSource(new File("data/arffJson/corpus.json"))
        val targetClassDef = CategoryIs("05C20")
        
        val selectionFilter = new CategorySelectionFilter(targetClassDef.parent)
        
        trainSetSelection(inst.applyFilter(selectionFilter), targetClassDef, 6000)
    }
    
    def trainSetSelection(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, targetNumInstances: Int) = {
        val minimumRateNegatives = 0.25
        val parentCategory = targetClassDef.asInstanceOf[CategoryIs].parent
        
        val (numPositivesMainClass, numPositivesSecondaryClass, numNegatives) = (
            ((0, 0, 0) /: source)((currentTuple, currentInstance) => {
                if(targetClassDef(List(currentInstance.categories(0)))) (currentTuple._1 + 1, currentTuple._2, currentTuple._3)
                else if(targetClassDef(currentInstance.categories)) (currentTuple._1, currentTuple._2 + 1, currentTuple._3)
                else (currentTuple._1, currentTuple._2, currentTuple._3 + 1)
            })
        )
        
        val (negativesDistribution, numNegativeCategories) = {
            val map = new mutable.HashMap[String, Int]() {
                override def default(key: String) = 0
            }
            
            for(inst <- source.filter(i => !targetClassDef(i.categories)); cat <- inst.categories.filter(c => parentCategory(List(c)))) {
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
        
        val targetNumPerNegativeCategory = targetNumNegatives / numNegativeCategories
        def propForNegatives(categories: List[String]) = {
            categories.filter(c => parentCategory(List(c))).map(c => (targetNumPerNegativeCategory.toDouble / negativesDistribution(c))).max
        }
        
        val instancesIterator = source.iterator.filter(inst => {
            if(targetClassDef(List(inst.categories(0)))) (if(math.random < (targetNumPositivesMainClass.toDouble / numPositivesMainClass)) {i += 1; true} else false)
            else if(targetClassDef(inst.categories)) (if(math.random < (targetNumPositivesSecondaryClass.toDouble / numPositivesSecondaryClass)) {j += 1; true} else false)
            else (if(math.random < propForNegatives(inst.categories)) {k += 1; true} else false)
            })
        
        val writer = new BufferedWriter(new FileWriter("selection"))
        writer.write(source.header.toJson + "\n")
        for(inst <- instancesIterator) {
            writer.write(inst.toJson + "\n")
        }
        writer.close()
        
        println("written instances: " + (i, j, k))
    }
}




















