package common
import parser.ArffJsonInstancesSource
import java.io.File
import scala.collection.mutable
import classifier.CategoryIs
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonInstancesFileWriter
import format.arff_json.ArffJsonHeader
import parser.ContentDescription

object TrainTuningTestSetSelection {
    def getSets(minDocumentsPerCategoryThreshold: Int, name: String, base: ArffJsonInstancesSource) = {
        val metaDataFile = new File("data/" + name + ".metadata")
        
        if(metaDataFile.exists()) {
	        val metaData = ObjectToFile.readObjectFromFile(metaDataFile).asInstanceOf[Map[String, Any]]
	        
	        val trainSetCD = metaData("trainSetCD").asInstanceOf[ContentDescription]
	        val tuningSetCD = metaData("tuningSetCD").asInstanceOf[ContentDescription]
	        val testSetCD = metaData("testSetCD").asInstanceOf[ContentDescription]
	        val consideredCategories = metaData("consideredCategories").asInstanceOf[Set[String]]
	        
	        ((ArffJsonInstancesSource(trainSetCD), ArffJsonInstancesSource(tuningSetCD), ArffJsonInstancesSource(testSetCD)), consideredCategories)
        } else {
	        val countPerCat = {
	            val map = new mutable.HashMap[String, Int]() {
	                override def default(key: String) = 0
	            }
	            
	            for(inst <- base; cat <- inst.categories.filter(c => c.substring(2, 3) != "-" && c.substring(3, 5).toLowerCase != "xx")) {
	                map(cat) += 1
	            }
	            
	            map.toMap
	        }
	        
	        val consideredCategories = countPerCat.filter(xy => xy._2 >= minDocumentsPerCategoryThreshold).map(_._1).toSet
	        
	        if(!(
	            ContentDescription(name, ContentDescription.TrainSet, List()).file.exists() && 
	            ContentDescription(name, ContentDescription.TuningSet, List()).file.exists() && 
	            ContentDescription(name, ContentDescription.TestSet, List()).file.exists()
	        )) {
	            val numInstances = base.numInstances
	            
	            val mean = {
	                val x = consideredCategories.map(c => countPerCat(c))
	                x.sum.toDouble / x.size
	            }
	            
	            val stdDev = {
	                val x = consideredCategories.map(c => math.abs(countPerCat(c) - mean))
	                x.sum.toDouble / x.size
	            }
	            
	            class InstancesSet(val targetRate: Double, val header: ArffJsonHeader, val contentDescription: ContentDescription) {
	                val writer = new ArffJsonInstancesFileWriter(header, contentDescription)
	                val stat = new mutable.HashMap[String, Int] {
	                    override def default(key: String) = 0
	                }
	                
	                def numInst = writer.numInstances
	                
	                def +=(inst: ArffJsonInstance) {
	                    writer += inst
	                    for(cat <- inst.categories) {
	                        stat(cat) += 1
	                    }
	                }
	                
	                def close() {
	                    writer.close
	                }
	            }
	            
	            val instancesSets = List(
	                new InstancesSet(0.6, base.header, ContentDescription(name, ContentDescription.TrainSet, List())),
	                new InstancesSet(0.2, base.header, ContentDescription(name, ContentDescription.TuningSet, List())),
	                new InstancesSet(0.2, base.header, ContentDescription(name, ContentDescription.TestSet, List()))
	            )
	            
	            for(inst <- base) {
	                val probabilityDist = {
	                    val probabilityDistByTotal = probabilityDistribution(instancesSets.map(_.numInst), instancesSets.map(_.targetRate))
	                
	                    val x = (for(cat <- inst.categories.filter(c => consideredCategories.contains(c))) yield {
	                        val probabilityDistByCat = probabilityDistribution(instancesSets.map(_.stat(cat)), instancesSets.map(_.targetRate))
	                        val rar = rareness(countPerCat(cat), mean, stdDev)
	                        (probabilityDistByCat, rar)
	                    })
	                    
	                    if(x.isEmpty) {
	                        probabilityDistByTotal
	                    } else {
	                        val (probabilityDistByCat, rar) = x.maxBy(_._2)
	                        (probabilityDistByCat.map(_ * rar) zip probabilityDistByTotal.map(_ * (1-rar))).map(x => x._1 + x._2)
	                    }
	                }
	                
	                val t = target(probabilityDist)
	                
	                instancesSets(t) += inst
	            }
	            
	            
	            for(set <- instancesSets) {
	                set.close
	            }
	        }
	        
	        val trainSetCD = ContentDescription(name, ContentDescription.TrainSet, List())
	        val tuningSetCD = ContentDescription(name, ContentDescription.TuningSet, List())
	        val testSetCD = ContentDescription(name, ContentDescription.TestSet, List())
	        
	        val metaData = Map[String, Any](
	            "trainSetCD" -> trainSetCD,
	            "tuningSetCD" -> tuningSetCD,
	            "testSetCD" -> testSetCD,
	            "consideredCategories" -> consideredCategories
	        )
	        
	        ObjectToFile.writeObjectToFile(metaData, metaDataFile)
	        ((ArffJsonInstancesSource(trainSetCD), ArffJsonInstancesSource(tuningSetCD), ArffJsonInstancesSource(testSetCD)), consideredCategories)
        }
    } 
    
    def target(probabiltyDistribution: Iterable[Double]) = {
        val random = math.random
        val summedProbabilities = ((List().asInstanceOf[List[Double]] /: probabiltyDistribution)((old, d) => old :+ (if(old.isEmpty) d else (old.last + d)))).zipWithIndex
        
        val target = {
            val x = summedProbabilities.find(_._1 >= random)
            if(x.isDefined) x.get._2 else 0
        }
        
        target
    }
    
    def rareness(n: Int, mean: Double, stdDev: Double) = {
        val maxRareness = 2.0
        
        val erg = {
            val a = (n-mean) / stdDev
            val b = (if(a > maxRareness) maxRareness else if(a < -maxRareness) -maxRareness else a)
            1 - ((b + maxRareness) / (2*maxRareness))
        }
        
        erg
    }
    
    def probabilityDistribution(numInstances: Iterable[Int], targetRates: Iterable[Double]) = {
        val currentRates = {
            val sum = numInstances.map(_ + 0.001).reduceLeft(_ + _)
            numInstances.map(n => ((n + 0.001) / sum))
        }
        val rateDiffs = (targetRates zip currentRates).map(x => x._1 - x._2)
        
        val normedProbabilities = {
            val min = rateDiffs.min
            val minusMin = rateDiffs.map(n => ((n - min) + 0.001))
            val sum = minusMin.reduceLeft(_ + _)
            minusMin.map(_ / sum)
        }
        
        normedProbabilities
    }
}






































