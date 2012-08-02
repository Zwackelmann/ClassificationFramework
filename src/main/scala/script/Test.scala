package script
import scala.collection.JavaConversions._
import model.RawClassification
import parser.ArffJsonInstancesFile
import parser.PaperJsonFile
import org.tartarus.snowball.ext.PorterStemmer
import format.arff_json.ArffJsonInstance
import filter.feature_scoreing.OddsRatio
import filter.NominalValueFromDictFilter
import java.io.File
import filter.OddsRatioFilter
import classifier.TopClassIs
import classifier.WekaClassifier
import new_approach.classifier.ClassifierGenerator
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import weka.classifiers.bayes.NaiveBayes
import new_approach.classifier.Classifier
import filter.NominalizeFilter
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.NominalArffJsonAttribute
import parser.ArffJsonInstancesFile2

object Test {
    def main(args: Array[String]) {
        val (trainInstances, testInstances) = {
            lazy val trainSource = new ArffJsonInstancesFile("final", "train", List()).project(List(3), "projected")
            lazy val testSource = new ArffJsonInstancesFile("final", "test", List()).project(List(3), "projected")
            
            val nomFilterFile = new File("nominalize")
            lazy val nomFilter = if(!nomFilterFile.exists()) {
                val f = new NominalizeFilter.Conf1("nominal")
                f.expandDict(trainSource)
                f.save(nomFilterFile)
                f
            } else {
                NominalizeFilter.load(nomFilterFile)
            }
            
            val trainInstances = if(!new File("trainSource").exists) {
                val flatMapNominalInstances = nomFilter
                    .applyFilter(trainSource)
                    .map(
                        (inst: Iterator[ArffJsonInstance]) => { inst.flatMap( i => {
                            i.data.map(d => new DenseArffJsonInstance(i.id, i.mscClasses, List(d)))
                        })},
                        (header: ArffJsonHeader) => header,
                        "flatMap"
                    )
            
                flatMapNominalInstances.save(new File("trainSource"))
                flatMapNominalInstances
            } else {
                new ArffJsonInstancesFile2(new File("trainSource"))
            }
             
            val testInstances = if(!new File("testSource").exists) {
                val flatMapNominalTestInstances = nomFilter
                    .applyFilter(testSource)
                    .map(
                        (inst: Iterator[ArffJsonInstance]) => { inst.flatMap( i => {
                            i.data.map(d => new DenseArffJsonInstance(i.id, i.mscClasses, List(d)))
                        })},
                        (header: ArffJsonHeader) => header,
                        "flatMap"
                    )
                flatMapNominalTestInstances.save(new File("testSource"))
                flatMapNominalTestInstances
            } else {
                new ArffJsonInstancesFile2(new File("testSource"))
            }
            
            (trainInstances, testInstances)
        }
        
        for(topClass <- common.Common.topClasses) {
            println("\n\ntopClass: " + topClass)
            
            // println(flatMapNominalInstances.take(100).mkString("\n"))
            
            /* val specificJournalCounts = new scala.collection.mutable.HashMap[String, Int] { override def default(key: String) = 0 }
            for(inst <- trainSource if inst.mscClasses.exists(_.substring(0, 2) == topClass.toString); journal <- inst2Words(inst).map(wordFun(_))) yield {
                specificJournalCounts(journal) = specificJournalCounts(journal) + 1
            }
            
            val totalJournalCounts = new scala.collection.mutable.HashMap[String, Int] { override def default(key: String) = 0 }
            for(inst <- trainSource; journal <- inst2Words(inst).map(wordFun(_))) yield {
                totalJournalCounts(journal) = totalJournalCounts(journal) + 1
            }
            
            val keys = specificJournalCounts.keySet
            
            val nominalInstancesAttributeNames = nominalInstances.header.attributes.map(_.name)
            val nominalInstancesIndexesFromName = nominalInstancesAttributeNames.zipWithIndex.toMap
            
            val foobar = (for(key <- keys if key != "") yield {
                (key, nominalInstancesIndexesFromName(key), specificJournalCounts(key), totalJournalCounts(key), specificJournalCounts(key).toDouble / totalJournalCounts(key))
            }).toList.sortWith((a, b) => a._5 > b._5).filter(a => a._5 < 1.0 && a._5 > 0.5)
            
            val ids = foobar.map(_._2)
            
            val naiveBayesTrainBase = nominalInstances.project(ids, "best-")*/
            
            /*val orFilter = new OddsRatioFilter(nominalInstances, 10, "or")
            val orFiltered = orFilter.applyFilter(nominalInstances, TopClassIs(topClass.toString))
            val orFilteredAttributeNames = orFiltered.header.attributes.map(_.name)*/
            
            /*val bayesClassifierGen = new ClassifierGenerator() {
                def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
                    new WekaClassifier(inst, TopClassIs(topClass.toString)){
                        def classifierConfig() = new NaiveBayes()
                    }
                }
                def fileAppendix: String = "nb-" + topClass
                def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): ArffJsonInstancesSource = inst
            }*/
            
            val classifier = new WekaClassifier(trainInstances, TopClassIs(topClass.toString)) {
                def classifierConfig() = new NaiveBayes()
            }
            val classifications = classifier.classifications(testInstances).toList
            
            val groupedClassifications = classifications.groupBy(c => c.id)
            
            def aggregateClassifications(groupedClassifications: Map[String, List[RawClassification]])(fun: (List[Double]) => Double) = {
                groupedClassifications.values.toList.map(cl => new RawClassification(
                    cl(0).id, 
                    fun(cl.map(_.classification)),
                    cl(0).realValue
                ))
            }
            
            val curriedAggregate = aggregateClassifications(groupedClassifications) _
            
            val aggregatedByMax = curriedAggregate((l: List[Double]) => l.reduceLeft((a, b) => math.max(a, b)))
            val aggregatedByAvg = curriedAggregate((l: List[Double]) => l.reduceLeft((a, b) => a + b) / l.size)
            
            /*val bayesClassifier = new WekaClassifier(flatMapNominalInstances, TopClassIs(topClass.toString)) {
                def classifierConfig() = new NaiveBayes()
            }
            
            val classifications = bayesClassifier.calculateClassifications(flatMapNominalInstances).toList
            */
            
            /*val classifications = bayesClassifierGen.classifications(naiveBayesTrainBase, TopClassIs(topClass.toString))*/
            
            val precisionByMax = Classifier.precision(aggregatedByMax, 0)
            val precisionByAvg = Classifier.precision(aggregatedByAvg, 0)
            
            val recallByMax = Classifier.recall(aggregatedByMax, 0)
            val recallByAvg = Classifier.recall(aggregatedByAvg, 0)
            
            println("precision: max: %1.5f | avg: %1.5f | diff: %1.5f".format(precisionByMax, precisionByAvg, (precisionByMax - precisionByAvg)))
            println("recall:    max: %1.5f | avg: %1.5f | diff: %1.5f".format(recallByMax, recallByAvg, (recallByMax - recallByAvg)))
            
            //val sets = List(("unaggregated", classifications), ("aggregated by max", aggregatedByMax), ("aggregated by avg", aggregatedByAvg))
            
            /*for(set <- sets) {
                println("\nresults for " + set._1 + ":")
                val classifications = set._2
                
                Classifier.precision(classifications, 0)
                
                val falseNegatives = new scala.collection.mutable.ListBuffer[Double]
                val trueNegatives = new scala.collection.mutable.ListBuffer[Double]
                for(c <- classifications) {
                    if(c.falseNegative) falseNegatives += c.classification
                    else if(c.trueNegative) trueNegatives += c.classification
                    else if(c.falsePositive) {
                        // println("falsePositive")
                    } else {
                        // throw new RuntimeException("true positive")
                    }
                }
                
                println("falseNegatives: " + (falseNegatives.reduceLeft(_ + _) / falseNegatives.size))
                println("trueNegatives: " + (trueNegatives.reduceLeft(_ + _) / trueNegatives.size))
            }*/
                
            /*println({for(instInst <- nominalInstances.zip(trainSource).take(100)) yield {
                val journalNames = instInst._1.sparseData.map((kv) => nominalInstancesAttributeNames(kv._1))
                (instInst._2, journalNames)
            }}.take(100).mkString("\n"))*/
            
            /*
            
            /*println({for(instInst <- orFiltered.zip(trainSource).take(100)) yield {
                val journalNames = instInst._1.sparseData.map((kv) => orFilteredAttributeNames(kv._1))
                (instInst._2, journalNames)
            }}.take(100).mkString("\n"))*/
            
            println(orFilteredAttributeNames.map(name => (name, specificJournalCounts(name), totalJournalCounts(name))))*/
        }
    }
    
    def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].toSeq
    def wordFun(word: String) = word.filter(_.isLetter).toLowerCase()
    
    def levenshteinDistance(s: String, t: String) = {
        // for all i and j, d[i,j] will hold the Levenshtein distance between
        // the first i characters of s and the first j characters of t;
        // note that d has (m+1)x(n+1) values
        
        val m = s.size
        val n = t.size
        
        var d: Array[Array[Int]] = Array.ofDim(m+1, n+1)
        for(i <- 0 until m+1) {
            for(j <- 0 until n+1) {
                d(i)(j) = 0
            }
        }
        
        // source prefixes can be transformed into empty string by
        // dropping all characters 
        for(i <- 0 until m) {
            d(i)(0) = i
        }

        // target prefixes can be reached from empty source prefix
        // by inserting every characters
        for(j <- 0 until n) {
            d(0)(j) = j
        }
        
        for(j <- 1 until n+1) {
            for(i <- 1 until m+1) {
                if(s(i-1) == t(j-1)) {
                    d(i)(j) = d(i-1)(j-1)
                } else {
                    d(i)(j) = List(
                        d(i-1)(j) + 1,
                        d(i)(j-1),
                        d(i-1)(j-1) + 1
                    ).min
                }
            }
        }
        
        d(m)(n)
    }
}