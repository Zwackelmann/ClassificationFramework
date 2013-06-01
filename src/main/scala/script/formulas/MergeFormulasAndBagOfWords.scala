package script.formulas

import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import scala.io.Source
import scala.collection.mutable
import java.nio.charset.MalformedInputException
import parser.ArffJsonInstancesSource
import java.io.BufferedWriter
import java.io.FileWriter
import common.FileManager
import format.arff_json.ArffJsonInstance
import filter.VectorFromDictFilter
import format.arff_json.SparseData
import script.SvmLightJniLearner
import parser.History
import classifier.BalancedTrainSetSelection
import filter.TfIdfFilter
import classifier.CategoryIs
import script.ApplyFinalClassifier
import common.Path
import common.TrainTuningTestSetSelection
import model.RawClassification
import filter.NormalizeVectorFilter
import classifier.CategoryIsMSC

object MergeFormulasAndBagOfWords {
    def id2ANMappings = {
        val s = new BufferedReader(new FileReader(new File("cikm/zbl_full_dmo.txt")))
        
        var an: String = null
        var id: String = null
        val mapping = new mutable.HashMap[String, String]
        var line: String = null
        
        while({line = s.readLine(); line != null}) {
            if(line.length > 5) {
                if(line.substring(0, 4) == ":an:") {
                    val anCandidate = line.substring(5, line.length())
                    if(anCandidate.length > 1) {
                        an = anCandidate
                        if(an != null && an.length > 1 && id != null && id.length > 1) {
                            mapping(id) = an
                        }
                        an = null
                    } else {
                        an = null
                    }
                } else if(line.substring(0, 4) == ":id:") {
                    id = line.substring(5, line.length())
                }
            }
        }
        
        mapping.toMap
    }
    
    
    def findRelativeAmountOfIntersectionDocuments() = {
        val knownIdsForTextFeatures = {
            val m = id2ANMappings
            m.values.toSet
        }
        
        val knownIdsForMathFeatures = {
            val docReader = new ReadDocuments.DocReader(new File("C:/Users/Simon/Desktop/document_matrix.txt"))
            docReader.map(_._1).toSet
        }
        
        val intersect = knownIdsForTextFeatures intersect knownIdsForMathFeatures
        
        println(knownIdsForMathFeatures.size)
        println(intersect.size)
        println(intersect.size / knownIdsForMathFeatures.size.toDouble)
    }
    
    def filterCorpusToThoseDocumentsWhereMathFeaturesAreAvailable() = {
        val corpus = ArffJsonInstancesSource("data_fiz_full/arffJson/fiz_full.json")
        val id2anMap = id2ANMappings
        
        val knownIdsForMathFeatures = {
            val docReader = new ReadDocuments.DocReader(new File("C:/Users/Simon/Desktop/document_matrix.txt"))
            docReader.map(_._1).toSet
        }
        
        val header = corpus.header
        
        val w = new BufferedWriter(new FileWriter(new File("text-corpus-for-math-formulas.json")))
        w.write(header.toJson + "\n")
        
        for(doc <- corpus) {
            id2anMap.get(doc.id) match {
                case Some(an) if(knownIdsForMathFeatures.contains(an)) => w.write(ArffJsonInstance(an, doc.categories, doc.data.toList, false).toJson + "\n") 
                case _ => 
            }
        }
        
        w.close
    }
    
    def checkValidityOfTextCorpus() {
        val corpus = ArffJsonInstancesSource("text-corpus-for-math-formulas.json")
        
        val textCorpusIds = corpus.map(_.id).toSet
        val mathCorpusIds = new ReadDocuments.DocReader(new File("C:/Users/Simon/Desktop/document_matrix.txt")).map(_._1).toSet
        
        val intersect = textCorpusIds intersect mathCorpusIds
        
        println(textCorpusIds.size)
        println(mathCorpusIds.size)
        println(intersect.size / textCorpusIds.size.toDouble)
    }
    
    def createBagOfWordsForTextCorpus() {
        common.Path.rootFolder = "text-math-corpus"
        val corpus = ArffJsonInstancesSource(common.Path.rootFolder + "/arffJson/corpus.json")
        val bowFilter = VectorFromDictFilter("conf10", 1)(corpus)
        val mappedCorpus = bowFilter.applyFilter(corpus)
        mappedCorpus.save("mappedCorpus")
    }
    
    def unionTextFeraturesAndFormulaFeatures() {
        val textCorpus = ArffJsonInstancesSource("text-math-corpus/arffJson/corpus-only-bow.json")
        val numTextAttributes = textCorpus.header.numAttributes
        
        val mathCorpus = new ReadDocuments.DocReader(new File("C:/Users/Simon/Desktop/document_matrix.txt"))
        val numMathAttributes = 50000
        val mathCorpusAttributeMap = {
            val bestAttributes = ReadMSCOnIndexNodes.bestAttributes(numMathAttributes).toSet
            val attributes = new mutable.HashSet[Int]
            for(doc <- mathCorpus) {
                attributes ++= doc._2.keys.filter(i => bestAttributes.contains(i))
            }
            attributes.toList.sortBy(i => i).zipWithIndex.toMap
        }
        val mathCorpusDataMap = mathCorpus.toMap
        val totalNumAttributes = numTextAttributes + numMathAttributes
        
        println("numTextAttributes: " + numTextAttributes)
        
        val headerString = "{ \"relation-name\" : \"math-formulas + text\", \"num-attributes\" : " + totalNumAttributes + "}"
        
        val writer = new BufferedWriter(new FileWriter(new File("merged-corpus-discrete.json")))
        writer.write(headerString + "\n")
        for(doc <- textCorpus) {
            val textData = doc.asInstanceOf[ArffJsonInstance with SparseData].dataMap
            val mathData = mathCorpusDataMap(doc.id).filter(kv => mathCorpusAttributeMap.contains(kv._1)).map(kv => (mathCorpusAttributeMap(kv._1) + numTextAttributes) -> (if(kv._2 >= 1.0) 1.0 else 0.0))
            
            writer.write(ArffJsonInstance(doc.id, doc.categories, textData ++ mathData, totalNumAttributes).toJson + "\n")
        }
        writer.close()
    }
    
    def classify() {
        Path.rootFolder = "text-math-corpus"
        
        val corpusMeta = // ("corpus-only-bow.json", "only-bow")
            // ("corpus-merged.json", "merged")
            ("merged-corpus-discrete.json", "merged-descrete")
        
        val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/" + corpusMeta._1)
        val ((trainSet, tuningSet, testSet), _) = TrainTuningTestSetSelection.getSets(100, corpusMeta._2, corpus, (0.7, 0.3, 0.0))
        
        val histogram = new mutable.HashMap[String, Int]() {
            override def default(key: String) = 0
        }
        for(doc <- trainSet; cat <- doc.categories.map(c => if(c.length >= 2) c.substring(0, 2) else "").distinct) {
            histogram(cat) = histogram(cat) + 1
        }
        
        val learner = List((SvmLightJniLearner(
            new History(),
            BalancedTrainSetSelection(Some(10000))
        ), "flat"), (SvmLightJniLearner(
            new History() with TfIdfFilter.Appendix,
            BalancedTrainSetSelection(Some(10000))
        ), "tfidf"), (SvmLightJniLearner(
            new History() with NormalizeVectorFilter.Appendix,
            BalancedTrainSetSelection(Some(10000))
        ), "norm"), (SvmLightJniLearner(
            new History() with TfIdfFilter.Appendix with NormalizeVectorFilter.Appendix,
            BalancedTrainSetSelection(Some(10000))
        ), "tfidf+norm"))
        
        val firstLevelClasses = 
            List("05", "11", "14", "16", "20", "30", "32", "34", "35", "45", "53", "60", "68")
            .map(CategoryIsMSC.top(_))
            
        for((l, name) <- learner) {
            println("start " + name)
            val b = new StringBuffer()
            var count = 0
            var macroFMeasureAccu = 0.0
            
            for((cat, i) <- firstLevelClasses.zipWithIndex) {
                ApplyFinalClassifier.printProgress(i, firstLevelClasses.size)
                
                val r = l.classifications(trainSet, tuningSet, cat)
                
                val precRecPoints = RawClassification.precRecGraphPoints(r)
                val bestF = FormulaClassifier.bestFmeasure(precRecPoints)
                
                val reportStr = "[" + 
                    "\"topClass\" : \"" + cat.topClass.get + "\", " + 
                    "\"numTrainInstances\" : " + histogram(cat.filenameExtension.substring(0, 2)) + ", " + 
                    "\"bestF\" : " + bestF + ", " +  
                    "\"precRecGraphPoints\" : " + "[" + precRecPoints.map(p => p._2).mkString(",") + "]" +
                "]\n"
                
                println(reportStr)
                b.append(reportStr)
                count += 1
                macroFMeasureAccu += bestF
            }
        
            val writer = new BufferedWriter(new FileWriter(new File(corpusMeta._2 + " " + name)))
            writer.write("[\"makro-f\" : " + (macroFMeasureAccu / count) + "]\n")
            writer.write(b.toString())
            
            writer.close
        }
        
        println("all finished")
    }
    
    def main(args: Array[String]) {
        try {
            classify()
        } finally {
            FileManager.quit
        }
    }
}





















