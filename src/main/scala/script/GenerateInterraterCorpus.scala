package script

import parser.ArffJsonInstancesSource
import common.Path
import common.TrainTuningTestSetSelection
import classifier.CategoryIsMscSome
import classifier.CategorizationHierarchy
import classifier.CategoryIs
import parser.History
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.BalancedTrainSetSelection
import model.RawClassification
import common.FileManager
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.File
import scala.io.Source
import scala.collection.mutable

object GenerateInterraterCorpus {
    def main(args: Array[String]) { try {
        common.Path.rootFolder = "data_try_train_set_selection"
        
        val corpusFilename = "corpus.json"
        
        val confusions = Map(
            53 -> 58,
            58 -> 53,
            16 -> 13,
            13 -> 16,
            20 -> 22,
            22 -> 20
        )
        
        val targetNumDocsPerClass = 50
        val targetFilename = "interrater-consistency-corpus.json"
        
        val learner = SvmLightJniLearner(
            new History() 
                    with AbstractTitleConcat
                    with VectorFromDictFilter.Appendix
                    with TfIdfFilter.Appendix
                    with NormalizeVectorFilter.Appendix { 
                val confName = "conf11"
                override val minOcc = 3
            },
            BalancedTrainSetSelection(Some(10000))
        )
        
        println("calculate statistics...")
        val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/" + corpusFilename)
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "prod", corpus, (0.7, 0.3, 0.0))
        
        val writer = new BufferedWriter(new FileWriter(new File(targetFilename)))
        val usedDocumentIds = new mutable.ListBuffer[String]
        
        for((actualClass, confusionClass) <- confusions) {
            println(actualClass, confusionClass)
            val r = learner.classifications(
                trainSet,
                tuningSet
                    .filter(inst => inst.categories.exists(_.substring(0, 2) == actualClass.toString))
                    .filterNot(inst => inst.categories.exists(_.substring(0, 2) == confusionClass.toString)),
                CategoryIsMscSome.top(confusionClass)
            )
            
            val docMetadata = r.sortWith((a, b) => a.classification > b.classification).filter(_.classification > 0).map(x => (x.id, x.classification, x.realValue))
            val docs = corpus.filter(d => docMetadata.map(_._1).contains(d.id)).toList
            
            val res = (for(docMetadata <- docMetadata; doc <- docs if docMetadata._1 == doc.id) yield (docMetadata, doc)) 
            val prob = targetNumDocsPerClass.toDouble / res.size
            val chosenDocs = res.filter(_ => math.random < prob)
            
            usedDocumentIds ++= chosenDocs.map(_._1._1)
            
            writer.write(
                chosenDocs
                    .map(doc => 
                        """{ "id" : "%s", "classes" : %s, "target-class" : %d, "confusion-class" : %d, "confusion-score" : %f, "title" : "%s", "abstract" : "%s" }"""
                            .format(
                                doc._1._1,
                                "[" + doc._2.categories.map(c => "\"" + c + "\"").mkString(", ") + "]",
                                actualClass,
                                confusionClass,
                                doc._1._2,
                                common.Common.escape(doc._2.dataAt(0).toString),
                                common.Common.escape(doc._2.dataAt(1).toString)
                            )
                    )
                    .mkString("\n")
            )
        }
        
        val numUsedDocuments = usedDocumentIds.size
        val numInstances = corpus.numInstances
        
        writer.write(
            corpus
                .filterNot(doc => usedDocumentIds.contains(doc.id))
                .filter(_ => math.random < (numUsedDocuments.toDouble / numInstances))
                .map(doc => 
                    """{ "id" : "%s", "classes" : %s, "title" : "%s", "abstract" : "%s" }"""
                        .format(
                            doc.id,
                            "[" + doc.categories.map(c => "\"" + c + "\"").mkString(", ") + "]",
                            common.Common.escape(doc.dataAt(0).toString),
                            common.Common.escape(doc.dataAt(1).toString)
                        )
                )
                .mkString("\n")
        )
            
        writer.close
    } finally {
         FileManager.quit()
    }}
}





















