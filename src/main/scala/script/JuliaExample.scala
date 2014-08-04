package script

import parser.ArffJsonInstancesSource
import common.Path
import common.TrainTuningTestSetSelection
import common.FileManager
import parser.History
import filter.SimpleTextToFeatureVectorFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import classifier.BalancedTrainSetSelection
import classifier.CategoryIsMscSome
import classifier.Classifier
import format.arff_json.Point

object Test6 {
    def main(args: Array[String]) = try {
        // === Allgemeines Setup ===
        
        // rootFolder und Corpus bestimmen / einlesen
        common.Path.rootFolder = "data_mini"
        val corpus = ArffJsonInstancesSource(Path.rootFolder + "/arffJson/corpus.json")
        
        // Trinset, Trainingsset und Testset einlesen
        val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(
            100, 
            "test", 
            corpus, 
            (0.6, 0.2, 0.2)
        )
        
        // Learner definieren
        val svmLearner = SvmLightJniLearner(
            new History() 
                        with AbstractTitleConcat
                        with SimpleTextToFeatureVectorFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix { 
                },
                BalancedTrainSetSelection(Some(10000))
        )
        
        // === Klassifizierung ===
        
        // Ergebnisse der Klassifizierung, mit dem Classifier, der mit dem Lerner "svmLerner" und dem Trainset "trainSet"
        // auf die Kategorie "00" trainiert wurde und anschlietend auf dem Testset "testSet" getestet wurde. 
        val results = svmLearner.classifications(trainSet, testSet, CategoryIsMscSome.top("00"))
        
        // Ergebnisse analysieren mit Precision / Recall  und F-Measure
        println(Classifier.precision(results))
        println(Classifier.recall(results))
        println(Classifier.fMeasure(results, 0.5))
        
        
        // === Zentroid einer Kategorie berechnen ===
        val mscString = "00A05"
        val targetCategory = CategoryIsMscSome(mscString)
        
        // Die Liste von Filtern, die von dem Lerner svmLerner benutzt werden, besorgen
        val filters = svmLearner.filters(trainSet, targetCategory)
        
        // Eine neue ArffJsonInstancesSource erstellen, die nur die Dokumente aus der Zielkategorie enthaelt
        val documentsInTargetCategory = ArffJsonInstancesSource(
            corpus.filter(_.categories.exists(_ == mscString)).toList,      // Das "toList" hier sorgt dafuer, dass die Dokumente tatsaechlich alle in den Arbeitsspeicher geladen werden.
                                                                            // Das ist sinnvoll, wenn nur auf einem Teil des Korpus gearbeitet wird - der gesammte Korpus passt
                                                                            // naemlich nicht in einen normal grossen RAM.
            corpus.header
        )
        
        // Die Dokumente in den Vektorraum des Learners ueberfuehren.
        // Falls du die Syntax nachschlagen willst - das "/:" ist ein "fold-left"
        val mappedDocuments = ((documentsInTargetCategory /: filters)((oldDocuments, filter) => oldDocuments.applyFilter(filter, targetCategory)))
        
        val numDocuments = mappedDocuments.size
        
        val centroidOfTargetCategory = (
            mappedDocuments
                .toList.asInstanceOf[List[Point]]       // die gemappten Dokumente in eine Liste von Punkten konvertieren
                .reduceLeft(_ + _)                      // Alle Punkte Aufaddieren
                .sparseData.mapValues(_ / numDocuments) // die Werte des resultierenden Punkts jeweils duch die Anzahl Dokumente teilen 
        )
        
        println(centroidOfTargetCategory)
    } finally {
        FileManager.quit
    }
}











