package script

import parser.ArffJsonInstancesSource
import parser.History
import filter.CategorySelectionFilter
import filter.VectorFromDictFilter
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import format.arff_json.ArffJsonHeader
import classifier.CategoryIs
import classifier.NoTrainSetSelection
import format.arff_json.ArffJsonInstance
import parser.ContentDescription
import common.FileManager
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object AllClassesForTIBData {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource("data/arffJson/corpus.json")
        
        val trainSet = ArffJsonInstancesSource(ContentDescription("min100", ContentDescription.TrainSet, List())).get
        val testSet = ArffJsonInstancesSource(ContentDescription("min100", ContentDescription.TestSet, List())).get
        val tuningSet = ArffJsonInstancesSource(ContentDescription("min100", ContentDescription.TuningSet, List())).get
        val tibCorpus = ArffJsonInstancesSource(ContentDescription("corpusTIB", ContentDescription.TestSet, List())).get
        
        val consideredCats = common.Common.topClasses
        
        val learner = SvmLightJniLearner(
            new History() 
                with AbstractProjection
                with VectorFromDictFilter.Appendix
                with TfIdfFilter.Appendix
                with NormalizeVectorFilter.Appendix { 
                    val confName = "conf9"
                    override val minOcc = 3
                },
            NoTrainSetSelection
        )
        
        val classifiers = (for(cat <- consideredCats.map(c => CategoryIs(c + "-xx"))) yield {
            cat -> learner.classifier(trainSet, cat)
        }).toMap
        
        def escape(str: String) = str.replaceAll("\\s", " ").replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")
        val header = ArffJsonHeader.jsonToArffJsonHeader("""{"relation-name" : "final_format", "attributes" : [{"name" : "title", "type" : "string"}, {"name" : "abstract", "type" : "string"}, {"name" : "journals", "type" : "string"}, {"name" : "terms", "type" : "string"}]}""")
        
        val x = for(doc <- tibCorpus.map(doc => ArffJsonInstance.stringToArffJsonInstance(
            """[["",[]],["%s","%s",[],[]]]""".format(escape(doc.data(0).asInstanceOf[String]), escape(doc.data(1).asInstanceOf[String])),
            header
        ))) yield {
            (doc.id -> (for(cat <- consideredCats.map(c => CategoryIs(c))) yield {
                val classifier = classifiers(cat)
                if(classifier.classifications(testSet)(0).classification >= 0) List(cat)
                else List()
            }).flatten)
        }
        
        val w = new BufferedWriter(new FileWriter(new File("results")))
        
        for(y <- x.iterator) {
            w.write(y + "\n")
        }
        w.close
        FileManager.quit
    }
}