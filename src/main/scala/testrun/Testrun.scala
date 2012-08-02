package testrun
import java.io.File
import conversion.Json2ArffJson
import model.Paper
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.StringArffJsonAttribute
import model.DetailledSource
import filter.generator.WekaFilterGenerator
import weka.filters.Filter
import weka.filters.unsupervised.attribute.StringToWordVector
import weka.core.stemmers.SnowballStemmer
import model.file.FilePlan
import common.Common.resolvePath

object Testrun {
    class PaperJson2AbstractOnlyArffJson(inFile: File, outFile: File) extends Json2ArffJson(inFile, outFile) {
        def paperToArffJsonInstance(paper: Paper) = {
            new DenseArffJsonInstance(
                dataList = List(paper.abstractText),
                id = paper.an._1 + "." + paper.an._2,
                mscClasses = paper.mscClasses
            )
        }
        
        def header = new ArffJsonHeader(
            relationName = "abstract_only",
            attributes = List(new StringArffJsonAttribute("abstract")),
            metaAttributes = List(new StringArffJsonAttribute("an"))
        )
    }
    
    
    class PaperJson2TitleOnlyArffJson(inFile: File, outFile: File) extends Json2ArffJson(inFile, outFile) {
        def paperToArffJsonInstance(paper: Paper) = {
            new DenseArffJsonInstance(
                dataList = List(paper.title),
                id = paper.an._1 + "." + paper.an._2,
                mscClasses = paper.mscClasses
            )
        }
        
        def header = new ArffJsonHeader(
            relationName = "title_only",
            attributes = List(new StringArffJsonAttribute("title")),
            metaAttributes = List(new StringArffJsonAttribute("an"))
        )
    }
    
    
    class PaperJson2JournalOnlyArffJson(inFile: File, outFile: File) extends Json2ArffJson(inFile, outFile) {
        def paperToArffJsonInstance(paper: Paper) = {
            new DenseArffJsonInstance(
                dataList = List(paper.sources.map(_ match {
                    case d: DetailledSource => Some(d.journal)
                    case s => None
                }).filter(_ != None).map(_.get)),
                id = paper.an._1 + "." + paper.an._2,
                mscClasses = paper.mscClasses
            )
        }
        
        def header = new ArffJsonHeader(
            relationName = "JournalOnly",
            attributes = List(new StringArffJsonAttribute("journals")),
            metaAttributes = List(new StringArffJsonAttribute("an"))
        )
    }
    
    class RawStringVectorFilterGenerator extends WekaFilterGenerator {
        def filterConfig(): Filter = {
            val vectorFilter = new StringToWordVector()
            val stemmer = new SnowballStemmer("porter")
            
            vectorFilter.setUseStoplist(true)
            vectorFilter.setStemmer(stemmer)
            
            vectorFilter
        }
    }
    
    class TfIdfStringVectorFilterGenerator extends WekaFilterGenerator {
        def filterConfig(): Filter = {
            val vectorFilter = new StringToWordVector()
            val stemmer = new SnowballStemmer("porter")
            
            vectorFilter.setUseStoplist(true)
            vectorFilter.setStemmer(stemmer)
            vectorFilter.setTFTransform(true)
            vectorFilter.setIDFTransform(true)
            
            vectorFilter
        }
    }
    
    trait Group
    trait Subgroup
    
    trait ClassifierSubgroup extends Subgroup {
        val classifier: FilePlan
        val results: FilePlan
    }
    
    class TestTrainPairFilePlan(trainplan: FilePlan, testplan: FilePlan) {
        def testfile = testplan.file
        def testref = testplan.fileref
        
        def trainfile = trainplan.file
        def trainref = trainplan.fileref
        
        var filter: FilePlan = null
        
        def map(file: File, fun: (FilePlan, FilePlan) => Unit) = new TestTrainPairFilePlan(
            new FilePlan(resolvePath(file, "train.json"), fun(trainplan, _: FilePlan)),
            new FilePlan(resolvePath(file, "test.json"), fun(testplan, _: FilePlan))
        )
    }
    
    def reqFolder(filename: String) {
        val file = new File(filename)
        if(!file.exists()) {
            file.mkdir()
        }
    }
}




