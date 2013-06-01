package external
import java.io.File
import java.io.IOException
import filter.Filter
import parser.ArffJsonInstancesSource
import common.Path._
import common.Path
import filter.GlobalFilter
import parser.ContentDescribable
import parser.History
import classifier.CategoryIs
import filter.FilterFactory
import filter.Loadable
import common.FileManager
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import common.Path

object GensimLsiFilter {
    val modelPath = new Path("lsi_models") !
    
    def apply(numDims: Int) = new FilterFactory with Loadable[GensimLsiFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = {
            val filter = new GensimLsiFilter(numDims) {
                        override val trainingParams = Filter.trainingParams(historyAppendix, trainBase)
                    }
            filter.builtModel(trainBase)
            filter
        }
        
        val historyAppendix = "lsi-" + numDims
    }
    
    @serializable
    trait Appendix extends History {
        val numLsiDims: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ GensimLsiFilter(numLsiDims)
    }
    
    def main(args: Array[String]) {
        Path.rootFolder = "data_mini"
        
        val source = ArffJsonInstancesSource(List(
            ArffJsonInstance("1", List("a", "b"), List(1.0, 2.5, -2.4)),
            ArffJsonInstance("2", List("a", "c"), List(3.45, 0.0, 0.1)),
            ArffJsonInstance("3", List("b"), List(-1.4, -2.5, -100.0))
        ),
        ArffJsonHeader(3))
        
        val lsiFilter = GensimLsiFilter(2)(source)
        source.applyFilter(lsiFilter)
    }
}

@serializable
abstract class GensimLsiFilter(val numTopics: Int) extends GlobalFilter {
    import common.Common.verbosity
    
    var modelFilename = common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString
    def modelFile = (GensimLsiFilter.modelPath / modelFilename).file
    
    def builtModel(source: ArffJsonInstancesSource) {
        val (intancesFile, isTmpInstancesFile) = {
            source match {
                case co: ContentDescribable =>
                    if(!source.saved) source.save
                    (FileManager.fullFilename2SafeFile(source.fullFilename), false)
                case _ => 
                    val filename = "tmpBeforeLsi"
                    source.save(filename)
                    (new File(filename), true)
            }
        }
        
        val command = ExternalAlgorithmApplier.command("lsi_generate")
        
        val cmd = List(
                command,
                intancesFile.getCanonicalPath(),
                numTopics.toString,
                modelFile.getCanonicalPath()
        ).mkString(" ")
        
         try {
            val p = MyProcess(cmd)
            if(p.exitValue() == 0) {
                if(verbosity >= 2) println("Building LSI model was successful")
            } else {
                throw new RuntimeException("An unknown error occured")
            }
        } catch {
            case io: IOException => System.err.println("An IOException occured: " + io.getMessage())
            case e: Throwable => throw e
        } finally {
            if(isTmpInstancesFile) {
                val filename = "tmpBeforeLsi"
                source.save(filename)
                (new File(filename), true)
            }
        }
    }
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        val (intancesFile, isTmpInstancesFile) = {
            inst match {
                case co: ContentDescribable =>
                    if(!inst.saved) inst.save
                    (FileManager.fullFilename2SafeFile(inst.fullFilename), false)
                case _ => 
                    (new File("tmpBeforeLsi"), true)
            }
        }

        val command = ExternalAlgorithmApplier.command("lsi_apply")
        
        val resultFile = new File("tmpAfterLsi")
        
        val cmd = List(
                command,
                intancesFile.getCanonicalPath(),
                modelFile.getCanonicalPath(),
                resultFile.getCanonicalPath()
        ).mkString(" ")
        
        try {
            val p = MyProcess(cmd)
            if(p.exitValue() == 0) {
                if(verbosity >= 2) println("Applying LSI filter was successful")
            } else {
                throw new RuntimeException("An unknown error occured")
            }
        } catch {
            case io: IOException => System.err.println("An IOException occured: " + io.getMessage())
            case e: Throwable => throw e
        } finally {
            if(isTmpInstancesFile) {
                intancesFile.delete()
            }
        }
        
        ArffJsonInstancesSource(resultFile.getCanonicalPath)
    }
}
