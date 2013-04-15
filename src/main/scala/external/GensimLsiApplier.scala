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

object GensimLsiFilter {
    val modelPath = new Path("lsi_models") !
    
    def apply(numDims: Int) = new FilterFactory with Loadable[GensimLsiFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = {
            val filter = new GensimLsiFilter(numDims)
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
}

@serializable
class GensimLsiFilter(val numTopics: Int) extends GlobalFilter {
    var modelFilename = common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString
    def modelFile = (GensimLsiFilter.modelPath / modelFilename).file
    
    def builtModel(source: ArffJsonInstancesSource) {
        val (intancesFile, isTmpInstancesFile) = {
            source match {
                case co: ContentDescribable =>
                    if(!source.saved) source.save
                    (FileManager.fullFilename2SafeFile(source.fullFilename), false)
                case _ => 
                    (new File("tmpBeforeLsi"), true)
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
                println("Execution was successful")
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
                println("Execution was successful")
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
