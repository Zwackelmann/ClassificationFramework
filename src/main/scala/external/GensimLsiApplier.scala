package external
import java.io.File
import java.io.IOException
import filter.Filter
import parser.ArffJsonInstancesSource
import common.Path._
import common.Path
import filter.GlobalFilter
import filter.StorableFilterFactory
import parser.ContentDescribable
import parser.History
import classifier.TargetClassDefinition

object GensimLsiFilter {
    val modelPath = new Path("lsi_models") !
    
    def apply(numDims: Int) = new StorableFilterFactory {
        def apply(trainBase: ArffJsonInstancesSource) = {
            val filter = new GensimLsiFilter(numDims)
            filter.builtModel(trainBase)
            filter
        }
        
        val historyAppendix = "lsi-" + numDims
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[GensimLsiFilter]
    }
    
    @serializable
    trait Appendix extends History {
        val numLsiDims: Int
        abstract override def apply(targetClassDef: TargetClassDefinition) = super.apply(targetClassDef) :+ GensimLsiFilter(numLsiDims)
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
                    (source.file, false)
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
            case e => throw e
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
                    (inst.file, false)
                case _ => 
                    (new File("tmpBeforeLsi"), true)
            }
        }

        val command = ExternalAlgorithmApplier.command("lsi_apply")
        
        val resultFile = new File("tmpAfterLsi")
        
        val cmd = List(
                command,
                inst.file.getCanonicalPath(),
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
            case e => throw e
        } finally {
            if(isTmpInstancesFile) {
                intancesFile.delete()
            }
        }
        
        ArffJsonInstancesSource(resultFile)
    }
}
