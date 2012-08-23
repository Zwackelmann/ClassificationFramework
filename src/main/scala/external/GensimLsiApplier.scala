package external
import java.io.File
import java.io.IOException
import filter.Filter
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesFile
import common.Path._
import common.Path
import filter.GlobalFilter
import filter.StorableFilterFactory
import format.arff_json.HistoryItem


object GensimLsiFilter {
    val modelPath = new Path("lsi_models") !
    
    def apply(numDims: Int) = new StorableFilterFactory {
        def apply(trainBase: ArffJsonInstancesSource) = {
            val filter = new GensimLsiFilter(numDims, this)
            filter.builtModel(trainBase)
            filter
        }
        
        val historyAppendix = "lsi-" + numDims
        
        def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[GensimLsiFilter]
    }
}

@serializable
class GensimLsiFilter(val numTopics: Int, val historyAppendix: HistoryItem) extends GlobalFilter {
    var modelFilename = common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString
    
    def modelFile = (GensimLsiFilter.modelPath / modelFilename).file
    
    def builtModel(source: ArffJsonInstancesSource) {
        if(!source.saved) {
            source.save
        }
        
        val command = ExternalAlgorithmApplier.command("lsi_generate")
        
        val cmd = List(
                command,
                source.file.getCanonicalPath(),
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
        }
    }
    
    def applyFilter(inst: ArffJsonInstancesSource) = {
        if(!inst.saved) {
            inst.save
        }

        val command = ExternalAlgorithmApplier.command("lsi_apply")
        val newInstancesContentDescription = inst.contentDescription.addHistoryItem(historyAppendix)
        
        val cmd = List(
                command,
                inst.file.getCanonicalPath(),
                modelFile.getCanonicalPath(),
                newInstancesContentDescription.file.getCanonicalPath()
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
        }
        
        new ArffJsonInstancesFile(newInstancesContentDescription)
    }
}
