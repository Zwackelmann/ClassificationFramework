package external
import java.io.File
import java.io.IOException
import filter.Filter
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesFile
import common.Path._
import common.Path
import filter.GlobalFilter

/*object GensimLsiGenModelApplier extends ExternalAlgorithmApplier("lsi_generate") {
    
    def apply(inFile: File, numTopics: Int, modelFile: File) {
        
        println("Executing gensim generate lsi model:")
        val cmd = List(
                command,
                inFile.getCanonicalPath(),
                numTopics.toString,
                modelFile.getCanonicalPath()
        ).mkString(" ")
        
        println(cmd)
        
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
}*/

object GensimLsiFilter {
    val modelPath = new Path("lsi_models") !
    def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[GensimLsiFilter]
}

@serializable
class GensimLsiFilter(val numTopics: Int) extends GlobalFilter {
    var modelFilename = common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString
    
    def modelFile = (GensimLsiFilter.modelPath / modelFilename).file
    val historyAppendix = "lsi-" + numTopics
    
    def builtModel(source: ArffJsonInstancesSource) {
        println("train lsi filter with " + source.contentDescription)
        
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
        println("use lsi filter with " + inst.contentDescription)
        
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

/*object GensimLsiApplyModelApplier extends ExternalAlgorithmApplier("lsi_apply") {
    def apply(trainFile: File, modelFile: File, outFile: File) {
        
        println("Executing gensim generate lsi model:")
        val cmd = List(
                command,
                trainFile.getCanonicalPath(),
                modelFile.getCanonicalPath(),
                outFile.getCanonicalPath()
        ).mkString(" ")
        
        println(cmd)
        
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
}*/