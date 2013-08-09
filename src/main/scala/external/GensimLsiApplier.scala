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
    
    def main(args: Array[String]) = try {
        val source = ArffJsonInstancesSource(List(
                 ArffJsonInstance("1", List(), Map( 0 -> 9.2670, 1 -> 11.1720,  2 -> 8.8520,  6 -> 0.5850,  8 -> 8.5871), 14),
                 ArffJsonInstance("2", List(), Map( 2 -> 5.2651, 6 ->  0.5850,  9 -> 4.0971                            ), 14), 
                 ArffJsonInstance("3", List(), Map( 4 -> 8.5466, 6 ->  0.5850,  9 -> 3.1699                            ), 14),
                 ArffJsonInstance("4", List(), Map( 3 -> 9.2670, 7 ->  9.2670, 12 -> 9.2670, 13 -> 8.5871              ), 14),
                 ArffJsonInstance("5", List(), Map(10 -> 4.7549                                                        ), 14),
                 ArffJsonInstance("6", List(), Map( 4 -> 6.8501, 5 -> 10.3399,  6 -> 0.5850, 10 -> 7.6195, 11 -> 9.8419), 14)
            ),
            ArffJsonHeader(14)
        )
        
        val lsiFilter = GensimLsiFilter(3)(source)
        
        val newInst = lsiFilter.applyFilter(
            ArffJsonInstancesSource(List(
                ArffJsonInstance("7", List(), Map(4 -> 1.5850, 11 -> 2.5850), 14)
            ), ArffJsonHeader(14))
        )
        
        println(newInst.mkString("\n"))
    } catch {
        case _: Throwable => FileManager.quit
    }
}

@serializable
abstract class GensimLsiFilter(val numTopics: Int) extends GlobalFilter {
    import common.Common.verbosity
    
    lazy val modelFilename = common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString
    var modelFile: File = _
    
    def builtModel(source: ArffJsonInstancesSource) {
        val (intancesFile, isTmpInstancesFile) = {
            source match {
                case co: ContentDescribable =>
                    if(!source.saved) source.save
                    modelFile = (GensimLsiFilter.modelPath / modelFilename).file
                    (FileManager.fullFilename2SafeFile(source.fullFilename), false)
                case _ => 
                    val filename = "tmpTrainLsi"
                    source.save(filename)
                    modelFile = new File(modelFilename)
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
                    val filename = "tmpApplyLsi"
                    inst.save(filename)
                    modelFile = new File(modelFilename)
                    (new File(filename), true)
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
