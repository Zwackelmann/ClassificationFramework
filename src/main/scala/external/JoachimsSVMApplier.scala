package external

import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import format.arff_json.ArffInstances
import format.arff_json.ArffJsonInstance
import java.io.IOException
import conversion.ArffJson2Joachims
import common.Common.randomStream
import model.RawClassification
import common.Common.FileConversion._
import parser.ArffJsonInstancesSource
import scala.collection.immutable.Iterable
import classifier.CategoryIs
import classifier.Classifier
import common.Path.classifierPath
import common.Path
import classifier.Learner
import parser.ContentDescription
import parser.ContentDescribable
import common.FileManager
import FileManager.Protocol._

/**
 * An object to apply Joachim's svm_learn algorithm.
 */
object JoachimsSVMLearnApplier extends ExternalAlgorithmApplier("svm_learn") {
    /**
     * Applies the svm_learn algorithm<br>
     * @param options A map containing all options for the algorithms. The options are listed in the svm_learn help.
     *     For example pass 
     *     <pre>Map("-v", List("0"))<pre>
     *     so set verbose to 0
     * @param inFile The example file in arffJson format
     * @param outFile The target file where the model should be saved to
     * @param classFun A function used during the transformation from the arffJson format to the custom format. It must map a List of 
     *     MSC classes to a boolean value which indicates if an example is considered to be positive or negative. To set all examples of
     *     MSC class 35 to positive you could pass:
     *     <pre> (mscClasses) => mscClasses.exists(_.substring(0, 2) == "35")</pre>
     */
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, outFile: File, classFun: CategoryIs) {
        // println("Building custom format")
        
        val tmpFile = {
            def randStream: Stream[Int] = Stream.cons((math.random*10).toInt, randStream)
            val filename = randStream.take(32).mkString("")
            File.createTempFile(filename, null)
        }
        
        ArffJson2Joachims(inst, tmpFile, classFun.matchesForTraining _)
        
        // println("Executing joachims svm learn:")
        val cmd = List(
                command,
                options.map(o => o._1 + " " + o._2.mkString(" ")).toList.mkString(" "),
                tmpFile.getCanonicalPath(),
                outFile.getCanonicalPath()
        ).mkString(" ")
        tmpFile.deleteOnExit()
        
        // println(cmd)
        
        try {
            val p = MyProcess(cmd)
            if(p.exitValue() == 0) {
                // println("Execution was successful")
            } else {
                throw new RuntimeException("An unknown error occured")
            }
        } catch {
            case io: IOException => System.err.println("An IOException occured: " + io.getMessage())
            case e: Throwable => throw e
        }
    }
}



object JoachimsSVMClassifyApplier extends ExternalAlgorithmApplier("svm_classify") {
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, modelFile: File, classFun: (List[String] => Option[Boolean])) = {
        // create file in joachim's format
        val convertedInstancesFile = File.createTempFile(randomStream().take(32).map(d => (d*10).toInt).mkString(""), null)
        convertedInstancesFile.deleteOnExit()
        ArffJson2Joachims(inst, convertedInstancesFile)
        
        // create a temp file for the classification results
        val resultsFile = File.createTempFile(randomStream().take(32).map(d => (d*10).toInt).mkString(""), null)
        resultsFile.deleteOnExit()
        
        // run svm classify
        val cmd = List(
                command,
                options.map(o => o._1 + " " + o._2.mkString(" ")).toList.mkString(" "),
                convertedInstancesFile.getCanonicalPath(),
                modelFile.getCanonicalPath(),
                resultsFile.getCanonicalPath()
        ).mkString(" ")
        
        // println("Execute command:\n" + cmd)
        
        try {
            val p = MyProcess(cmd)
            if(p.exitValue() == 0) {
                // println("Execution was successful")
            } else {
                throw new RuntimeException("An error occured")
            }
        } catch {
            case io: IOException => 
                System.err.println("An IOException occured: " + io.getMessage())
                System.exit(1)
            case e: Throwable => throw e
        }
        
        val resultsFileIterator = new BufferedReader(new FileReader(resultsFile)).lines
        val arffJsonInstancesIterator = inst.iterator
        
        resultsFileIterator.zip(arffJsonInstancesIterator).map(pair => {
            val result = pair._1
            val inst = pair._2
            
            val isTarget = classFun(inst.categories)
            new RawClassification(inst.id, result.toDouble, (if(isTarget.isDefined && isTarget.get) 1 else if(isTarget.isDefined) -1 else 0))
        })
    }
    
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, modelFile: File, outFile: File, classFun: (List[String] => Option[Boolean])) {
        val rawClassifications = apply(options, inst, modelFile, classFun)
        
        val out = new BufferedWriter(new FileWriter(outFile))
        
        for(cl <- rawClassifications) {
            out.write(cl.toString() + "\n")
        }
        
        out.close
    }
}

object JoachimsSVMClassifier {
    val modelPath = new Path("svm_models") !
    val idAppendix = "svm"
    def load(fullFilename: String, learner: Option[Learner] = None) = {
        val savedObject = (FileManager !? ReceiveFile(fullFilename)) match {
            case AcceptReceiveFile(file) => {
                common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
            }
            case FileNotExists => throw new RuntimeException(fullFilename + " does not exist")
        }
        
        new JoachimsSVMClassifier(
            try { savedObject(0).asInstanceOf[Map[String, List[String]]] } catch { case _: Throwable => Map[String, List[String]]("-v" -> List("0")) },
            savedObject(0).asInstanceOf[String],
            savedObject(2).asInstanceOf[Option[ContentDescription]],
            savedObject(3).asInstanceOf[CategoryIs],
            learner
        )
    }
}

class JoachimsSVMClassifier(_options: Map[String, List[String]], _modelFilename: String, trainBaseContentDescription: Option[ContentDescription], categoryIs: CategoryIs, parent: Option[Learner]) extends Classifier(trainBaseContentDescription, categoryIs, parent) with Serializable {
    
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, parent: Option[Learner]) {
        this(
        options,
        common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString,
        (trainBase match {
            case co: ContentDescribable => Some(co.contentDescription)
            case _ => None
        }),
        categoryIs,
        parent
        )
        
        buildClassifier(trainBase)
    }
    
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs, parent: Learner) = 
        this(options, trainBase, categoryIs, Some(parent))
        
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, categoryIs: CategoryIs) = 
        this(options, trainBase, categoryIs, None)
    
    val options = _options
    val modelFilename = _modelFilename
    def modelFile = (JoachimsSVMClassifier.modelPath / modelFilename).file
    
	private def buildClassifier(inst: ArffJsonInstancesSource) {
		JoachimsSVMLearnApplier(
			options,
			inst,
			modelFile,
			categoryIs
		)
	}
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = {
        new Iterable[RawClassification] {
            def iterator = JoachimsSVMClassifyApplier(
                options, 
                mappedInst, 
                modelFile, 
                categoryIs.matchesForTesting _
            ) 
        }
    }
    
    def save(fullFilename: String) {
        val objToSave = Array[Any](
            this.modelFilename,
            this.options,
            this.trainBaseContentDescription,
            this.targetClassDef,
            None
        )
        
        (FileManager !? CreateFile(fullFilename)) match {
            case AcceptCreateFile(fileHandle) => 
                common.ObjectToFile.writeObjectToFile(objToSave, fileHandle.file)
                fileHandle.close
            case RejectCreateFile => throw new RuntimeException("Could not save SVM")
        }
    }
    
    override def toString = "SvmClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}
















