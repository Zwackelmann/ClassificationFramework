package external

import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import format.arff_json.ArffJsonInstances
import format.arff_json.ArffJsonInstance
import java.io.IOException
import conversion.ArffJson2Joachims
import common.Common.randomStream
import format.arff_json.DenseArffJsonInstance
import net.sf.json.JSONSerializer
import net.sf.json.JSONArray
import model.RawClassification
import common.Common.FileConversion._
import parser.ArffJsonInstancesSource
import scala.collection.immutable.Iterable
import classifier.TargetClassDefinition
import classifier.Classifier
import common.Path.classifierPath
import common.Path
import classifier.Learner
import parser.ContentDescription
import parser.ContentDescribable

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
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, outFile: File, classFun: List[String] => Boolean) {
        // println("Building custom format")
        
        val tmpFile = {
            def randStream: Stream[Int] = Stream.cons((math.random*10).toInt, randStream)
            val filename = randStream.take(32).mkString("")
            File.createTempFile(filename, null)
        }
        
        ArffJson2Joachims(inst, tmpFile, classFun)
        
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
            case e => throw e
        }
    }
}



object JoachimsSVMClassifyApplier extends ExternalAlgorithmApplier("svm_classify") {
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, modelFile: File, classFun: (List[String] => Boolean)) = {
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
            case e => throw e
        }
        
        val resultsFileIterator = new BufferedReader(new FileReader(resultsFile)).lines
        val arffJsonInstancesIterator = inst.iterator
        
        resultsFileIterator.zip(arffJsonInstancesIterator).map(pair => {
            val result = pair._1
            val inst = pair._2
            
            new RawClassification(inst.id, result.toDouble, if(classFun(inst.categories)) 1 else -1)
        })
    }
    
    def apply(options: Map[String, List[String]], inst: ArffJsonInstancesSource, modelFile: File, outFile: File, classFun: (List[String] => Boolean)) {
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
    def load(file: File, learner: Option[Learner] = None) = {
        val savedObject = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Array[Any]]
        
        new JoachimsSVMClassifier(
            try { savedObject(0).asInstanceOf[Map[String, List[String]]] } catch { case _ => Map[String, List[String]]("-v" -> List("0")) },
            savedObject(0).asInstanceOf[String],
            savedObject(2).asInstanceOf[Option[ContentDescription]],
            savedObject(3).asInstanceOf[TargetClassDefinition],
            learner
        )
    }
}

@serializable
class JoachimsSVMClassifier(_options: Map[String, List[String]], _modelFilename: String, trainBaseContentDescription: Option[ContentDescription], targetClassDef: TargetClassDefinition, parent: Option[Learner]) extends Classifier(trainBaseContentDescription, targetClassDef, parent) {
    
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Option[Learner]) {
        this(
        options,
        common.Common.randomStream().map(d => (d*9).toInt).take(32).mkString,
        (trainBase match {
            case co: ContentDescribable => Some(co.contentDescription)
            case _ => None
        }),
        targetClassDef,
        parent
        )
        
        buildClassifier(trainBase)
    }
    
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, parent: Learner) = 
        this(options, trainBase, targetClassDef, Some(parent))
        
    def this(options: Map[String, List[String]], trainBase: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = 
        this(options, trainBase, targetClassDef, None)
    
    val options = _options
    val modelFilename = _modelFilename
    def modelFile = (JoachimsSVMClassifier.modelPath / modelFilename).file
    
	private def buildClassifier(inst: ArffJsonInstancesSource) {
		JoachimsSVMLearnApplier(
			options,
			inst,
			modelFile,
			targetClassDef
		)
	}
    
    def calculateClassifications(mappedInst: ArffJsonInstancesSource) = {
        new Iterable[RawClassification] {
            def iterator = JoachimsSVMClassifyApplier(
                options, 
                mappedInst, 
                modelFile, 
                targetClassDef
            ) 
        }
    }
    
    def save(outFile: File) {
        val objToSave = Array[Any](
            this.modelFilename,
            this.options,
            this.trainBaseContentDescription,
            this.targetClassDef,
            None
        )
        
        common.ObjectToFile.writeObjectToFile(objToSave, outFile)
    }
    
    override def toString = "SvmClassifier(trainBase: " + trainBaseContentDescription + ", targetClassDef: " + targetClassDef + ")"
}
















