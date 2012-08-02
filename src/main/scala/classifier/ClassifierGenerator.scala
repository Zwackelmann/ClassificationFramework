package classifier
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesMapping
import parser.ArffJsonInstancesFile
import filter.VectorFromDictFilter
import common.Path
import common.Path._
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import external.JoachimsSVMLearnApplier
import external.JoachimsSVMClassifier
import model.RawClassification
import parser.InstancesMappings
import weka.classifiers.bayes.NaiveBayes
import common.ForgetableMap
import scala.collection.mutable.HashMap
import weka.classifiers.trees.J48

object TitleOnlyLsiSVMGenerator extends ClassifierGenerator {
    val numLsiDims = 100
    
    def fileAppendix = "title-only-lsi_-" + numLsiDims + "-svm"
    @transient lazy val mapping = InstancesMappings.instancesMappings1(0, numLsiDims)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-0", "vector-conf4", "tf-idf", "normalized", ("lsi-" + numLsiDims))),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + "_" + targetClassDef.filenameExtension + "_" + JoachimsSVMClassifier.idAppendix)
        if(!classifierFile.exists()) {
            val svm = new JoachimsSVMClassifier(
                Map("-v" -> List("0")),
                inst,
                targetClassDef, 
                this
            )
            
            svm.save(classifierFile)
            svm
        } else {
            JoachimsSVMClassifier.load(classifierFile)
        }
    }
}

object TitleOnlyOddsRatioSVMGenerator extends ClassifierGenerator {
    val numOddsRatioDims = 2000
    
    def fileAppendix = "title-only-odds-ratio-" + numOddsRatioDims + "-svm"
    @transient lazy val mapping = InstancesMappings.orMappings(0, numOddsRatioDims)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-0", "vector-conf4", "or-" + numOddsRatioDims)),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + targetClassDef.filenameExtension + "_" + JoachimsSVMClassifier.idAppendix)
        if(!classifierFile.exists()) {
            val svm = new JoachimsSVMClassifier(
                Map("-v" -> List("0")),
                inst,
                targetClassDef, 
                this
            )
            
            svm.save(classifierFile)
            svm
        } else {
            JoachimsSVMClassifier.load(classifierFile)
        }
    }
}

object AbstractOnlyLsiSVMGenerator extends ClassifierGenerator {
    val numLsiDims = 500
    
    def fileAppendix = "abstract-only-lsi_-" + numLsiDims + "-svm"
    @transient lazy val mapping = InstancesMappings.instancesMappings1(1, numLsiDims)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-1", "vector-conf4", "tf-idf", "normalized", ("lsi-" + numLsiDims))),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + targetClassDef.filenameExtension + "_" + JoachimsSVMClassifier.idAppendix)
        if(!classifierFile.exists()) {
            val svm = new JoachimsSVMClassifier(
                Map("-v" -> List("0")),
                inst,
                targetClassDef, 
                this
            )
            
            svm.save(classifierFile)
            svm
        } else {
            JoachimsSVMClassifier.load(classifierFile)
        }
    }
}

object AbstractOnlyOddsRatioSVMGenerator extends ClassifierGenerator {
    val numOddsRatioDims = 2000
    
    def fileAppendix = "abstract-only-odds-ratio_-" + numOddsRatioDims + "-svm"
    @transient lazy val mapping = InstancesMappings.orMappings(1, numOddsRatioDims)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-1", "vector-conf4", "or-" + numOddsRatioDims)),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + targetClassDef.filenameExtension + "_" + JoachimsSVMClassifier.idAppendix)
        if(!classifierFile.exists()) {
            val svm = new JoachimsSVMClassifier(
                Map("-v" -> List("0")),
                inst,
                targetClassDef, 
                this
            )
            
            svm.save(classifierFile)
            svm
        } else {
            JoachimsSVMClassifier.load(classifierFile)
        }
    }
}

object AbstractOnlyOddsRatioC45Generator extends ClassifierGenerator {
    val numOddsRatioDims = 2000
    
    def fileAppendix = "abstract-only-odds-ratio_-" + numOddsRatioDims + "-c45"
    @transient lazy val mapping = InstancesMappings.orMappings(1, numOddsRatioDims)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-1", "vector-conf4", "or-" + numOddsRatioDims)),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + targetClassDef.filenameExtension + "_" + JoachimsSVMClassifier.idAppendix)
        if(!classifierFile.exists()) {
            val bayes = new WekaClassifier(inst, targetClassDef, this) {
                def classifierConfig() = new J48()
            }
            bayes.save(classifierFile)
            bayes
        } else {
            WekaClassifier.load(classifierFile)
        }
    }
}

object JournalOnlyBayesGenerator extends ClassifierGenerator {
    def fileAppendix = "journal-only-nominalized-flattened-bayes"
    
    @transient lazy val mapping = InstancesMappings.nominalMapping(2)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-2", "nominalized", "flattened")),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + "_" + targetClassDef.filenameExtension)
        
        val mappedInstances = mapInstances(inst, targetClassDef)
        
        if(!classifierFile.exists()) {
            val bayes = new WekaClassifier(inst, targetClassDef, this) {
                def classifierConfig() = new NaiveBayes()
                
                def aggregateClassifications(fun: (List[Double]) => Double)(groupedClassifications: Map[String, List[RawClassification]]) = {
                    groupedClassifications.values.toList.map(cl => new RawClassification(
                        cl(0).id, 
                        fun(cl.map(_.classification)),
                        cl(0).realValue
                    ))
                }
                
                val aggregatedByMax = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => math.max(a, b))) _
                val aggregatedByAvg = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => a + b) / l.size) _
                
                override def calculateClassifications(inst: ArffJsonInstancesSource) = {
                    val calculated = aggregatedByMax(
                        super.calculateClassifications(inst)
                            .toList
                            .groupBy(c => c.id)
                    )
                    
                    val allIdsWithClass = inst.map(i => (i.id, targetClassDef(i.mscClasses)))
                    val calculatedIds = calculated.map(_.id)
                    val missingIdsWithClass = allIdsWithClass.toList.filter(idClass => !calculatedIds.contains(idClass._1))
                    
                    val missingClassifications = missingIdsWithClass.map(idClass => new RawClassification(idClass._1, 0, if(idClass._2) 1.0 else -1.0))
                    calculated ++ missingClassifications
                }
            }
            bayes.save(classifierFile)
            bayes
        } else {
            WekaClassifier.load(classifierFile)
        }
    }
}

object TermsOnlyBayesGenerator extends ClassifierGenerator {
    def fileAppendix = "terms-only-nominalized-flattened-bayes"
    
    @transient lazy val mapping = InstancesMappings.nominalMapping(3)
    
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = mapping(
        inst,
        inst.contentDescription.withHistory(List("project-3", "nominalized", "flattened")),
        targetClassDef
    )
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = {
        val classifierFile = classifierPath / (inst.contentDescription.base + "_" + fileAppendix + "_" + targetClassDef.filenameExtension)
        val mappedInstances = mapInstances(inst, targetClassDef)
        
        if(!classifierFile.exists()) {
            val bayes = new WekaClassifier(inst, targetClassDef, this) {
                def classifierConfig() = new NaiveBayes()
                
                def aggregateClassifications(fun: (List[Double]) => Double)(groupedClassifications: Map[String, List[RawClassification]]) = {
                    groupedClassifications.values.toList.map(cl => new RawClassification(
                        cl(0).id, 
                        fun(cl.map(_.classification)),
                        cl(0).realValue
                    ))
                }
                
                val aggregatedByMax = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => math.max(a, b))) _
                val aggregatedByAvg = aggregateClassifications((l: List[Double]) => l.reduceLeft((a, b) => a + b) / l.size) _
                
                override def calculateClassifications(inst: ArffJsonInstancesSource) = {
                    val calculated = aggregatedByMax(
                        super.calculateClassifications(inst)
                            .toList
                            .groupBy(c => c.id)
                    )
                    
                    val allIdsWithClass = inst.map(i => (i.id, targetClassDef(i.mscClasses)))
                    val calculatedIds = calculated.map(_.id)
                    val missingIdsWithClass = allIdsWithClass.toList.filter(idClass => !calculatedIds.contains(idClass._1))
                    
                    val missingClassifications = missingIdsWithClass.map(idClass => new RawClassification(idClass._1, 0, if(idClass._2) 1.0 else -1.0))
                    calculated ++ missingClassifications
                }
            }
            bayes.save(classifierFile)
            bayes
        } else {
            WekaClassifier.load(classifierFile)
        }
    }
}

object ClassifierGenerator {
    def resultsFilePath(classifierGen: ClassifierGenerator, targetClassDef: TargetClassDefinition, inst: ArffJsonInstancesSource) = 
        Path.resultsPath / (classifierGen.fileAppendix + "_" + targetClassDef.filenameExtension + "_" + inst.contentDescription.filenameAppendix + ".json")
}

@serializable
trait ClassifierGenerator {
    import RawClassification._
    
    def generateClassifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): Classifier
    
    def fileAppendix: String
    def mapInstances(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition): ArffJsonInstancesSource
    
    val classifierCache = new HashMap[Pair[ArffJsonInstancesSource, TargetClassDefinition], Classifier]
    def classifier(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = classifierCache.getOrElseUpdate((inst, targetClassDef), {
        generateClassifier(inst, targetClassDef)
    })
    
    val classificationsCache = new HashMap[Pair[ArffJsonInstancesSource, TargetClassDefinition], List[RawClassification]]
    def classifications(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) = classificationsCache.getOrElseUpdate((inst, targetClassDef), {
        val resultsFilePath = ClassifierGenerator.resultsFilePath(this, targetClassDef, inst)
        if(resultsFilePath.exists) RawClassification.fromFile(resultsFilePath)
        else classifier(inst, targetClassDef).classifications(inst)
    })
    
    val filterAndGroupCache = new HashMap[Tuple4[ArffJsonInstancesSource, TargetClassDefinition, Double, Double], Map[RawClassification.Category, Iterable[RawClassification]]]
    def filterAndGroup(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = filterAndGroupCache.getOrElseUpdate((inst, targetClassDef, positiveThreshold, certaintyThreshold), {
        Map(
            TRUE_POSITIVE -> List(),
            FALSE_POSITIVE -> List(),
            TRUE_NEGATIVE -> List(),
            FALSE_NEGATIVE -> List()
        ) ++ classifications(inst, targetClassDef).filter(c => math.abs(c.classification - positiveThreshold) > certaintyThreshold).groupBy(_.cat(positiveThreshold))
    })
    
    def precision(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_POSITIVE).size)
    }
    
    def recall(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) = {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        filteredAndGrouped(TRUE_POSITIVE).size.toDouble / (filteredAndGrouped(TRUE_POSITIVE).size + filteredAndGrouped(FALSE_NEGATIVE).size)
    }
        
    def report(inst: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition, positiveThreshold: Double = 0.0, certaintyThreshold: Double = 0.0) {
        val filteredAndGrouped = filterAndGroup(inst, targetClassDef, positiveThreshold, certaintyThreshold)
        
        println("precision: " + precision(inst, targetClassDef, positiveThreshold, certaintyThreshold))
        println("recall: " + recall(inst, targetClassDef, positiveThreshold, certaintyThreshold))
        
        val certainties = classifications(inst, targetClassDef).map(c => math.abs(c.classification))
        println("mean certainty: " + (if(certainties.size != 0) mean(certainties) else "NaN")) 
        println("certainty std dev: " + (if(certainties.size != 0) stdDev(certainties) else "NaN"))
        println("total: " + classifications(inst, targetClassDef).size)
    }
    
    def mean(values: Iterable[Double]) = {
        values.reduceLeft(_ + _) / values.size
    }
    
    def variance(values: Iterable[Double]) = {
        val m = mean(values)
        values.map(v => (m - v) * (m - v)).reduceLeft(_ + _) / values.size
    }
    
    def stdDev(values: Iterable[Double]) = math.sqrt(variance(values))
}















