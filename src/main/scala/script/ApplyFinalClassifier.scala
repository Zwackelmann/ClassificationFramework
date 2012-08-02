package script
import classifier.ClassifierGenerator
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import common.Path.classifierPath
import classifier.Classifier
import external.JoachimsSVMClassifier
import classifier.WekaClassifier
import model.RawClassification
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.trees.J48
import classifier.FinalClassifierGenerator
import parser.ArffJsonInstancesFile
import classifier.TopClassIs
import filter.NominalizeFilter
import filter.FlattenFilter
import filter.ProjectionFilter
import filter.VectorFromDictFilter
import java.io.File
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import external.GensimLsiFilter
import filter.OddsRatioFilter

object ApplyFinalClassifier {
    def main(args: Array[String]) {
        val finalGen = new FinalClassifierGenerator(
            List(
                AbstractOnlyOddsRatioC45Generator/*,
                TitleOnlyOddsRatioSVMGenerator,
                AbstractOnlyLsiSVMGenerator,
                AbstractOnlyOddsRatioSVMGenerator,
                JournalOnlyBayesGenerator,
                TermsOnlyBayesGenerator*/
            )
        )
        
        val testset = new ArffJsonInstancesFile("final", "test", List())
        
        for(topClass <- common.Common.topClasses) {
            println("\n\nStart evaluating results for topClass: " + topClass)
            finalGen.classifications(testset, TopClassIs(topClass))
        }
    }
}

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

object InstancesMappings {
    def nominalMapping(projectId: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None)
                case "nominalized" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new NominalizeFilter.Conf1("nominalized")
                    filter.expandDict(trainBase)
                    filter
                }, Some((filterFile: File) => NominalizeFilter.load(filterFile)))
                case "flattened" => (
                    (_: ArffJsonInstancesSource) => new FlattenFilter,
                    None
                )
            }
        }
    }
    
    def instancesMappings1(projectId: Int, numLsiDims: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None) 
                    
                case "vector-conf4" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new VectorFromDictFilter.Conf4
                    filter.buildDict(trainBase)
                    filter
                }, Some((filterFile: File) => {
                    VectorFromDictFilter.load(filterFile)
                }))
                case "tf-idf" => (
                    (trainBase: ArffJsonInstancesSource) => new TfIdfFilter(trainBase, "tf-idf"),
                    Some((filterFile: File) => TfIdfFilter.load(filterFile))
                )
                case "normalized" => (
                    (_: ArffJsonInstancesSource) => new NormalizeVectorFilter,
                    None
                )
                case lsi if lsi == ("lsi-" + numLsiDims) => (
                    (trainBase: ArffJsonInstancesSource) => {
                        val filter = new GensimLsiFilter(numLsiDims)
                        filter.builtModel(trainBase)
                        filter
                    },
                    Some((filterFile: File) => GensimLsiFilter.load(filterFile))
                )
                case _ => throw new RuntimeException(filterName + " is no valid filter name")
            }
        }
    }
    
    def orMappings(projectId: Int, numOrDims: Int) = new InstancesMappings {
        def filter(filterName: String) = {
            filterName match {
                case project if project == "project-" + projectId => ((_: ArffJsonInstancesSource) => new ProjectionFilter(List(projectId), "project-" + projectId), None) 
                    
                case "vector-conf4" => ((trainBase: ArffJsonInstancesSource) => {
                    val filter = new VectorFromDictFilter.Conf4
                    filter.buildDict(trainBase)
                    filter
                }, Some((filterFile: File) => {
                    VectorFromDictFilter.load(filterFile)
                }))
                case or if or == ("or-" + numOrDims) => (
                    (trainBase: ArffJsonInstancesSource) => {
                        new OddsRatioFilter(trainBase, numOrDims, "or-" + numOrDims)
                    },
                    Some((filterFile: File) => OddsRatioFilter.load(filterFile))
                )
                case _ => throw new RuntimeException(filterName + " is no valid filter name")
            }
        }
    }
}