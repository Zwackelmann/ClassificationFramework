package testrun.scripts
import model.file.FilePlan
import java.io.File
import conversion.Json2ArffJson
import model.Paper
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.StringArffJsonAttribute
import model.DetailledSource
import filter.generator.WekaFilterGenerator
import weka.filters.Filter
import weka.filters.unsupervised.attribute.StringToWordVector
import weka.core.stemmers.SnowballStemmer
import filter.applier.FilterApplier
import filter.VectorFromDictFilter
import testrun.Testrun._
import external.JoachimsSVMLearnApplier
import external.JoachimsSVMClassifyApplier
import classifier.generator.WekaClassifierGenerator
import weka.classifiers.bayes.NaiveBayes
import filter.NominalValueFromDictFilter
import java.util.{Set => JavaSet}
import java.util.HashSet
import scala.collection.JavaConversions._
import parser.ArffJsonInstancesFile
import filter.TfIdfFilter
import filter.NormalizeVectorFilter
import common.Common.FileConversion._
import filter.feature_scoreing.OddsRatio
import common.Path
import classifier.TopClassIs
import classifier.ClassifierApplier

object Testrun3 {
    /*def main(args: Array[String]) {
        import common.Path.pathToFile
        
        val topClasses = new File("data/util/top_classes.txt").lines.toList
        
        val rootPath = new Path("common") !
        
        val base = new TestTrainPairFilePlan(
            new FilePlan(rootPath / "raw/train.json", self => ()),
            new FilePlan(rootPath / "raw/train.json", self => ())
        )
        
        val abstractOnlyPath = rootPath / "abstract_only" !
        val abstractOnly = new Group() {
            
            val rawPath = abstractOnlyPath / "raw"
            rawPath.require
            val raw = base.map(rawPath, (raw, abstractOnly) => {
                new PaperJson2AbstractOnlyArffJson(
                    inFile = raw.file, 
                    outFile = abstractOnly.fileref
                ).run
            })
            
            
            val rawVector = new Subgroup() {
                val rawVectorPath = abstractOnlyPath / "raw_vector" !
                
                val filter = new FilePlan(rawVectorPath / "filter", self => {
                    val f = new VectorFromDictFilter.Conf3
                    f.buildDict(new ArffJsonInstancesFile(raw.trainfile))
                    f.save(self.fileref)
                })
                
                val instances = raw.map(rawVectorPath, (raw, vector) => {
                    val f = VectorFromDictFilter.load(filter.file)
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(f)
                        .save(vector.fileref)
                })
            }
            
            val tfIdfVector = new Subgroup() {
                val tfIdfPath = abstractOnlyPath / "tf_idf" !
                
                val filter = new FilePlan(tfIdfPath / "filter", self => {
                    val rawFilter = VectorFromDictFilter.load(rawVector.filter.file)
                    val tfIdfFilter = new TfIdfFilter(
                        new ArffJsonInstancesFile(raw.trainfile).applyFilter(rawFilter)
                    )
                    tfIdfFilter.save(self.fileref)
                })
                
                val instances = raw.map(tfIdfPath, (raw, vector) => {
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(VectorFromDictFilter.load(rawVector.filter.file))
                        .applyFilter(TfIdfFilter.load(filter.file))
                        .save(vector.fileref)
                })
            }
            
            val normalizedTfIdfVector = new Subgroup() {
                val tfIdfNormalizedPath = abstractOnlyPath / "tf_idf_normalized" !
                
                val instances = raw.map(tfIdfNormalizedPath, (raw, normalized) => {
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(VectorFromDictFilter.load(rawVector.filter.file))
                        .applyFilter(TfIdfFilter.load(tfIdfVector.filter.file))
                        .applyFilter(new NormalizeVectorFilter())
                        .save(normalized.fileref)
                })
                
                /*val adaBoost = topClasses.map(topClass => {
                    val targetClassfun = (mscClasses: List[String]) => mscClasses.exists(_.substring(0, 2) == topClass)
                    
                    new ClassifierSubgroup() {
                        val classifier = new FilePlan(new File("data/common/abstract_only/tf_idf_lsi/adaboost_" + topClass), self => {
                            
                        })
                    }
                })*/
            }
            
            val lsiVector = new Subgroup() {
                val tfIdfLsiPath = abstractOnlyPath / "tf_idf_lsi" !
                
                val filter = new FilePlan(abstractOnlyPath / "filter", self => {
                    GensimLsiGenModelApplier(
                        normalizedTfIdfVector.instances.trainfile,
                        200,
                        self.fileref
                    )
                })
                
                val instances = tfIdfVector.instances.map(abstractOnlyPath, (tfIdf, lsi) => {
                    GensimLsiApplyModelApplier(
                        tfIdf.file,
                        filter.file,
                        lsi.fileref
                    )
                })
                
                def svmClassifier(topClass: String) =  {
                    val targetClassfun = (mscClasses: List[String]) => mscClasses.exists(_.substring(0, 2) == topClass)
                    
                    new ClassifierSubgroup() {
                        val classifier = new FilePlan(tfIdfLsiPath / ("svm_" + topClass), self => {
                            JoachimsSVMLearnApplier(
                                Map("-v" -> List("0")),
                                new ArffJsonInstancesFile(instances.trainfile),
                                self.fileref,
                                targetClassfun
                            )
                        })
                        
                        val results = new FilePlan(tfIdfLsiPath / ("svm_results_" + topClass + ".json"), self => {
                            JoachimsSVMClassifyApplier(
                                Map("-v" -> List("0")),
                                new ArffJsonInstancesFile(instances.testfile),
                                classifier.file,
                                self.fileref,
                                targetClassfun
                            )
                        })
                    }
                }
            }
        }
        
        val titleOnlyPath = rootPath / "title_only" !
        val titleOnly = new Group() {
            val rawPath = titleOnlyPath / "raw" !
            val raw = base.map(rawPath, (raw, titleOnly) => {
                new PaperJson2TitleOnlyArffJson(
                    inFile = raw.file, 
                    outFile = titleOnly.fileref
                ).run
            })
            
            val rawVector = new Subgroup() {
                val rawVectorPath = titleOnlyPath / "raw_ector" !
                
                val filter = new FilePlan(rawVectorPath / "filter", self => {
                    val f = new VectorFromDictFilter.Conf4
                    f.buildDict(new ArffJsonInstancesFile(raw.trainfile))
                    f.save(self.fileref)
                })
                
                val instances = raw.map(rawVectorPath, (raw, vector) => {
                    val f = VectorFromDictFilter.load(filter.file)
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(f)
                        .save(vector.fileref)
                })
            }
            
            val tfIdfVector = new Subgroup() {
                val tfIdfPath = titleOnlyPath / "tf_idf" !
                
                val filter = new FilePlan(tfIdfPath / "filter", self => {
                    val rawFilter = VectorFromDictFilter.load(rawVector.filter.file)
                    val tfIdfFilter = new TfIdfFilter(
                        new ArffJsonInstancesFile(raw.trainfile).applyFilter(rawFilter)
                    )
                    tfIdfFilter.save(self.fileref)
                })
                
                val instances = raw.map(tfIdfPath, (raw, vector) => {
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(VectorFromDictFilter.load(rawVector.filter.file))
                        .applyFilter(TfIdfFilter.load(filter.file))
                        .save(vector.fileref)
                })
            }
            
            val normalizedTfIdfVector = new Subgroup() {
                val tfIdfNormalizedPath = titleOnlyPath / "tf_idf_normalized" !
                
                val instances = raw.map(tfIdfNormalizedPath, (raw, normalized) => {
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(VectorFromDictFilter.load(rawVector.filter.file))
                        .applyFilter(TfIdfFilter.load(tfIdfVector.filter.file))
                        .applyFilter(new NormalizeVectorFilter())
                        .save(normalized.fileref)
                })
            }
            
            def lsiVector(numDim: Int) = new Subgroup() {
                val lsiVectorPath = titleOnlyPath / ("tf_idf_lsi_" + numDim) !
                
                val filter = new FilePlan(lsiVectorPath / "filter", self => {
                    GensimLsiGenModelApplier(
                        normalizedTfIdfVector.instances.trainfile,
                        numDim,
                        self.fileref
                    )
                })
                
                val instances = normalizedTfIdfVector.instances.map(lsiVectorPath, (normalizedTfIdf, lsi) => {
                    GensimLsiApplyModelApplier(
                        normalizedTfIdf.file,
                        filter.file,
                        lsi.fileref
                    )
                })
                
                def svmClassifier(topClass: String) = {
                    val targetClassfun = (mscClasses: List[String]) => mscClasses.exists(_.substring(0, 2) == topClass)
                    
                    new ClassifierSubgroup() {
                        val classifier = new FilePlan(lsiVectorPath / ("svm_" + topClass), self => {
                            JoachimsSVMLearnApplier(
                                Map("-v" -> List("0")),
                                new ArffJsonInstancesFile(instances.trainfile),
                                self.fileref,
                                targetClassfun
                            )
                        })
                        
                        val results = new FilePlan(lsiVectorPath / ("svm_results_" + topClass + ".json"), self => {
                            JoachimsSVMClassifyApplier(
                                Map("-v" -> List("0")),
                                new ArffJsonInstancesFile(instances.testfile),
                                classifier.file,
                                self.fileref,
                                targetClassfun
                            )
                        })
                    }
                }
            }
            
            val oddsRatioPath = titleOnlyPath / "odds_ratio" !
            def oddsRatio(numFeatures: Int, topClass: String) = {
                def svmClassifier(topClass: String) = new ClassifierSubgroup() {
                    val documentsPath = oddsRatioPath / ("class_" + topClass + "_" + numFeatures + "_features") !
                    val targetClassfun = (mscClasses: List[String]) => mscClasses.exists(_.substring(0, 2) == topClass)
                    
                    lazy val bestFeatures = {
                        val rankedList = new OddsRatio(new ArffJsonInstancesFile(rawVector.instances.trainfile)).rankedFeatureList(topClass.toInt)
                        rankedList.take(numFeatures)
                    }
                    
                    val instances = rawVector.instances.map(documentsPath, (rawVector, oddsRatio) => {
                        new ArffJsonInstancesFile(rawVector.file)
                            .project(bestFeatures.map(_._1))
                            .save(oddsRatio.fileref)
                    })
                    
                    val classifier = new FilePlan(oddsRatioPath / ("classifier_" + topClass + ".svm"), self => {
                        JoachimsSVMLearnApplier(
                            Map("-v" -> List("0")),
                            new ArffJsonInstancesFile(instances.trainfile),
                            self.fileref,
                            targetClassfun
                        )
                    })
                    
                    val results = new FilePlan(oddsRatioPath / ("classifier_" + topClass + ".svm"), self => {
                        JoachimsSVMClassifyApplier(
                            Map("-v" -> List("0")),
                            new ArffJsonInstancesFile(instances.testfile),
                            classifier.file,
                            self.fileref,
                            targetClassfun
                        )
                    })
                }
            }
        }
        
        val journalOnlyPath = rootPath / "journal_only" !
        val journalOnly = new Group() {
            val rawPath = journalOnlyPath / "raw" !
            val raw = base.map(rawPath, (raw, journalOnly) => {
                new PaperJson2JournalOnlyArffJson(
                    inFile = raw.file, 
                    outFile = journalOnly.fileref
                ).run
            })
            
            val rawVector = new Subgroup() {
                val rawVectorPath = journalOnlyPath / "raw_vector" !
                
                val filter = new FilePlan(rawVectorPath / "filter", self => {
                    val f = new VectorFromDictFilter.Conf1
                    f.buildDict(new ArffJsonInstancesFile(raw.trainfile))
                    f.save(self.fileref)
                })
                
                val instances = raw.map(rawVectorPath, (raw, vector) => {
                    val f = VectorFromDictFilter.load(filter.file)
                    
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(f)
                        .save(vector.fileref)
                })
                
                def naiveBayesClassifier(topClass: String) = {
                    val targetClassDef = TopClassIs(topClass)
                    
                    new ClassifierSubgroup() {
                        val classifier = new FilePlan(rawVectorPath / ("classifier_" + topClass + ".bayes"), self => {
                            val gen = new WekaClassifierGenerator() {
                                def classifierConfig() = new NaiveBayes()
                            }
                            
                            gen.genClassifier(instances.trainfile, targetClassDef, self.fileref)
                        })
                        
                        val results = new FilePlan(rawVectorPath / ("naive_bayes_" + topClass + "_results.json"), self => {
                            val appl = new WekaClassifierApplier(classifier.file)
                            appl.applyClassifier(new ArffJsonInstancesFile(instances.testfile), targetClassDef, self.fileref)
                        })
                    }
                }
            }
            
            val nominal = new Subgroup() {
                val nominalPath = journalOnlyPath / "nominal" !
                
                val filter = new FilePlan(nominalPath / "filter", self => {
                    val filter = new NominalValueFromDictFilter.Conf1
                    filter.expandDict(raw.trainfile)
                    
                    filter.save(self.fileref)
                })
                
                val instances = raw.map(nominalPath, (raw, nominal) => {
                    val f = NominalValueFromDictFilter.load(filter.file)
                
                    new ArffJsonInstancesFile(raw.file)
                        .applyFilter(f)
                        .save(nominal.fileref)
                })
                
                def naiveBayesClassifier(topClass: String) = {
                    val targetClassDef = TopClassIs(topClass)
                    
                    new ClassifierSubgroup() {
                        val classifier = new FilePlan(nominalPath / ("classifier_" + topClass + ".bayes"), self => {
                            val gen = new WekaClassifierGenerator() {
                                def classifierConfig() = new NaiveBayes()
                            }
                            
                            gen.genClassifier(instances.trainfile, targetClassDef, self.fileref)
                        })
                        
                        val results = new FilePlan(nominalPath / ("naive_bayes_" + topClass + "_results.json"), self => {
                            val appl = new WekaClassifierApplier(classifier.file)
                            appl.applyClassifier(new ArffJsonInstancesFile(instances.testfile), targetClassDef, self.fileref)
                        })
                    }
                }
            }
        }
        
        val combined = new Group() {
            
        }
        
        /*for(classifierSubgroup <- journalOnly.nominal.naiveBayesClassifiers) {
            classifierSubgroup.results.file
        }*/
        
        /*for(classifierSubgroup <- abstractOnly.lsiVector.svmClassifiers) {
            classifierSubgroup.results.file
        }*/
        
        for(topClass <- topClasses) {
            ClassifierApplier.fromFile(titleOnly.lsiVector(100).svmClassifier(topClass).classifier.file)
        }
    }*/
}





















