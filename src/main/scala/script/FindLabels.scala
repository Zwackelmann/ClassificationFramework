package script

import common.TrainTuningTestSetSelection
import parser.{
    ArffJsonInstancesSource, 
    History,
    ContentDescribable
}
import filter.{
	CategorySelectionFilter, 
	VectorFromDictFilter, 
	NormalizeVectorFilter, 
	OddsRatioFilter, 
	TfIdfFilter
}
import classifier.{
    PrioritizeMainClass, 
    NoTrainSetSelection
}
import classifier.CategoryIs
import classifier.Classifier
import model.{
    RawClassification
}
import java.io.File
import filter.ProjectionFilter
import model.CertaintyToThresholdFunction
import java.io.BufferedWriter
import java.io.FileWriter
import classifier.BalancedTrainSetSelection
import classifier.CategoryIsMSC
import classifier.CategorizationHierarchy
import classifier.CategorizationHierarchy

object FindLabels {
	def main(args: Array[String]) {
	    val corpus = ArffJsonInstancesSource("data/arffJson/corpus.json")
	    val ((trainSet, tuningSet, testSet), c) = TrainTuningTestSetSelection.getSets(100, "min100", corpus, (0.7, 0.3, 0.0))
	    
        val allThirdLevelClasses = ApplyFinalClassifier.findConsideredCategories(corpus, 3, 100).map(cStr => CategoryIsMSC(cStr))
        val allSecondLevelClasses = ApplyFinalClassifier.findConsideredCategories(corpus, 2, 100).map(cStr => CategoryIsMSC.topAndMiddle(cStr.substring(0, 2), cStr.substring(2, 3)))
        val allFirstLevelClasses = ApplyFinalClassifier.findConsideredCategories(corpus, 1, 100).map(cStr => CategoryIsMSC.top(cStr.substring(0, 2)))
        
        val minCertaintyThreshold = 0.5
        
        for(firstLevelCat <- allFirstLevelClasses) yield {
            val learner = SvmLightJniLearner(
                new History() 
                        with AbstractProjection
                        with CategorySelectionFilter.Appendix
                        with VectorFromDictFilter.Appendix
                        with TfIdfFilter.Appendix
                        with NormalizeVectorFilter.Appendix { 
                    
                    val selection = firstLevelCat.parent
                    val confName = "conf9"
                    override val minOcc = 3
                },
                BalancedTrainSetSelection(Some(5000))
            )
            
            println("calc first level classifiers for category " + firstLevelCat)
            val firstLevelResults = classificationRun(firstLevelCat, trainSet, tuningSet, testSet, minCertaintyThreshold)
            println("firstLevelPositives: " + (firstLevelResults match { case Some(x) => x._1.size.toString case None => "no results"}))
            val writer = new BufferedWriter(new FileWriter(new File(firstLevelCat.filenameExtension)))
            
            val currentSecondLevelClasses = allSecondLevelClasses.filter(_.parent.filenameExtension == firstLevelCat.filenameExtension)
            for(secondLevelCat <- currentSecondLevelClasses) yield {
                firstLevelResults match {
                    case Some(firstLevelResults) => {
                        val firstLevelPositives = firstLevelResults._1
                        val firstLevelCertaintyList = firstLevelResults._2
                        
	                    val secondLevelResults = classificationRun(secondLevelCat, trainSet, tuningSet, firstLevelPositives, minCertaintyThreshold)
	                    println("secondLevelPositives: " + (secondLevelResults match { case Some(x) => x._1.size.toString case None => "no results"}))
	                    
	                    val currentThirdLevelClasses = allThirdLevelClasses.filter(_.parent.filenameExtension == secondLevelCat.filenameExtension)
			            for(thirdLevelCat <- currentThirdLevelClasses) yield {
			                secondLevelResults match {
			                    case Some(secondLevelResults) => {
			                        val secondLevelPositives = secondLevelResults._1
			                        val secondLevelCertaintyList = secondLevelResults._2
			                        
				                    val thirdLevelResults = classificationRun(thirdLevelCat, trainSet, tuningSet, secondLevelPositives, minCertaintyThreshold)
				                    println("thirdLevelPositives: " + (thirdLevelResults match { case Some(x) => x._1.size.toString case None => "no results"}))
				                    
				                    thirdLevelResults match {
			                            case Some(thirdLevelResults) => {
			                                val thirdLevelPositives = thirdLevelResults._1
			                                val thirdLevelCertaintyList = thirdLevelResults._2
			                                
			                                val results = certaintyStackTrace(firstLevelCertaintyList, secondLevelCertaintyList, thirdLevelCertaintyList)
			                                for(r <- results) {
			                                    writer.write(r + "\n")
			                                }
			                            }
			                            case None => 
			                        }
			                    }
			                    case None => 
			                }
			            }
                    }
                    case None => 
                }
            }
            
            writer.close()
        }
        
	}
	
	def parentCat(catFilenameExt: String) = {
	    val (top, middle, bottom) = if(catFilenameExt.length() == 5) (
	        catFilenameExt.substring(0, 2),
	        catFilenameExt.substring(2, 3),
	        catFilenameExt.substring(3, 5)
	    ) else (
	        catFilenameExt.substring(1, 3),
	        catFilenameExt.substring(3, 4),
	        catFilenameExt.substring(4, 6)
	    )
	    
	    (if(bottom == "__" && middle == "_") CategoryIsMSC.top(top)
	        else if(bottom == "__") CategoryIsMSC.topAndMiddle(top, middle)
	        else CategoryIsMSC.topMiddleAndLeave(top, middle, bottom)
	    ).parent.filenameExtension
	}
	
	def certaintyStackTrace(firstLevel: List[Tuple3[String, String, Double]], secondLevel: List[Tuple3[String, String, Double]], thirdLevel: List[Tuple3[String, String, Double]]) = {
	    for(
	        t <- thirdLevel;
	        s <- secondLevel if s._1 == t._1 && s._2 == parentCat(t._2);
	        f <- firstLevel if f._1 == s._1 && f._2 == parentCat(s._2)
	    ) yield (t._1, t._2, (f._3, s._3, t._3))
	}
	
	def classificationRun(cat: CategoryIs with CategorizationHierarchy, trainSet: ArffJsonInstancesSource with ContentDescribable, tuningSet: ArffJsonInstancesSource with ContentDescribable, testSet: ArffJsonInstancesSource, certaintyThreshold: Double) = {
	    val (classifiers, meta, coofficients, finalThreshold, thresholds, fMeasures, certaintyFunctions) = classifiersAndCoofficients(cat, trainSet, tuningSet)
        
        val (testClassifications, certaintyFunction) = if(coofficients.isDefined) {
            val thres = thresholds.map(_.get)
            val classifications = ((classifiers zip meta) zip thresholds).map(x => {
                val c = x._1._1
                val m = x._1._2
                val t = x._2
                
                val mappedTestSet = m.get("learner") match {
                    case "svm" => mapSetForSVM(testSet, cat, m.get("proj"))
                    case "c45" => mapSetForC45(testSet, cat, m.get("proj"))
                }
                
                RawClassification.normalize(RawClassification.withThreshold(c.get.calculateClassifications(mappedTestSet).toList, t.get))
            })
            
            val combinedResult = RawClassification.withThreshold(RawClassification.weightedSumWithCoofficients(classifications, coofficients.get), finalThreshold.get)
            // Some(combinedResult) // TODO
            (None, None)
        } else {
            if(classifiers.exists(_ != None)) {
                val indexWithMaxFMeasure = fMeasures.filter(_ != None).map(_.get).zipWithIndex.maxBy(_._1)._2
                val c = classifiers(indexWithMaxFMeasure).get
                val m = meta(indexWithMaxFMeasure).get
                val certaintyFunction = certaintyFunctions(indexWithMaxFMeasure).get
                val t = thresholds(indexWithMaxFMeasure).get
                
                val mappedTestSet = m("learner") match {
                    case "svm" => mapSetForSVM(testSet, cat, m("proj"))
                    case "c45" => mapSetForC45(testSet, cat, m("proj"))
                }
                
                val bestSingleClassifications = RawClassification.normalize(RawClassification.withThreshold(c.calculateClassifications(mappedTestSet).toList, t))
                (Some(bestSingleClassifications), Some(certaintyFunction))
            } else {
                (None, None)
            }
        }
        
        testClassifications map (c => {
            val positivesList = c.filter(rc => certaintyFunction.get.classificationToCertainty(rc.classification) > certaintyThreshold)
            val certaintyList = positivesList.map(rc => (rc.id, cat.filenameExtension, certaintyFunction.get.classificationToCertainty(rc.classification)))
        	val positivesIds = positivesList.map(_.id).toSet
        	(testSet.filter(inst => positivesIds.contains(inst.id)), certaintyList)
        })
	}
	
	def classifiersAndCoofficients(cat: CategoryIs with CategorizationHierarchy, trainSet: ArffJsonInstancesSource with ContentDescribable, tuningSet: ArffJsonInstancesSource with ContentDescribable) = {
	    val minWordCount = cat.targetLevel match { case 1 => 3 case _ => 1 }
        val orTh = cat.targetLevel match { case 1 => 5 case _ => 2 }
        val maxTrainSetSize = 16000
        val numAdaBoostIterations = 20
            
        val c45AbstractLearner = (BoostedC45LearnerNT(
            new History() 
                    with AbstractProjection
                    with CategorySelectionFilter.Appendix
                    with VectorFromDictFilter.Appendix
                    with NormalizeVectorFilter.Appendix
                    with OddsRatioFilter.Appendix { 
                
                val selection = cat.parent
                val confName = "conf9"
                override val minOcc = minWordCount
                val orThreshold = orTh.toDouble
                val numWorst = 0
            },
            PrioritizeMainClass(maxTrainSetSize),
            numAdaBoostIterations
        ), Map("proj" -> "abs", "learner" -> "c45"))
            
        
        val c45TitleLearner = (BoostedC45LearnerNT(
            new History() 
                    with TitleProjection
                    with CategorySelectionFilter.Appendix
                    with VectorFromDictFilter.Appendix
                    with NormalizeVectorFilter.Appendix
                    with OddsRatioFilter.Appendix { 
                
                val selection = cat.parent
                val confName = "conf9"
                override val minOcc = minWordCount
                val orThreshold = orTh.toDouble
                val numWorst = 0
            },
            PrioritizeMainClass(maxTrainSetSize),
            numAdaBoostIterations
        ), Map("proj" -> "tit", "learner" -> "c45"))
        
        val svmAbstractLearner = (SvmLearnerNT(
            new History() 
                    with AbstractProjection
                    with CategorySelectionFilter.Appendix
                    with VectorFromDictFilter.Appendix
                    with TfIdfFilter.Appendix
                    with NormalizeVectorFilter.Appendix { 
                
                val selection = cat.parent
                val confName = "conf9"
                override val minOcc = minWordCount
            },
            NoTrainSetSelection
        ), Map("proj" -> "abs", "learner" -> "svm"))
        
        val svmTitleLearner = (SvmLearnerNT(
            new History() 
                    with TitleProjection
                    with CategorySelectionFilter.Appendix
                    with VectorFromDictFilter.Appendix
                    with TfIdfFilter.Appendix
                    with NormalizeVectorFilter.Appendix { 
                
                val selection = cat.parent
                val confName = "conf9"
                override val minOcc = minWordCount
            },
            NoTrainSetSelection
        ), Map("proj" -> "tit", "learner" -> "svm"))
        
        val learnerListMeta = List(/*c45AbstractLearner, c45TitleLearner, */svmAbstractLearner/*, svmTitleLearner*/)
        val classifiersMeta = learnerListMeta.map(learnerMeta => {
            val classifier = Some(learnerMeta._1.classifier(trainSet, cat))
            
            val trainResults = learnerMeta._1.classifications(trainSet, tuningSet, cat)
            val bestFMeasureThreshold = RawClassification.findBestThreshold(trainResults)
            val thresholdedTuningResults = RawClassification.normalize(RawClassification.withThreshold(trainResults, bestFMeasureThreshold))
            val certaintyFunction = CertaintyToThresholdFunction(thresholdedTuningResults)
            
            if(!classifier.isDefined) Some((classifier.get, learnerMeta._2, thresholdedTuningResults, bestFMeasureThreshold, certaintyFunction))
            else None
        })
        
        
        
        /*val normalizedAndThresholdedTuningResultsWithThreshold = (for(cm <- classifiersMeta) yield {
            cm.map(cm => {
                val tuningClassifications = cm._1.calculateClassifications(
                    cm._2("learner") match {
                        case "svm" => mapSetForSVM(tuningSet, cat, cm._2("proj"))
                        case "c45" => mapSetForC45(tuningSet, cat, cm._2("proj"))
                    }).toList
                    
                val bestFMeasureThreshold = RawClassification.findBestThreshold(tuningClassifications)
                val thresholdedResults = RawClassification.normalize(RawClassification.withThreshold(tuningClassifications, bestFMeasureThreshold))
                val certaintyFunction = CertaintyToThresholdFunction(thresholdedResults)
                (thresholdedResults, bestFMeasureThreshold, certaintyFunction)
            })
        }).toList*/
        
        // TODO => "false" in der naechsten Zeile...
        val (coofficients, finalThreshold) = if(false/* && normalizedAndThresholdedTuningResultsWithThreshold.forall(_ != None)*/) {
        	/*val r = normalizedAndThresholdedTuningResultsWithThreshold.map(_.get._1)
        	val coofficients = CombineClassifiers.findBestCoofficients(r)
        	
        	val t = RawClassification.findBestThresholdWithPrecison(RawClassification.weightedSumWithCoofficients(r, coofficients), targetPrecision)
        	(Some(coofficients), Some(t))
        	*/
            (None, None)
        } else {
            (None, None)
        }
        
        val classifiers = classifiersMeta.map(_.map(_._1))
        val meta = classifiersMeta.map(_.map(_._2))
        val fMeasures = classifiersMeta.map(oResults => oResults.map(results => Classifier.fMeasure(results._3, 1.0)))
        val thresholds = classifiersMeta.map(oResults => oResults.map(results => results._4))
        val certaintyFunctions = classifiersMeta.map(oResults => oResults.map(results => results._5))
        
        Tuple7(classifiers, meta, coofficients, finalThreshold, thresholds, fMeasures, certaintyFunctions)
	}
	
	def mapSetForSVM(inst: ArffJsonInstancesSource, cat: CategoryIs with CategorizationHierarchy, projection: String) = {
	    val minWordCount = cat.targetLevel match { case 1 => 3 case _ => 1 }
	    val projectionFilter = if(projection == "tit") new ProjectionFilter(Set(0)) else new ProjectionFilter(Set(1))
	    val vectorFromDictFilter = common.ObjectToFile.readObjectFromFile(new File("data/filter/proj-" + projection + "_sel-" + cat.parent.filenameExtension + "__proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount)).asInstanceOf[VectorFromDictFilter]
	    val tfidfFilter = common.ObjectToFile.readObjectFromFile(new File("data/filter/proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount + "__proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount + "_tf-idf")).asInstanceOf[TfIdfFilter]
	    val normalizeFilter = new NormalizeVectorFilter
	    
	    inst.applyFilter(projectionFilter)
	    	.applyFilter(vectorFromDictFilter)
	    	.applyFilter(tfidfFilter)
	    	.applyFilter(normalizeFilter)
	}
	
	def mapSetForC45(inst: ArffJsonInstancesSource, cat: CategoryIs with CategorizationHierarchy, projection: String) = {
	    val minWordCount = cat.targetLevel match { case 1 => 3 case _ => 1 }
	    val or = cat.targetLevel match { case 1 => 5 case _ => 2 }
	    val projectionFilter = if(projection == "tit") new ProjectionFilter(Set(0)) else new ProjectionFilter(Set(1))
	    val vectorFromDictFilter = common.ObjectToFile.readObjectFromFile(new File("proj-" + projection + "_sel-" + cat.parent.filenameExtension + "__proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount)).asInstanceOf[VectorFromDictFilter]
	    val normalizeFilter = new NormalizeVectorFilter
	    val orFilter = common.ObjectToFile.readObjectFromFile(new File("proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount + "_norm__proj-" + projection + "_sel-" + cat.parent.filenameExtension + "_vec-conf9-min-" + minWordCount + "_norm_or-" + or + ".0-0-" + cat.filenameExtension)).asInstanceOf[OddsRatioFilter]
	    
	    inst.applyFilter(projectionFilter)
	    	.applyFilter(vectorFromDictFilter)
	    	.applyFilter(normalizeFilter)
	    	.applyFilter(orFilter)
	}
}





























