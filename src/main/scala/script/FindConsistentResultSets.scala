package script
import java.io.File
import parser.ArffJsonInstancesSource
import scala.collection.mutable
import classifier.CategoryIs
import model.RawClassification
import java.nio.file.Files
import java.nio.file.StandardCopyOption._;

object FindConsistentResultSets {
	def main(args: Array[String]) {
	    val corpus = ArffJsonInstancesSource("data/arffJson/corpus.json")
	    val layer = 3
	    val set = "tuning"
	    val learner = "c45"
	    val projs = List("tit", "abs", "jour", "ter", "uni")
	    
	    val consideredCategories = layer match {
	        case 1 => common.Common.topClasses map (c => CategoryIs.top(c))
	        case 2 => ApplyFinalClassifier.findConsideredCategories(corpus, layer, 100).map(c => CategoryIs.topAndMiddle(c.substring(0, 2), c.substring(2, 3)))
	        case 3 => ApplyFinalClassifier.findConsideredCategories(corpus, layer, 100).map(c => CategoryIs.topMiddleAndLeave(c.substring(0, 2), c.substring(2, 3), c.substring(3, 5)))
	    }
	    val realSetIds = ArffJsonInstancesSource("data/arffJson/min100-" + set + ".json").map(_.id).toSet
	    
	    val resultDirnames = List(
	        "data/results(x)",
	        "data/results(merged into results)",
	        "data/results(tbdb)",
	        "data/results(sascha)",
	        "data/results(pc)",
			"data/results(nb)"
	    )
	    
	    val targetDir = new File("consitentResults")
	    targetDir.mkdir()
	    
	    val lines = new scala.collection.mutable.ListBuffer[String]
	    for(cat <- consideredCategories; proj <- projs) {
	        val files = if(learner == "c45") {
	            filenamesC45_2(cat.parent.filenameExtension, cat.filenameExtension, set, layer, proj)
	        } else if(learner == "svm") {
	            filenamesSVM_2(cat.parent.filenameExtension, cat.filenameExtension, set, layer, proj)
	        } else {
	            throw new RuntimeException()
	        } 
	        
	        val candidates = files.map(filename => {
	            (for(resultDirname <- resultDirnames) yield {
	                val fullFilename = resultDirname + "/" + filename
	                if(new File(fullFilename).exists()) List(Pair(RawClassification.fromFile(fullFilename).map(_.id).toSet, fullFilename))
	                else List()
	            }).flatten
	        })
	        
	        for((fileTypeList, fileTypeName) <- candidates zip files) {
	            // if(!fileTypeList.isEmpty) println("found file for " + fileTypeName)
	            for((rset, file) <- fileTypeList) yield {
		            val valid = rset.forall(id => realSetIds.contains(id))
		            if(!valid) println("   ! " + file) else println("   / " + file)
		            val srcPath = new File(file).toPath()
		            val targetPath = targetDir.toPath().resolve(new File(file).toPath().getFileName())
		            
		            if(valid) {
		                if(!targetPath.toFile.exists() || targetPath.toFile().length() < srcPath.toFile().length()) {
		                	Files.copy(srcPath, targetPath, REPLACE_EXISTING)
		                }
		            } else {
		                // println("invalid file: " + fileList._2)
		            }
	            }
	            
	            if(!new File(targetDir.getName() + "/" + fileTypeName).exists()) {
	                lines += fileTypeName
	                println("(X) missing file for: " + fileTypeName)
	            } else {
	                println("(/) found matching file for: " + fileTypeName)
	            }
	        }
	        
	        /*println(candidates.map(filetypeList => filetypeList.map(fileType => fileType._1.size)).mkString("\n"))
	        
	        val validity = for(check <- allCross(candidates).iterator) yield (check.map(_._2), consistencyCheck(check.map(_._1)))
	        validity.find(_._2) match {
	            case Some((files, _)) => {
	                val paths = files.map(_.toPath())
	                
	                println("copy set for: " + cat.filenameExtension)
	                for(path <- paths) {
	                	Files.copy(path, targetDir.toPath().resolve(path.getFileName()))
	                }
	            }
	            case None => {
	            	println("\n\nno consistent set found for: " + cat.filenameExtension + "\n\n")
	            	/*val filesC45 = filenamesC45(cat.parent.filenameExtension, cat.filenameExtension, "tuning", 1)
	            	val candidatesC45 = filesC45.map(filename => {
			            (for(resultDirname <- resultDirnames) yield {
			                val file = new File(resultDirname + "/" + filename)
			                if(file.exists()) List(Pair(RawClassification.fromFile(file).map(_.id), file))
			                else List()
			            }).flatten
			        })
			        
			        val validityC45 = for(check <- allCross(candidatesC45)) yield (check.map(_._2), consistencyCheck(check.map(_._1)))
			        validityC45.find(_._2) match {
			            case Some((files, _)) => println(files + " would match with appropiate svms...")
			            case None => println("recalculate")
			        }*/
	            }
	        }*/
	    }
	    
	    println("report: ")
	    println(lines.mkString("\n"))
	}
	
	def filenames(parentCat: String, cat: String, set: String) = List(
		    "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-1_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-1_tf-idf_norm.json",
		    "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-1_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-1_tf-idf_norm.json",
		    "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-1_norm_or-2.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-1_norm_or-2.0-0-" + cat + ".json",
		    "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-1_norm_or-2.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-1_norm_or-2.0-0-" + cat + ".json"
		)
	
	def filenamesC45_2(parentCat: String, cat: String, set: String, layer: Int, proj: String) = {
	    val or = layer match { case 1 => 5 case _ => 2 }
	    val m = layer match { case 1 => 3 case _ => 1 }
	    
	    
	    List(if(proj == "abs") {
	        "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + ".json"
	    } else if(proj == "tit") {
	        "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + ".json"
	    } else if(proj == "jour") {
	        "min100_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + ".json"
	    } else if(proj == "ter") {
	        "min100_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + ".json"
	    } else if(proj == "uni") {
	        "min100_uni-at_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_uni-at_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + ".json"
	    } else {
	        throw new IllegalArgumentException("invalid proj")
	    })
	}
		
	def filenamesC45(parentCat: String, cat: String, set: String, layer: Int) = {
	    val or = layer match { case 1 => 5 case _ => 2 }
	    val m = layer match { case 1 => 3 case _ => 1 }
	    
	    List(
		    "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + ".json",
		    "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_norm_or-" + or + ".0-0-" + cat + ".json",
		    "min100_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + ".json",
		    "min100_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + "_c45-boost-20_thd_all_" + cat + "_min100-" + set + "_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_norm_or-300.0-0-" + cat + ".json"
		)
	}
	
	def filenamesSVM(parentCat: String, cat: String, set: String, layer: Int) = {
	    val or = layer match { case 1 => 5 case _ => 2 }
	    val m = layer match { case 1 => 3 case _ => 1 }
	    
	    List(
		    "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm.json",
		    "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm.json",
		    "min100_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + "_svm_all_min100-" + set + "_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + ".json",
		    "min100_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + "_svm_all_min100-" + set + "_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + ".json"
		)
	}
	
	def filenamesSVM_2(parentCat: String, cat: String, set: String, layer: Int, proj: String) = {
	    val or = layer match { case 1 => 5 case _ => 2 }
	    val m = layer match { case 1 => 3 case _ => 1 }
	    
	    
	    List(if(proj == "abs") {
	        "min100_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-abs_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm.json"
	    } else if(proj == "tit") {
	        "min100_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_proj-tit_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm.json"
	    } else if(proj == "jour") {
	        "min100_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + "_svm_all_" + cat + "_min100-" + set + "_proj-jour_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + ".json"
	    } else if(proj == "ter") {
	        "min100_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + "_svm_all_" + cat + "_min100-" + set + "_proj-ter_sel-" + parentCat + "_vec-conf2-min-1_or-300.0-0-" + cat + ".json"
	    } else if(proj == "uni") {
	        "min100_uni-at_sel-" + parentCat + "_vec-conf9-min-" + m + "_tf-idf_norm_svm_all_" + cat + "_min100-" + set + "_uni-at_sel-" + parentCat +"_vec-conf9-min-" + m + "_tf-idf_norm.json"
	    } else {
	        throw new IllegalArgumentException("invalid proj")
	    })
	}
		
	def findConsideredCategories(corpus: ArffJsonInstancesSource, categoryCondition: String => Boolean, categoryMapping: String => String, minOccurences: Int) = { 
        val categoryCount = new mutable.HashMap[String, Int]() {
            override def default(key: String) = 0
        } 
        
        for(inst <- corpus; cat <- inst.categories.filter(categoryCondition).map(categoryMapping).distinct) {
            categoryCount(cat) += 1
        }
        
        categoryCount.toList.filter(c => c._2 >= minOccurences).sortBy(_._1).map(_._1)
    }
	
	def consistencyCheck(ids: List[Set[String]]) = {
        val classificationsValid = ids.forall(_.size == ids.head.size) /* && (ids.map(_.sortBy(c => c)).transpose.map(c => 
            if(c.isEmpty) true
            else c.forall(_ == c.head)
        ).forall(b => b))*/
        
        /*if(!classificationsValid) {
            println(ids.reduceLeft(_ intersect _).size.toDouble / ids.map(_.size).max)
        }*/
        
        classificationsValid
	}
	
	def crossList[T](es1: List[List[T]], es2: List[T]): List[List[T]] =
	    for(e1 <- es1; e2 <- es2) yield e1 :+ e2
	
	def cross[T](es1: List[T], es2: List[T]): List[List[T]] = 
	    for(e1 <- es1; e2 <- es2) yield List(e1, e2)
	
	def allCross[T](l: List[List[T]]) = (
	    (l.head.map(List(_)) /: l.tail)((old, next) => ({
	        crossList(old, next)
	    }))
	)
	
	val inconsistentClasses = List(
	    "11B39", "11F30", "11F46", "11K38", "11L07", "11R27", "11R42", "13A15", "13A50", "13H10",
		"14A20", "14F35", "14G05", "14J17", "14J29", "14P10", "14Q05", "15A45", "15A54", "16E30",
		"16E65", "16S34", "16S38", "17B01", "17B30", "18C15", "18D50", "20D05", "20D15", "20F10",
		"20F18", "20K10", "20K27", "22C05", "22E25", "26A51", "26C10", "30B10", "30C15", "30F35",
		"30G20", "32G15", "32L05", "33C20", "33C55", "34A37", "34A60", "34C05", "34C11", "34C23",
		"34C25", "34C29", "34C37", "34D05", "34D10", "34D99", "34E15", "34G20", "34K05", "34K06",
		"34K11", "34K12", "34K17", "34K25", "34K40", "34L05", "34L20", "34M10", "34M35", "35A01",
		"35A02", "35A08", "35A24", "35A35", "35B10", "35B27", "35B34", "35B35", "35B40", "35B41",
		"35B50", "35C05", "35C15", "35D30", "35F25", "35G30", "35H10", "35J08", "35J10", "35J25",
		"35J45", "35J57", "35J65", "35J85", "35K10", "35K15", "35K35", "35K40", "35K51", "35K59",
		"35K85", "35L15", "35L45", "35L65", "35L67", "35L75", "35L80", "35P05", "35P25", "35Q20",
		"35Q40", "35Q55", "35Q61", "35Q72", "35Q80", "35Q84", "35Q99", "35R11", "35R45", "35S10",
		"37A20", "37A45", "37A50", "37B10", "37B15", "37B30", "37B99", "37C20", "37C30", "37C70",
		"37D05", "37D10", "37D30", "37D35", "37D50", "37E30", "37F35", "37G05", "37G40", "37H99",
		"37J05", "37J20", "37J25", "37J45", "37K10", "37K30", "37K50", "37L10", "37M10", "37M15",
		"37M99", "37N05", "37N20", "37N99", "39A12", "39A22", "39B12", "39B72", "39B82", "40B05",
		"40C05", "41A05", "41A20", "41A29", "41A46", "41A60", "42A10", "42A16", "42A32", "42A38",
		"42B05", "42B20", "42C05", "43A05", "43A22", "43A60", "43A62", "43A85", "43A90", "44A15",
		"45A05", "45E05", "45G15", "45M05", "45P05", "45Q05", "46A16", "46A40", "46B03", "46B10",
		"46B25", "46B42", "46B99", "46E10", "46E15", "46E25", "46E30", "46F05", "46G05", "46H05",
		"46J15", "46L10", "46L51", "46L52", "46L55", "46L57", "46L80", "46M05", "46N30", "47A05",
		"47A12", "47A20", "47A30", "47A45", "47A48", "47A53", "47A58", "47A64", "47B07", "47B25",
		"47B36", "47B37", "47B40", "47B47", "47B50", "47B99", "47D09", "47G10", "47H05", "47H10",
		"47H11", "47H40", "47J05", "47J15", "47J30", "47L25", "47N10", "47S40", "49J27", "49J30",
		"49J45", "49J50", "49J55", "49K27", "49K45", "49M15", "49M37", "49N35", "49N45", "49N90",
		"49Q05", "49Q20", "51A50", "51E21", "51K05", "51M15", "51N20", "52A01", "52A15", "52A20",
		"52A30", "52A41", "52B12", "52C07", "52C20", "53A04", "53A05", "53A15", "53A17", "53A35",
		"53B20", "53B35", "53C07", "53C17", "53C23", "53C24", "53C27", "53C28", "53C35", "53C43",
		"53C55", "53C80", "53D15", "53D35", "53D40", "53D55", "53Z05", "54A20", "54B05", "54C05",
		"54C25", "54C60", "54D15", "54D20", "54D55", "54E15", "54E50", "54E99", "54F50", "54H15",
		"55M30", "55P10", "55P15", "55P48", "55P60", "55Q05", "55S10", "57M07", "57M20", "57M60",
		"57N13", "57N65", "57R19", "57R20", "57R45", "57R70", "57S25", "58A20", "58B32", "58D05",
		"58D27", "58E15", "58E20", "58E50", "58J20", "58J37", "58J50", "58J65", "60A10", "60B05",
		"60B12", "60B15", "60D05", "60E15", "60F15", "60G09", "60G18", "60G40", "60G42", "60G48",
		"60G50", "60G55", "60G99", "60H15", "60H35", "60J10", "60J27", "60J35", "60J60", "60J65",
		"60J80", "60K10", "60K30", "60K99", "62C12", "62D05", "62E10", "62E20", "62E99", "62F07",
		"62F25", "62F99", "62G09", "62G30", "62H05", "62H10", "62H15", "62H17", "62H30", "62J05",
		"62J15", "62K10", "62L10", "62M05", "62M07", "62M15", "62M20", "62M45", "62N03", "62P10",
		"62P25", "62Q05", "65C30", "65C40", "65D05", "65D07", "65D17", "65D30", "65F08", "65F20",
		"65F35", "65G40", "65G50", "65H10", "65H20", "65J20", "65K15", "65L10", "65L50", "65L99",
		"65M15", "65M20", "65M32", "65M50", "65M70", "65N15", "65N30", "65N55", "65P30", "65R32",
		"65T40", "65Y05", "65Y10", "65Z05", "68M12", "68M99", "68N19", "68N99", "68P20", "68P25",
		"68Q05", "68Q10", "68Q17", "68Q32", "68Q60", "68Q85", "68R15", "68T15", "68T20", "68T35",
		"68T37", "68T45", "68U07", "68U99", "68W20", "68W32", "70E55", "70E60", "70F07", "70F10",
		"70F40", "70H05", "70H40", "70K40", "70L05", "70S15", "74A05", "74A20", "74A25", "74A45",
		"74B05", "74C10", "74D05", "74E10", "74E35", "74F05", "74G05", "74G10", "74G65", "74H15",
		"74H40", "74H60", "74J20", "74J99", "74K05", "74K20", "74K25", "74L05", "74M10", "74N05",
		"74P15", "74R10", "74S15", "74S20", "74S60", "74S99", "76A15", "76B07", "76B45", "76B99",
		"76D08", "76D27", "76D33", "76D55", "76D99", "76E07", "76E20", "76F02", "76F25", "76F60",
		"76H05", "76J20", "76M12", "76M15", "76M25", "76M40", "76M60", "76N20", "76Q05", "76R99",
		"76S05", "76T20", "76T25", "76U05", "76Y05", "78A10", "78A40", "78A50", "78M10", "78M15",
		"78M50", "80A05", "80A20", "80A30", "80M20", "81P10", "81P45", "81Q05", "81Q10", "81Q30",
		"81Q35", "81Q60", "81R05", "81R25", "81R60", "81S25", "81T10", "81T13", "81T17", "81T18",
		"81T30", "81T60", "81T99", "81U35", "81V10", "81V25", "81V35", "81V65", "81V70", "82B03",
		"82B21", "82B27", "82B35", "82B44", "82C10", "82C20", "82C24", "82C26", "82C32", "82C44",
		"82D10", "82D30", "82D50", "82D80", "82D99", "83C05", "83C10", "83C22", "83C35", "83C50",
		"83C65", "83D05", "83E50", "83F05", "85A25", "85A30", "86A05", "86A22", "86A60", "90B15",
		"90B25", "90B40", "90B50", "90B80", "90B85", "90C05", "90C10", "90C22", "90C29", "90C33",
		"90C40", "90C46", "90C51", "90C52", "90C56", "90C70", "91A06", "91A20", "91A25", "91A40",
		"91A43", "91A60", "91A65", "91B02", "91B12", "91B18", "91B28", "91B40", "91B51", "91B52",
		"91B60", "91B62", "91B68", "91B74", "91B84", "91D30", "91E40", "91G20", "91G30", "91G60",
		"91G70", "92B05", "92C15", "92C35", "92C45", "92C80", "92D25", "92D30", "92D99", "92E10",
		"93A10", "93A30", "93B10", "93B17", "93B28", "93B40", "93B50", "93B55", "93B60", "93C15",
		"93C30", "93C42", "93C70", "93C85", "93D15", "93D20", "93D30", "93D99", "93E11", "93E24",
		"94A08", "94A14", "94A24", "94A55", "94A60", "94B15", "94B25", "94B60", "94C12"
	)
}







