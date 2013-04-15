package script

import scala.collection.mutable
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import common.Common.FileConversion._
import parser.ArffJsonInstancesSource

object FindLabelEvaluation {
	def main(args: Array[String]) {
	    val corpus = ArffJsonInstancesSource("data/arffJson/min100-test.json")
	    val trueLabels = corpus.map(inst => inst.id -> inst.categories.filter(c => c.substring(2, 3) != "-" && c.substring(3, 5) != "xx")).toMap
	    
	    val dist = {
	        val map = new scala.collection.mutable.HashMap[String, Int] {
	            override def default(key: String) = 0
	        }
	        
	        for(inst <- corpus; cat <- inst.categories.map(c => c.substring(0, 2)).distinct) {
	            map(cat) += 1
	        }
	        
	        map.toMap
	    }
	    
	    println("")
	    
	    // val calcLabels = {
	        
	        val labelFolder = new File("data/labels")
	        val percentageGivenLabels = for(file <- labelFolder.listFiles()) yield {
	            val category = file.getName().substring(0, 2)
	            val calcLabels = file.lines.map(l => Label.fromLine(l)).toList.groupBy(_.id).toMap
			    val foo = for(t1 <- 0.5 to 1.0 by 0.1; t2 <- 0.5 to 1.0 by 0.1; t3 <- 0.5 to 1.0 by 0.1) yield {
			    	(t1, t2, t3, evaluate(calcLabels, trueLabels, (label => label.certainty(0) > t1 && label.certainty(1) > t2 && label.certainty(2) > t3), dist(category)))
			    }
			    println("category: " + category)
			    val bar = foo.filter(x => x._4._1 > 0.8 && !foo.exists(y => y._4._1 > x._4._1 && y._4._2 > x._4._2 && y._4._3 > x._4._3)).toList.filter(x => !x._4._1.isNaN).sortWith((a, b) => a._4._3 > b._4._3).take(5)
				println(bar.mkString("\n"))
			    println("\n\n\n")
				if(bar.isEmpty) 0.0 else bar.head._4._3
	        }
	        
			println(percentageGivenLabels.sum / percentageGivenLabels.size)
			println(percentageGivenLabels.sortWith((a, b) => a > b).take(10).mkString("\n"))
	    // }
	    
	    
	}
	
	def evaluate(calcLabels: Map[String, List[Label]], trueLabels: Map[String, List[String]], filterFun: Label => Boolean, totalInstances: Int) = {
	    val filteredCalcLabels = calcLabels.map(inst_labels => 
			inst_labels._1 -> {
				val labelList = inst_labels._2.filter(filterFun).map(label => (
					label, 
					label.certainty.sum / label.certainty.size
				))
				if(labelList.isEmpty) List() else labelList.sortWith((a, b) => a._2 > b._2).map(_._1.cat).take(1)
			}
		).filter(l => l._2.size >= 1)
		
		// .map(l => l._1 -> l._2.filter(filterFun).map(l => l.cat))
	    
	    val evalData = for((id, labels) <- filteredCalcLabels) yield {
	        val c = trueLabels(id)
	        
	        // println(labels + " <<>> " + c)
	        // tp fp fn #labels
	        ((c intersect labels).size, (labels diff c).size, (c diff labels).size, labels.size)
	    }
	    
		val totalTP = evalData.map(_._1).sum
		val totalFP = evalData.map(_._2).sum
		val totalFN = evalData.map(_._3).sum
		
	    val precision = totalTP.toDouble / (totalTP + totalFP) /*{
	        // tp / (tp + fp)
	        val x = evalData.map(x => x._1.toDouble / (x._1 + x._2))
	        x.sum / x.size
	    }*/
	    
	    val recall = totalTP.toDouble / (totalTP + totalFN) /*{
	        val x = evalData.map(x => if(x._1 + x._3 == 0) 0 else x._1.toDouble / (x._1 + x._3))
	        x.sum / x.size
	    }*/
	    
	    (precision, recall, filteredCalcLabels.map(_._1).size.toDouble / totalInstances)
	}
	
	object Label {
	    def fromLine(line: String) = {
	        val re = """\((.+),(.+),\((.+),(.+),(.+)\)\)""".r
	        line match {
	            case re(id, cat, topCer, middleCer, leaveCer) => Label(id, cat, List(topCer.toDouble, middleCer.toDouble, leaveCer.toDouble))
	            case _ => throw new RuntimeException("No match: " + line)
	        }
	    }
	}
	case class Label(val id: String, val cat: String, certainty: List[Double])
}























