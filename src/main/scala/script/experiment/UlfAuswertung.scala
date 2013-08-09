package script.experiment

import scala.io.Source

object UlfAuswertung {
    def main(args: Array[String]) {
        def rows = Source.fromFile("ulf-auswertung.txt").getLines.map(line => {
            val arr = line.split(",")
            (
                arr(0), 
                arr(1).split(" ").toList.map(_.substring(0,2)).distinct.map(_.filter(_.isDigit).toInt), 
                arr(2).split(" ").toList.flatMap(s => if(s.length < 2) List() else List(s.substring(0,2))).distinct.map(_.filter(_.isDigit).toInt), 
                List(arr(3).toInt)
            )
        })
        
        println(rows.mkString("\n"))
        
        // println(rows.map(_._2).flatten.toList.groupBy(a => a).map(x => x._1 -> x._2.size).mkString("\n"))
        // 5 -> 136
        // 93 -> 43
        // 3 -> 107
        // 68 -> 51
        
        
        
        def eval(catNr: Int) = {
            
            val tpBS = rows.count(row => {row._2.contains(catNr) && row._3.contains(catNr)})
            val tnBS = rows.count(row => {!row._2.contains(catNr) && !row._3.contains(catNr)})
            val fpBS = rows.count(row => {!row._2.contains(catNr) && row._3.contains(catNr)})
            val fnBS = rows.count(row => {row._2.contains(catNr) && !row._3.contains(catNr)})
            
            val precisionBS = tpBS.toDouble / (tpBS + fpBS)
            val recallBS = tpBS.toDouble / (tpBS + fnBS)
            
            val tpB = rows.count(row => {row._2.contains(catNr) && row._4.contains(catNr)})
            val tnB = rows.count(row => {!row._2.contains(catNr) && !row._4.contains(catNr)})
            val fpB = rows.count(row => {!row._2.contains(catNr) && row._4.contains(catNr)})
            val fnB = rows.count(row => {row._2.contains(catNr) && !row._4.contains(catNr)})
            
            val precisionB = tpB.toDouble / (tpB + fpB)
            val recallB = tpB.toDouble / (tpB + fnB)
            
            println("")
            println(catNr + ":")
            println("precisionBS: " + precisionBS)
            println("recallBS: " + recallBS)
            println("F-BS: " + ((2*precisionBS*recallBS)/(precisionBS+recallBS)))
            println("")
            println("precisionB: " + precisionB)
            println("recallB: " + recallB)
            println("F-B: " + ((2*precisionB*recallB)/(precisionB+recallB)))
        }
        
        eval(3)
        eval(5)
        eval(68)
        eval(93)
    }
}













