package script
import java.io.Reader
import java.util.Scanner
import java.io.File
import scala.collection.mutable

object Format {
    def main(args: Array[String]) {
        format(new Scanner(new File("C:\\Users\\Simon\\Masterarbeit\\experimente\\composed (all cl)")))
    }
    
    val targetRe = """(.*) \(.*([0-9]+\.[0-9]*).*,.*([0-9]+\.[0-9]*).*,.*([0-9]+\.[0-9]*).*\)(.*)""".r
    
    
    // [info] topClass: TopClassIs(00), List(0.8, 0.0, 0.1, 0.1), results: 0.82987012987013
    val otherRe = """.*TopClassIs\(([^\)]+)\).*results: ([0-9]\.[0-9]+)""".r 
    
    def format(str: String) {
        format(new Scanner(str))
    }
    
    def fm(prec: Double, rec: Double) = (2*prec*rec) / (prec+rec)
    
    def format(sc: Scanner) {
        val results = {
            val r = new mutable.ListBuffer[Tuple5[Double, Int, String, Boolean, Double]]
            
            while(sc.hasNext()) {
                val line = sc.nextLine()
                line match {
                    /*case targetRe(before, prec, rec, f, after) => {
                        println(beforeParser(before).get.mkString("\t") + "\t" + formatDouble2(prec) + "\t" + formatDouble2(rec) + "\t" + formatDouble2(fm(prec.toDouble, rec.toDouble).toString))
                        // val bef = beforeParser(before).get
                        // r += (Tuple5(bef(0).toDouble, bef(1).toInt, bef(2), bef(3).toBoolean, f.toDouble))
                    }*/
                    case otherRe(topClass, f) => println(topClass + "\t" + formatDouble2(f))
                    case _ => 
                }
            }
            // r.toList
        }
        
        // println(results.groupBy(_._3).map(c => c._2.sortWith((a, b) => a._5 > b._5).take(3)).mkString("\n"))
    }
    
    // topClass: TopClassIs(00), or: 50.0, results: (0.6923, 1.0000, 0.8182)
    val beforeRe = """.*TopClassIs\(([0-9]+)\).*or: ([^,]+).*""".r
    
    def beforeParser(str: String) = {
        str match {
            case beforeRe(c, x) => Some(List(c, x))
            case _ => None
        }
    }
    
    def formatDouble(str: String) = ("%.4f".format(str.toDouble)).replaceAllLiterally(",", ".")
    def formatDouble2(str: String) = ("%.4f".format(str.toDouble))
}
































