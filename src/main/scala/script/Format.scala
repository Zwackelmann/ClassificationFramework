package script
import java.io.Reader
import java.util.Scanner

object Format {
    def main(args: Array[String]) {
        var str = """
minus:     (0.7864077669902912,0.3029925187032419,0.4374437443744374)
not minus: (0.7906976744186046,0.2967581047381546,0.4315503173164098)

TopClassIs(35): 
minus:     (0.8092972181551976,0.5295808383233533,0.6402200665991024)
not minus: (0.801258327165063,0.518562874251497,0.6296350152682856)
"""
            
        format(str)
    }
    
    val targetRe = """(.*)\(.*([0-9]+\.[0-9]*).*,.*([0-9]+\.[0-9]*).*,.*([0-9]+\.[0-9]*).*\)(.*)""".r
    
    def format(str: String) {
        format(new Scanner(str))
    }
    
    def format(sc: Scanner) {
        while(sc.hasNext()) {
            val line = sc.nextLine()
            
            line match {
                case targetRe(before, prec, rec, f, after) => println(before + "(" + formatDouble(prec) + ", " + formatDouble(rec) + ", " + formatDouble(f) + ")" + after)
                case _ => println(line)
            }
        }
    }
    
    def formatDouble(str: String) = ("%.4f".format(str.toDouble)).replaceAllLiterally(",", ".")
}