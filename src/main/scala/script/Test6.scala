package script

import common.Common.FileConversion._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.collection.mutable.ListBuffer

object Test6 {
    def main(args: Array[String]) {
        val testString = """R. Stanley\'s book [Enumerative combinatorics 2 (Cambridge Studies in Advanced Mathematics. 62. Cambridge University Press, Cambridge) (1999; Zbl 0928.05001)] and its addendum (http://www-math.mit.edu/~rstan/ec/catadd.pdf) list over 95 collections of objects counted by the Catalan numbers."""
            
        val formulasReplaced = """\$+([^$]+)\$+""".r 
            .replaceAllIn(
                testString, 
                g => {
                    val formula = g.group(1).filter(c => c.isDigit || c.isLetter)
                    if(formula == "") "" else "<formula|" + formula + ">"
                }
            )
            
        val linksReplaced = """\[([^\]]+)\]""".r 
            .replaceAllIn(
                testString, 
                g => {
                    val link = g.group(1).filter(c => c.isDigit || c.isLetter)
                    if(link == "") "" else "<link|" + link + ">"
                }
            )
        
        val x = linksReplaced
            .replaceAll("[.,;:]", " ")
            .replaceAll("\\\\[^\\w]", "") // special characters (like \')
            .replaceAll("\\\\[^\\s]+", "")
            .filter(c => c.isSpaceChar || c.isDigit || c.isLetter || c == '<' || c == '>' || c == '|' || c==']' || c=='[' || c=='(' || c==')')
        println(x)
    }
}



















