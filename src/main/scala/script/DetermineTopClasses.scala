package script
import parser.PaperJsonFile
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object DetermineTopClasses {
    def main(args: Array[String]) {
        val x = (for(paper <- new PaperJsonFile("data/json/deliver-math-raw.json").elements) yield paper.mscClasses.map(_.substring(0, 2))).flatten.toSet.toList
        val sorted = x.sortBy(s => s)
        
        val writer = new BufferedWriter(new FileWriter(new File("data/util/top_classes.txt")))
        for(c <- sorted) {
            writer.write(c + "\n")
        }
        writer.close
    }
}