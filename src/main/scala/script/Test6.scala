package script

import common.Common.FileConversion._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object Test6 {
    def main(args: Array[String]) {
        val nonEmptyLines = new File("data/ngrams/13-XX").lines.filter(line => line.filter(c => !c.isWhitespace) != "")
        val writer = new BufferedWriter(new FileWriter(new File("data/ngrams/13-XX2")))
        writer.write(nonEmptyLines.map(line => line.split("\\s+").drop(1).mkString(" ")).mkString("\n"))
        writer.close()
    }
}



















