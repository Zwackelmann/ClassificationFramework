package script

import conversion.ArffJson2Joachims
import java.io.File
import parser.ArffJsonInstancesFile
import parser.ContentDescription

object ApplyArffJson2Joachims {
    def main(args: Array[String]) {
        ArffJson2Joachims(
            new ArffJsonInstancesFile("abstract-only", ContentDescription.TestSet, List(/* history */)),
            new File("jotest"),
            (mscClasses) => mscClasses.exists(_.substring(0, 2) == "35")
        )
    }
}