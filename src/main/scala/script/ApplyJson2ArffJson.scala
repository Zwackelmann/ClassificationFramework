package script

import format.arff_json._
import java.io.File
import model.Paper
import conversion.Json2ArffJson
import format.arff_json.ArffJsonInstance

object ApplyJson2ArffJson {
    def main(args: Array[String]) {
        new Json2ArffJson(
            inFile = new File("data/common/raw/train.json"), 
            outFile = new File("data/common/abstract_only/raw/train.json")
        ) {
            def paperToArffJsonInstance(paper: Paper) = {
                ArffJsonInstance(
                    paper.an._1 + "." + paper.an._2,
                    paper.mscClasses,
                    List(paper.abstractText)
                )
            }
            
            def header = ArffJsonHeader(
                "abstract_only",
                List(new StringArffJsonAttribute("abstract"))
            )
            
        }.run
    }
}