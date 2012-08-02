package script

import format.arff_json._
import java.io.File
import model.Paper
import conversion.Json2ArffJson

object ApplyJson2ArffJson {
    def main(args: Array[String]) {
        new Json2ArffJson(
            inFile = new File("data/common/raw/train.json"), 
            outFile = new File("data/common/abstract_only/raw/train.json")
        ) {
            def paperToArffJsonInstance(paper: Paper) = {
                new DenseArffJsonInstance(
                    dataList = List(paper.abstractText),
                    id = paper.an._1 + "." + paper.an._2,
                    mscClasses = paper.mscClasses
                )
            }
            
            def header = new ArffJsonHeader(
                relationName = "abstract_only",
                attributes = List(new StringArffJsonAttribute("abstract")),
                metaAttributes = List(new StringArffJsonAttribute("an"))
            )
            
        }.run
    }
}