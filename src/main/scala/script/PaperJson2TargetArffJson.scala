package script
import java.io.File
import conversion.Json2ArffJson
import model.Paper
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.StringArffJsonAttribute
import format.arff_json.StringArffJsonAttribute
import model.DetailledSource

object PaperJson2TargetArffJson {
    def main(args: Array[String]) {
        
        val parser = new PaperJson2TargetArffJson(new File("data/common/raw/test.json"), new File("data/arffJson/final-test.json"))
        parser.run
        
        val parser2 = new PaperJson2TargetArffJson(new File("data/common/raw/train.json"), new File("data/arffJson/final-train.json"))
        parser2.run
    }
}

class PaperJson2TargetArffJson(inFile: File, outFile: File) extends Json2ArffJson(inFile, outFile) {
    def paperToArffJsonInstance(paper: Paper) = {
        new DenseArffJsonInstance(
            dataList = List(
                paper.title, 
                paper.abstractText, 
                paper.sources.flatMap(_ match {
                    case d: DetailledSource => List(d.journal)
                    case s => List()
                }),
                paper.terms
            ),
            id = paper.an._1 + "." + paper.an._2,
            mscClasses = paper.mscClasses
        )
    }
    
    def header = new ArffJsonHeader(
        relationName = "final_format",
        attributes = List(
            new StringArffJsonAttribute("title"), 
            new StringArffJsonAttribute("abstract"), 
            new StringArffJsonAttribute("journals"),
            new StringArffJsonAttribute("terms")
        ),
        metaAttributes = List(new StringArffJsonAttribute("an"))
    )
}