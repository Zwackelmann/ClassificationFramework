package conversion
import model.Paper
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import model.DetailledSource
import parser.LangDetector
import parser.PaperJsonFile

object PaperJson2TestTrainSet {
    def main(args: Array[String]) {
        // TODO use the submitted assertions within the class!!
        // last time i tried it i had some strange performance penalties so i hard coded them...
        val paperJson2TestTrainSet = new PaperJson2TestTrainSet(
            0.7, 
            List(
                (paper) => paper.abstractText != "",
                (paper) => paper.title != "",
                (paper) => paper.sources.forall(source => source.isInstanceOf[DetailledSource]),
                (paper) => LangDetector.detect(paper.abstractText) == "en"
            )
        ).convert(
            new File("data/json/deliver-math-raw.json"),
            new File("data/common/raw/test.json"),
            new File("data/common/raw/train.json")
        )
    }
}

class PaperJson2TestTrainSet(
    val trainPortion: Double,
    val assertions: List[Paper => Boolean]) {
    
    def convert(inFile: File, testFile: File, trainFile: File) {
        val paperfile = new PaperJsonFile(inFile)
        
        val trainsetWriter = new BufferedWriter(new FileWriter(trainFile))
        val testsetWriter = new BufferedWriter(new FileWriter(testFile))
        
        var writtenTrainInstances = 0
        var writtenTestInstances = 0
        var droppedPapers = 0
        
        for(paper <- paperfile.elements) {
            if(paper.abstractText != "" &&
                paper.title != "" && 
                // paper.sources.forall(source => source.isInstanceOf[DetailledSource]) &&
                LangDetector.detect(paper.abstractText) == "en"
            ) {
                if(writtenTrainInstances == 0 || (writtenTrainInstances.toDouble / (writtenTrainInstances + writtenTestInstances)) < trainPortion) {
                    trainsetWriter.write(paper.toJson + "\n")
                    writtenTrainInstances = writtenTrainInstances + 1
                } else {
                    testsetWriter.write(paper.toJson + "\n")
                    writtenTestInstances = writtenTestInstances + 1
                }
                
                if((writtenTrainInstances + writtenTestInstances) % 1000 == 0) {
                    println("writtenPapers: " + (writtenTrainInstances + writtenTestInstances))
                }
            } else {
                droppedPapers = droppedPapers + 1
                if(droppedPapers % 1000 == 0) {
                    println("droppedPapers: " + droppedPapers)
                }
            }
        }
        
        trainsetWriter.close
        testsetWriter.close
    }
}
















