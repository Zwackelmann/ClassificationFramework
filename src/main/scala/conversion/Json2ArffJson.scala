package conversion

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import model.Paper
import format.arff_json._
import parser.PaperJsonFile

abstract class Json2ArffJson(val inFile: File, val outFile: File) {
    def paperToArffJsonInstance(paper: Paper): ArffJsonInstance
    def header: ArffJsonHeader
    
    def run() {
        val paperfile = new PaperJsonFile(inFile)
        
        val jsonArffHeader = header
        val out = new BufferedWriter(new FileWriter(outFile))
        
        out.write(header.toJson + "\n")
        
        for(paper <- paperfile.elements) {
            out.write(paperToArffJsonInstance(paper).toJson + "\n")
        }
        
        out.close()
    }
}





