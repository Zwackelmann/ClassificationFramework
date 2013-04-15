package format.arff_json
import parser.ContentDescription
import java.io.BufferedWriter
import java.io.FileWriter
import common.FileManager
import FileManager.Protocol._

class ArffJsonInstancesFileWriter(val header: ArffJsonHeader, val contentDescription: ContentDescription) {
    val writer = (FileManager !? WriteFile(contentDescription.fullFilename)) match {
        case AcceptWriteFile(writer) => writer
    }
    
    writer.write(header.toJson + "\n")
    
    var numInstances = 0
    
    def +=(inst: ArffJsonInstance) {
        writer.write(inst.toJson + "\n")
        numInstances += 1
    }
    
    def close {
        writer.close()
    }
}