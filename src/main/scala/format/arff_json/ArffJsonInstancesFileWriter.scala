package format.arff_json
import parser.ContentDescription
import java.io.BufferedWriter
import java.io.FileWriter
import common.FileManager
import FileManager.Protocol._

class ArffJsonInstancesFileWriter(val header: ArffJsonHeader, val contentDescription: ContentDescription) {
    val fileHandle = (FileManager !? CreateFile(contentDescription.fullFilename)) match {
        case AcceptCreateFile(fileHandle) => fileHandle
    }
    
    val writer = new BufferedWriter(new FileWriter(fileHandle.file))
    
    writer.write(header.toJson + "\n")
    
    var numInstances = 0
    
    def +=(inst: ArffJsonInstance) {
        writer.write(inst.toJson + "\n")
        numInstances += 1
    }
    
    def close {
        writer.close()
        fileHandle.close
    }
}