package parser
import java.io.BufferedReader
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import common.Time

class ArffJsonInstancesIterator(reader: BufferedReader, header: ArffJsonHeader) extends Iterator[ArffJsonInstance] {
    var buffer: ArffJsonInstance = null
    var count = 0
    
    def bufferNext() = {
        val line = reader.readLine()
        if(line != null) {
            count += 1
            
            if(count <= 10) {
                buffer = ArffJsonInstance(line, header, true)
            } else {
                buffer = ArffJsonInstance(line, header, false)
            }
        } else {
            buffer = null
        }
        
        buffer
    }
    
    def hasNext = !(buffer == null && bufferNext() == null)
    
    def next() = if(hasNext) {
        val b = buffer
        buffer = null
        b
    } else {
        reader.close
        null
    }
}







