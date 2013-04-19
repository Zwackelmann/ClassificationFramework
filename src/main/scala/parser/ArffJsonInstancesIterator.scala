package parser
import java.io.BufferedReader
import format.arff_json.ArffJsonHeader
import format.arff_json.ArffJsonInstance
import common.Time
import com.alibaba.fastjson.parser.DefaultJSONParser
import com.alibaba.fastjson.JSONArray

class ArffJsonInstancesIterator(reader: BufferedReader, header: ArffJsonHeader) extends Iterator[ArffJsonInstance] {
    var buffer: ArffJsonInstance = null
    var count = 0
    
    def bufferNext() = {
        val line = reader.readLine()
        if(line != null) {
            count += 1
            buffer = try {
                ArffJsonInstance(line, header)
            } catch {
                case e: Exception => {
                    // println("catch: line: " + count + ", text: " + line)
                    ArffJsonInstance(new DefaultJSONParser(line).parse().asInstanceOf[JSONArray], header)
                }
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







