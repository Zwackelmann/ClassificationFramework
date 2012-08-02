package parser

import java.io.File
import model.Paper
import model.Paper._
import net.sf.json.JSONObject
import net.sf.json.JSONSerializer
import java.io.FileReader
import java.io.BufferedReader

class PaperJsonFile(val file: File) {
    def this(filename: String) = this(new File(filename))
    
    def elements = new Iterator[Paper] {
        val reader = new BufferedReader(new FileReader(file))
        var buffer: Paper = null
    
        def bufferNext() = {
            val line = reader.readLine()
            if(line != null) {
                buffer = Paper(JSONSerializer.toJSON(line).asInstanceOf[JSONObject])
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
}








