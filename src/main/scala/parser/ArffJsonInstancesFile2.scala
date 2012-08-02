package parser
import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import format.arff_json.ArffJsonHeader
import net.sf.json.JSONSerializer
import net.sf.json.JSONException
import net.sf.json.JSONObject

class ArffJsonInstancesFile2(_file: File) extends ArffJsonInstancesSource {
    def reader = new BufferedReader(new FileReader(file))
    
    val header: ArffJsonHeader = {
        val r = reader
        
        val h = try {
            JSONSerializer.toJSON(r.readLine) match {
                case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
            }
        } catch {
            case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
            case e => throw e
        }
        r.close()
        h
    }
    
    val contentDescription = null
    
    def iterator = {
        val r = reader
        r.readLine()
        new ArffJsonInstancesIterator(r, header)
    }
    
    override def file = _file
}