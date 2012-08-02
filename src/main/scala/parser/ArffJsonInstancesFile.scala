package parser
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import format.arff_json.ArffJsonHeader
import net.sf.json.JSONSerializer
import net.sf.json.JSONObject
import net.sf.json.JSONException
import common.Path

class ArffJsonInstancesFile(val contentDescription: ContentDescription) extends ArffJsonInstancesSource {
    def this(base: String, set: String, formatHistory: List[String]) = this(ContentDescription(base, set, formatHistory))
    
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
    
    def iterator = {
        val r = reader
        r.readLine()
        new ArffJsonInstancesIterator(r, header)
    }
}





