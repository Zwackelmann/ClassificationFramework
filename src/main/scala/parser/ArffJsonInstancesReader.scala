package parser
import java.io.BufferedReader
import format.arff_json.ArffJsonHeader
import net.sf.json.JSONSerializer
import net.sf.json.JSONObject
import net.sf.json.JSONException

class ArffJsonInstancesReader(reader: BufferedReader, val contentDescription: ContentDescription) extends ArffJsonInstancesSource {
    var read = false
    
    val header: ArffJsonHeader = try {
        JSONSerializer.toJSON(reader.readLine) match {
            case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
            case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
        }
    } catch {
        case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
        case e => throw e
    }
    
    def iterator = {
        if(!read) {
            read = true
            new ArffJsonInstancesIterator(reader, header)
        } else {
            throw new RuntimeException("elements can only be called once on an ArffJsonInstancesReader")
        }
    }
}











