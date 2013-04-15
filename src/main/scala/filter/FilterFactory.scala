package filter
import parser.ArffJsonInstancesSource
import parser.ContentDescription
import common.Path
import common.FileManager
import FileManager.Protocol._
import java.io.File

@serializable
trait FilterFactory {
    def apply(trainBase: ArffJsonInstancesSource): Filter
    val historyAppendix: String
}

trait Loadable[T <: Filter] extends FilterFactory {
    @deprecated
    def load(file: File) = if(file.exists()) {
        try {
            Some(common.ObjectToFile.readObjectFromFile(file).asInstanceOf[T])
        } catch {
            case _: Throwable => None
        }
    } else {
        None
    } 
    
    def load(fullFilename: String) = (FileManager !? ReceiveFile(fullFilename, true)) match {
        case AcceptReceiveFile(file) => try {
            Some(common.ObjectToFile.readObjectFromFile(file).asInstanceOf[T])
        } catch {
            case _: Throwable => None
        }
        case FileNotExists => None
        case Error(msg) => throw new RuntimeException(msg)
    }
    
    def load(trainBaseCd: ContentDescription): Option[T] = load(common.Path.filterPath / Filter.filename(trainBaseCd, historyAppendix))
    
    def save(filter: Any, trainBaseCd: ContentDescription) {
        val filterFile = Filter.fullFilename(trainBaseCd, this.historyAppendix)
        save(filter, filterFile)
    }
    
    def save(filter: Any, fullFilename: String) {
        assert(filter.isInstanceOf[Filter])
        
        (FileManager !? CreateFile(fullFilename, true, (file => {
            common.ObjectToFile.writeObjectToFile(filter, file)
        }))) match {
            case Success =>
            case Failed => throw new RuntimeException("saving of filter " + fullFilename + " failed")
            case Error(msg) => throw new RuntimeException(msg)
        }
    }
}






