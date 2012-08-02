package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import filter.Filter
import common.Path.arffJsonPath
import filter.GlobalFilter

object ArffJsonInstancesSource {
    def apply(_source: Iterable[ArffJsonInstance], _header: ArffJsonHeader, _contentDescription: ContentDescription) = {
        new ArffJsonInstancesSource {
            val header = _header
            val iterator = _source.iterator
            val contentDescription = _contentDescription
        }
    }
}

trait ArffJsonInstancesSource extends Iterable[ArffJsonInstance] {
    def header: ArffJsonHeader
    def iterator: Iterator[ArffJsonInstance]
    def applyFilter(filter: GlobalFilter) = {
        filter.applyFilter(this)
    }
    
    def contentDescription: ContentDescription
    
    def numAttributes = header.attributes.size
    def numInstances = iterator.size
    
    def map(elemFun: Iterator[ArffJsonInstance] => Iterator[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: String) = new ArffJsonInstancesMapping(
        this,
        elemFun,
        headerFun,
        historyAppendix
    )
    
    def project(ids: List[Int], historyAppendix: String) = map(
        (arffJsonInstances) => arffJsonInstances.map(inst => inst.project(ids)),
        (header) => new ArffJsonHeader(
            header.relationName, 
            ids.map(id => header.attributes(id)),
            header.metaAttributes
        ),
        historyAppendix
    )
    
    def save(file: File) {
        this match {
            case f: ArffJsonInstancesFile => // skip (allready saved)
            case _ => {
                val writer = new BufferedWriter(new FileWriter(file))
                writer.write(header.toJson + "\n")
            
                for(inst <- iterator) {
                    writer.write(inst.toJson + "\n")
                }
                writer.close()
            }
        } 
    }
    
    def save(): Unit = save(file)
    def saved = file.exists()
    
    def file: File = contentDescription.file
    
    def as(contentDescription: ContentDescription, mappings: (ContentDescription => ArffJsonInstancesSource)) = mappings(contentDescription)
}












