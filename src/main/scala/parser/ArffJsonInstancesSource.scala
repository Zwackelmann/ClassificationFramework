package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import filter.Filter
import common.Path.arffJsonPath
import filter.GlobalFilter
import filter.FilterFactory
import format.arff_json.HistoryItem

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
    
    def map(elemFun: Iterator[ArffJsonInstance] => Iterator[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: HistoryItem) = new ArffJsonInstancesMapping(
        this,
        elemFun,
        headerFun,
        historyAppendix
    )
    
    def map(elemFun: Iterator[ArffJsonInstance] => Iterator[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, _historyAppendix: String): ArffJsonInstancesMapping = map(
        elemFun,
        headerFun,
        new HistoryItem {val historyAppendix = _historyAppendix}
    )
    
    def project(ids: List[Int], historyAppendix: HistoryItem) = map(
        (arffJsonInstances: Iterator[ArffJsonInstance]) => arffJsonInstances.map(inst => inst.project(ids)),
        (header: ArffJsonHeader) => new ArffJsonHeader(
            header.relationName, 
            ids.map(id => header.attributes(id)),
            header.metaAttributes
        ),
        historyAppendix
    )
    
    def project(ids: List[Int], _historyAppendix: String): ArffJsonInstancesSource = project(
        ids, 
        new HistoryItem() {val historyAppendix = _historyAppendix}
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












