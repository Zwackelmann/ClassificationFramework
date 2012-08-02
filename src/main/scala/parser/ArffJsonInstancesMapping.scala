package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader

class ArffJsonInstancesMapping(src: ArffJsonInstancesSource, instMapping: Iterator[ArffJsonInstance] => Iterator[ArffJsonInstance], headerMapping: ArffJsonHeader => ArffJsonHeader, historyAppendix: String) extends ArffJsonInstancesSource {
    def header = headerMapping(src.header)
    def iterator = instMapping(src.iterator)
    
    val contentDescription = src.contentDescription.addHistoryItem(historyAppendix)
}