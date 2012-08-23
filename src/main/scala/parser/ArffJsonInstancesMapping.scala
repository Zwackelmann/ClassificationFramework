package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import filter.FilterFactory
import format.arff_json.HistoryItem

class ArffJsonInstancesMapping(src: ArffJsonInstancesSource, instMapping: Iterator[ArffJsonInstance] => Iterator[ArffJsonInstance], headerMapping: ArffJsonHeader => ArffJsonHeader, historyItem: HistoryItem) extends ArffJsonInstancesSource {
    def header = headerMapping(src.header)
    def iterator = instMapping(src.iterator)
    
    val contentDescription = src.contentDescription.addHistoryItem(historyItem)
}