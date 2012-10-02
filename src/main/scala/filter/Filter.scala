package filter

import parser.ArffJsonInstancesSource
import java.io.File
import parser.ContentDescription
import common.Path.filterPath
import classifier.TargetClassDefinition
import format.arff_json.HistoryItem

object Filter {
    val serializeFilters = false
}

trait Filter {
    def applyFilter(source: ArffJsonInstancesSource, targetClassDefinition: TargetClassDefinition): ArffJsonInstancesSource
    
    def save(outFile: File) {
        common.ObjectToFile.writeObjectToFile(this, outFile)
    }
    
    val historyAppendix: HistoryItem
}
