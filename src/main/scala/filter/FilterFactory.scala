package filter
import parser.ArffJsonInstancesSource
import format.arff_json.HistoryItem

trait FilterFactory extends HistoryItem {
    def apply(trainBase: ArffJsonInstancesSource): Filter
}