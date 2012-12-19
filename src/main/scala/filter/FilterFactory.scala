package filter
import parser.ArffJsonInstancesSource

@serializable
trait FilterFactory {
    def apply(trainBase: ArffJsonInstancesSource): Filter
    val historyAppendix: String
}