package format.arff_json

object HistoryItem {
    def apply(_historyAppendix: String) = new HistoryItem{
        val historyAppendix = _historyAppendix
    }
}

@serializable
trait HistoryItem {
    val historyAppendix: String
}