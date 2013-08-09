package script

import parser.ArffJsonInstancesSource

object FilterIrrelevantPapers {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource("data_filter_irrelevant_papers/arffJson/corpus.json")
        
        corpus.filterNot(doc => doc.categories.exists(cat => cat.charAt(2) == '-' || cat.substring(3, 5) == "99")).save("filteredCorpus")
    }
}