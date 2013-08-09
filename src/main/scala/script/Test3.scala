package script

import parser.ArffJsonInstancesSource

object Test3 {
    def main(args: Array[String]) {
        val corpus = ArffJsonInstancesSource("C:/Users/Simon/Projekte/javascala/workspace/ClassificationFramework/data_evaluate_if_choosing_only_the_main_class_is_better/arffJson/corpus.json")
        
        val x = corpus.map(_.categories.map(x => x.substring(0, 2)).distinct.size)
        
        println(x.count(_ > 3).toDouble / x.size)
    }
}

