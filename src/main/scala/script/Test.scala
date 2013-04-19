package script

import common.Path
import parser.ArffJsonInstancesSource
import common.FileManager

object Test {
    def main(args: Array[String]) {
        val inst = ArffJsonInstancesSource("data_fiz_full/arffJson/corpus.json") 
        println(inst.size)
        FileManager.quit
    }
}




















