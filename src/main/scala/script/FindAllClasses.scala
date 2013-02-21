package script
import parser.ArffJsonInstancesSource
import java.io.File

object FindAllClasses {
    def main(args: Array[String]) {
        val inst = ArffJsonInstancesSource(new File("data/arffJson/exp-test.json")) ++ ArffJsonInstancesSource(new File("data/arffJson/exp-train.json"))
        val allClasses = (Set[String]() /: inst.iterator.map(i => i.categories))((old, cats) => old ++ cats)
        // println(allClasses.toList.mkString("\n"))
    }
}