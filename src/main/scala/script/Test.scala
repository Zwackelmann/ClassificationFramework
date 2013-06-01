package script

import common.Path
import parser.ArffJsonInstancesSource
import common.FileManager
import com.alibaba.fastjson.parser.DefaultJSONParser
import com.alibaba.fastjson.JSONArray
import scala.io.Source

object Test {
    
    def main(args: Array[String]) {
        val input = Source.fromFile("reports_not_only_main_class_not_normalized/no-formulas-not-stemmed").getLines
        input.next
        
        val parsedInput = input.map(line => {
            common.Common.jsonToScalaType(new DefaultJSONParser(line).parse())
        }).toList.asInstanceOf[List[Map[String, Any]]]
        
        println(parsedInput.sortBy(m => m("topClass").asInstanceOf[String]).map(item => 
            item("topClass") + "\t" + 
            item("bestF") + "\t\t\t" + 
            item("precRecGraphPoints").asInstanceOf[List[Double]].mkString("\t")
        ).mkString("\n"))
    }
}




















