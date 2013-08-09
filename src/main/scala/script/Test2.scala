package script

import parser.ArffJsonInstancesSource
import com.alibaba.fastjson.parser.DefaultJSONParser
import com.alibaba.fastjson.JSONObject
import scala.io.Source
import common.Path
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object Test2 {
    def main(args: Array[String]) {
        val documents = Source
            .fromFile("interrater-consistency-corpus.json")
            .getLines
            .map(line => new DefaultJSONParser(line).parse.asInstanceOf[JSONObject])
        
        val writer = new BufferedWriter(new FileWriter(new File("play-style-interrater-corpus")))
        writer.write(
            documents.map(doc => 
                "- !!models.Document\nid: %s\ntitle: %s\nabstract: %s".format(
                    doc.get("id"), 
                    common.Common.escape(doc.get("title").toString), 
                    common.Common.escape(doc.get("abstract").toString)
                )
            ).mkString("\n\n")
        )
        writer.close
    }
}








