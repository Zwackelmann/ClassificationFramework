package script

import scala.io.Source
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable
import format.arff_json.ArffJsonInstance
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import common.FileManager

object ParseZblFullCorpus {
    def main(args: Array[String]) = try {
        val reader = new BufferedReader(new FileReader(new File("C:/Users/Simon/Projekte/research/ZBL Full Corpus/zbl_full_dmo.txt")))
        
        val lineRe = """^:([^:]+):\t(.*)$""".r
        
        val documentBuffer = new mutable.ListBuffer[ArffJsonInstance]
        
        var count = 0
        var documentDataMap: mutable.HashMap[String, String] = new mutable.HashMap[String, String]
        var line: String = null
        var lastKey: String = null
        while({ line = reader.readLine(); line != null }) {
            if(line == "::::") {
                // transform documentDataMap into a document
                // 
                if(documentDataMap.contains("id") && 
                        documentDataMap.contains("cc") && 
                        !documentDataMap("cc").split(" ").toList.isEmpty && 
                        documentDataMap.contains("ti") && 
                        documentDataMap.contains("ab/en") &&
                        documentDataMap.contains("py") &&
                        (documentDataMap("py") == "2013" || documentDataMap("py") == "2012" || documentDataMap("py") == "2011" || documentDataMap("py") == "2010") && 
                        documentDataMap.contains("dt")) {
                    val doc = ArffJsonInstance(
                            documentDataMap("id"), 
                            documentDataMap("cc").split(" ").toList.flatMap(c => if(c.length == 5) List(c) else if(c.length == 3) List(c + "xx") else if(c.length == 2) List(c + "-xx") else List()), 
                            List(documentDataMap("ti"), documentDataMap("ab/en"), documentDataMap("py").toInt, documentDataMap("dt")), 
                            false
                    )
                    documentBuffer += doc
                    count += 1
                    if(count % 100 == 0) println(count + " documents added")
                    // println("added " + doc)
                } else {
                    // println("skipped document")
                }
                
                documentDataMap = new mutable.HashMap[String, String]
                lastKey = null
            } else {
                line match {
                    case lineRe(key, value) => 
                        documentDataMap(key) = value
                        lastKey = key
                    case _ if lastKey != null && lastKey == "ab/en" =>
                        documentDataMap(lastKey) = documentDataMap(lastKey) + " " + line
                    case _ => 
                        // println("no match")
                }
            }
        }
        
        println(documentBuffer.size + " documents read")
        ArffJsonInstancesSource(
            documentBuffer,
            ArffJsonHeader(4)
        ).save("corpus_2010-2013_ti+aben+py+dt.json")
    } finally {
        FileManager.quit
    }
}











