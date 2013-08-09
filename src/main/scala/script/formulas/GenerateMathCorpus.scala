package script.formulas

import script.formulas.ReadDocuments.MappingReader
import script.formulas.ReadDocuments.DocReader
import java.io.File
import scala.collection.mutable
import java.io.BufferedWriter
import java.io.FileWriter
import formulas.index.Index
import java.io.BufferedReader
import java.io.FileReader

object GenerateMathCorpus {
    def main(args: Array[String]) {
        val docReader = new DocReader(new File("C:/Users/Simon/Projekte/research/Kohlhase Korpora/document_matrix.txt"))
        println("read documents")
        val categoriesForDocument = new MappingReader(new File("C:/Users/Simon/Projekte/research/Kohlhase Korpora/files.txt")).toMap
        println("read categories")
        val index = Index(new BufferedReader(new FileReader(new File("C:/Users/Simon/Projekte/research/Kohlhase Korpora/index.txt"))))
        println("read index")
        
        // map erstellen, die die gelesenen indexe auf neue indexe abbildet (falls in den Quelldaten nicht jeder Index gebracht wird,
        // werden die Indexe so "zusammengerueckt")
        val attributes = new mutable.HashSet[Int]
        for(doc <- docReader) {
            attributes ++= doc._2.keys/*.filter(i => bestAttributes.contains(i))*/
        }
        val indexMap = attributes.toList.sortBy(i => i).zipWithIndex.toMap
        val numAttributes = attributes.size
        
        val outFile = new BufferedWriter(new FileWriter(new File("reweighted-math-corpus-0.25.json")))
        outFile.write("{ \"relation-name\" : \"math-formulas\", \"num-attributes\" : " + numAttributes + "}\n")
        
        var count = 0
        for(doc <- docReader) {
            count += 1
            if(count % 100 == 0) println(count)
            val (an, content) = doc
            val categories = categoriesForDocument.get(an)
            
            if(categories.isDefined) {
                outFile.write("[[\"" + 
                    an + 
                    "\",[" + 
                        categories.get.map(c => "\"" + c + "-xx\"").mkString(",") + 
                    "]],{" + 
                        index.reweight(content.map(kv => kv._1 -> kv._2.toDouble), 0.25).filter(kv => kv._2 > 0.001)
                        .toList
                        .sortBy(_._1)
                        .flatMap(kv => 
                            (if(indexMap.contains(kv._1)) List("\"" + indexMap(kv._1) + "\":" + kv._2) else List())
                        )
                        .mkString(",") + 
                    "}]\n")
            }
        }
        
        outFile.close
    }
}