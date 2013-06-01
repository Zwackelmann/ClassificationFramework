package script.formulas

import scala.collection.mutable
import parser.ArffJsonInstancesSource
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import format.arff_json.ArffJsonInstance
import common.Path

object ReadDocuments {
    class DocReader(file: File) extends Iterable[Pair[String, Map[Int, Int]]] {
        def iterator = new Iterator[Pair[String, Map[Int, Int]]] {
            val fis = new BufferedInputStream(new FileInputStream(file))
            var buffer: Pair[String, Map[Int, Int]] = null
            
            var state = 0
            val b = new StringBuilder()
            var an: String = null
            var key: Int = 0
            var value: Int = 0
            val dataMap = new mutable.HashMap[Int, Int]
            
            var i: Int = 0
            while(!(state == 1 || state == -1) && {i = fis.read(); i != -1}) {
                changeState(i.asInstanceOf[Char])
                if(state == -1) throw new RuntimeException()
            }
            
            def changeState(c: Char) = {
                state match {
                    case 0 => c match {
                        case '{' => state = 1
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 1 => c match {
                        case '"' => state = 2
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 2 => c match {
                        case c if c != '"' =>
                            b.append(c)
                        case '"' =>
                            an = b.toString
                            b.clear
                            state = 3
                        case _ => state = -1
                    }
                    case 3 => c match {
                        case ':' => state = 4
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 4 => c match {
                        case '{' => state = 5
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 5 => c match {
                        case d if d.isDigit => 
                            b.append(d)
                            state = 6
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 6 => c match {
                        case d if d.isDigit =>
                            b.append(d)
                        case w if w.isWhitespace => 
                            key = b.toString.toInt
                            b.clear
                            state = 7
                        case ':' =>
                            key = b.toString.toInt
                            b.clear
                            state = 8
                        case _ => state = -1
                    }
                    case 7 => c match {
                        case ':' => state = 8
                        case w if w.isWhitespace => 
                        case _ => state = -1
                    }
                    case 8 => c match {
                        case d if d.isDigit =>
                            b.append(d)
                            state = 9
                        case w if w.isWhitespace => 
                        case _ => state = -1
                    }
                    case 9 => c match {
                        case d if d.isDigit => 
                            b.append(d)
                        case w if w.isWhitespace =>
                            value = b.toString.toInt
                            dataMap(key) = value
                            b.clear
                            state = 10
                        case ',' =>
                            value = b.toString.toInt
                            dataMap(key) = value
                            b.clear
                            state = 5
                        case '}' =>
                            value = b.toString.toInt
                            dataMap(key) = value
                            b.clear
                            
                            buffer = (an, dataMap.toMap)
                            
                            dataMap.clear
                            state = 11
                        case _ => state = -1
                    }
                    case 10 => c match {
                        case w if w.isWhitespace =>
                        case ',' => state = 5
                        case '}' => state = 11
                        case _ => state = -1
                    }
                    case 11 => c match {
                        case ',' => state = 1
                        case '}' => 
                            state = -2
                            buffer = null
                        case w if w.isWhitespace => 
                        case _ => state = -1
                    }
                    case -1 => throw new RuntimeException("fooobaaaarbaz")
                    case -2 => c match {
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                }
            }
            
            def bufferNext() = {
                if(state == 11) {
                    i = fis.read()
                    if(i != -1) {
                        changeState(i.asInstanceOf[Char])
                        while(!(state == 11 || state == -1 || state == -2) && {i = fis.read(); i != -1}) {
                            changeState(i.asInstanceOf[Char])
                            if(state == -1) throw new RuntimeException()
                        }
                    } else {
                        buffer = null
                    }
                } else {
                    while(!(state == 11 || state == -1 || state == -2) && {i = fis.read(); i != -1}) {
                        changeState(i.asInstanceOf[Char])
                        if(state == -1) throw new RuntimeException()
                    }
                }
                
                buffer
            }
            
            def hasNext = !(buffer == null && bufferNext() == null)
            
            def next() = if(hasNext) {
                val b = buffer
                buffer = null
                b
            } else {
                fis.close
                null
            }
        }
    }
    
    class MappingReader(file: File) extends Iterable[Pair[String, List[String]]] {
        def iterator = new Iterator[Pair[String, List[String]]] {
            val it = Source.fromFile(file).getLines.flatMap(line => {
                val arr = line.split(" ")
                if(arr.length < 3) {
                    List()
                } else {
                    val id = arr(1)
                    val classes = arr.slice(2, arr.length).toList
                    List(Pair(id, classes))
                }
            })
            
            def hasNext = it.hasNext
            def next = it.next
        }
    }
    
    def main(args: Array[String]) {
        val docReader = new DocReader(new File("C:/Users/Simon/Desktop/document_matrix.txt"))
        val mappingReader = new MappingReader(new File("C:/Users/Simon/Desktop/files.txt"))
        val bestAttributes = ReadMSCOnIndexNodes.bestAttributes(50000).toSet
        
        val attributes = new mutable.HashSet[Int]
        for(doc <- docReader) {
            attributes ++= doc._2.keys.filter(i => bestAttributes.contains(i))
        }
        val indexMap = attributes.toList.sortBy(i => i).zipWithIndex.toMap
        
        val categoriesForDocument = mappingReader.toMap
        
        val numAttributes = attributes.size
        println("numAttributes: " + numAttributes)
        
        val outFile = new BufferedWriter(new FileWriter(new File("cikm-corpus.json")))
        
        outFile.write("{ \"relation-name\" : \"math-formulas\", \"num-attributes\" : " + numAttributes + "}\n")
        
        for(doc <- docReader) {
            val categories = categoriesForDocument.get(doc._1)
            
            if(categories.isDefined) {
                outFile.write("[[\"" + 
                    doc._1 + 
                    "\",[" + 
                        categories.get.map(c => "\"" + c + "-xx\"").mkString(",") + 
                    "]],{" + 
                        doc._2
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














