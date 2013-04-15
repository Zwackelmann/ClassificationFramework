package script

import java.security.MessageDigest
import java.io.File
import Stream._
import java.io.FileNotFoundException
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import common.FileManager

object ParseAndEscapeCorpora {
    def main(args: Array[String]) {
        // {"relation-name" : "final_format", "attributes" : [{"name" : "title", "type" : "string"}, {"name" : "abstract", "type" : "string"}]}
        val lines = Source.fromFile("C:\\Users\\Simon\\Projekte\\javascala\\workspace\\ClassificationFramework\\data\\arffJson\\corpusTIB-test.json")("UTF-8").getLines
        val re = """^\[\[(".*"),\[(".*"(,)?)*\]\], \["(.*)", "(.*)", \[(".*"(,)?)*\], \[(".*"(,)?)*\]\]\]$""".r
        
        val header = lines.next
        
        val writer = new BufferedWriter(new FileWriter(new File("C:\\Users\\Simon\\Projekte\\javascala\\workspace\\ClassificationFramework\\data\\arffJson\\corpus-tib2.json")))
        
        writer.write(header)
        writer.write("\n")
        
        val l = """[["CN0209"53253" , ["LINSEARCH:inf","LINSEARCH:mat"]], ["Using a Generic Approach to Support the Construction of Methods", "null", ["Springer"], ["DEXA","expert systems applications","database applications"]]]"""
        println(l)
        readDocument(l)
        
        /*var count = 0
        for(line <- lines) {
            count += 1
            if(count % 1000 == 0) println(count)
            if(count > 1141000)
                println(line)
            line match {
                case re(id, cl, _, tit, abs, jour, _, terms, _) => 
                    if(!filterList(terms).isDefined || !filterList(jour).isDefined) {
                        println("undefined list: " + jour + " " + terms)
                    } else {
                        try {
                            writer.write("""[[%s, %s],["%s","%s",%s,%s]]%n""".format(
                                id, 
                                filterClasses(cl), 
                                escape(tit), 
                                escape(abs), 
                                filterList(jour), 
                                filterList(terms)
                            ))
                        } catch {
                            case _: Throwable => println(line)
                        }
                    }
                case _ => println(line)
            }
        }
        
        FileManager.quit
        println("finished")
        writer.close()*/
    }
    
    def readDocument(line: String) = {
        var pos = 0
        while(line.charAt(pos) == ' ') pos += 1
        
        assert(line.charAt(pos) == '[')
        pos += 1
        while(line.charAt(pos) == ' ') pos += 1
        
        assert(line.charAt(pos) == '[')
        pos += 1
        while(line.charAt(pos) == ' ') pos += 1
        
        val id = {
            assert(line.charAt(pos) == '"')
            pos += 1
            val startPos = pos
            
            val endPos = {
                while(line.charAt(pos) != '"') pos += 1
                
                while(!{
                    var pos2 = pos + 1
                    while(line.charAt(pos2) == ' ' && pos2 < line.length) pos2 += 1
                    if(line.charAt(pos2) != ',' && pos2 < line.length) false
                    else {
                        pos2 += 1
                        while(line.charAt(pos2) == ' ' && pos2 < line.length) pos2 += 1
                        line.charAt(pos2) == '['
                    }
                }) {
                    pos += 1
                    while(line.charAt(pos) != '"' && pos < line.length) pos += 1
                }
                
                pos
            }
            
            line.substring(startPos, endPos)
        }
        
        pos += 1
        while(line.charAt(pos) == ' ' && pos < line.length) pos += 1
        assert(line.charAt(pos) == ',')
        pos += 1
        while(line.charAt(pos) == ' ' && pos < line.length) pos += 1
        assert(line.charAt(pos) == '[')
        pos += 1
        while(line.charAt(pos) == ' ' && pos < line.length) pos += 1
        
        println(line.charAt(pos))
        
        println(id.replaceAllLiterally("\"", "\\\""))
    }
    
    def test(line: String, pos: Int)(fun: (String, Int) => Boolean) = {
        
    }
    
    def filterClasses(classes: String) = {
        "[" + classes.split(",\\s*").map(s => s.substring(1, s.length()-1)).toList.filter(_.length != 0).map(s => "\"" + s + "\"").mkString(",") + "]"
    }
    
    def escape(text: String) = {
        text.replaceAllLiterally("\"", "\\\"")
    }
    
    def filterList(list: String) = {
            if(list == null) {
                Some("[]")
            } else {
                /*if(list.contains("\",") || !list.contains(",")) {
                    Some("[" + list.split(",\\s*").map(s => s.substring(1, s.length()-1).replaceAllLiterally("\"", "\\\"")).toList.filter(_.length != 0).map(s => "\"" + s + "\"").mkString(",") + "]")
                } else if(list.contains(",\"") && list.length >= 2) {
                    val cont = list.substring(1, list.length - 1)
                    val x = Some("[" + cont
                        .split(",\\s*")
                        .toList
                        .map(s => s.filter(c => c != '"' && c != ','))
                        .filter(_.length != 0)
                        .map(s => "\"" + s + "\"")
                        .mkString(",") + "]")
                    x
                } else {*/
                    Some("[" + list.replaceAllLiterally("\"", "\\\"") + "]")
                // }
            }
    }
}













