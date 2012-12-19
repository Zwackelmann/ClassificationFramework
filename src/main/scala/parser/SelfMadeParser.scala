package parser

import java.util.Scanner
import java.io.File
import scala.collection.mutable.ListBuffer
import model._

object SelfMadeParser {
    def main(args: Array[String]) {
        val p = new SelfMadeParser("data2/raw/deliver-math.txt")
        var count = 0
        var error = 0
        while(p.hasNext) {
            try {
                val paper = p.readPaper(p.nextLines)
                count += 1
            } catch {
                case _ => error += 1
            }
        }
        
        println(count)
        println(error)
    }
}

class SelfMadeParser(val filename: String) {
    val scanner = new Scanner(new File(filename))
    var line: String = scanner.nextLine

    while({val att = lineAtt(line); !(att.isDefined)}) {
        line = scanner.nextLine()
    }
    
    def nextLines = {
        val lines = new ListBuffer[String]
        
        while({val att = lineAtt(line); att.isDefined}) {
            lines += line
            line = scanner.nextLine
        }
        
        while({val att = lineAtt(line); (!att.isDefined || att.isDefined && att.get != "AN") && scanner.hasNextLine}) {
            lines += line
            line = scanner.nextLine
        }
        
        lines.toList
    }
    
    def hasNext = scanner.hasNextLine()
    
    def readPaper(lines: List[String]) = {
        def read[T](lines: List[String], whileAtt: String, fun: List[String] => T): Pair[T, List[String]] = {
            def lineCond(line: String) = { val att = lineAtt(line); (!att.isDefined || att.get != whileAtt) }
            
            var xLines = lines.takeWhile(lineCond)
            val returnValue = fun(xLines)
            val tail = lines.dropWhile(lineCond)
            
            (returnValue, tail)
        }
        
        val ((an1, an2), anTail) = read(lines, "AU", readAN)
        val (authors, auTail) = read(anTail, "TI", readAU)
        val (title, tiTail) = read(auTail, "SO", readTI)
        val (sources, soTail) = read(tiTail, "AB", readSO)
        val (abstractText, abTail) = read(soTail, "CC", readAB)
        val (ccList, ccTail) = read(abTail, "UT", readCC)
        val (utList, utTail) = read(ccTail, "", readUT)
        
        new Paper((an1, an2), title, authors, sources, ccList, utList, abstractText)
    }

    def readAN(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "AN") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) =>
                    val parts = value.split('.')
                    (parts(0).toInt, parts(1).toInt)
                case None =>
                    (-1, -1)
            }
        } else {
            throw new RuntimeException("Invalid AN: " + lineAtt(lines(0)).get)
        }
    }

    def readAU(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "AU") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) =>
                    val authors = value.split(";").map(_.trim()).toList

                    authors.map(author => {
                        val parts = author.split(",").map(_.trim()).toList
                        if (parts.length == 2) {
                            new HumanAuthor(parts(1), parts(0))
                        } else if (parts.length == 1) {
                            new OtherAuthor(parts(0))
                        } else {
                            throw new IllegalArgumentException("Invalid Author: " + author)
                        }
                    })
                case None => List()
            }
        } else {
            throw new RuntimeException("Invalid AU")
        }
    }

    def readTI(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "TI") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) => value
                case None => ""
            }
        } else {
            throw new RuntimeException("Invalid TI")
        }
    }
    
    val soBook = """.*ISBN [0-9]+-+[0-9]+-+[0-9]+.*""".r
    val soRegComplete = """([^0-9]+)([0-9]+)?[^\w]*(No\. ?([0-9]+)(-[0-9]+)?,)?[^\w]*(([0-9]+)(--?([0-9]+))?)?[^\w]*\((([0-9]+)([^0-9][0-9]+)?)\)?\.?""".r
    val soRegShort = """([^0-9]+).*\((([0-9]+)([^0-9][0-9]+)?)\)\.?""".r
    
    def readSO(lines: List[String]): List[Source] = {
        if (lineAtt(lines(0)).get == "SO") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) => {
                    value match {
                        case soBook() => {
                            List(new OtherSource(value))
                        }
                        case _ => {
                            val sources = value.split(";").map(_.trim()).toList
                            
		                    sources.map(
		                        source => source match {
		                            case soRegComplete(a, b, c, d, e, f, g, h, i, j, k, l) => 
		                            
		                            val journal = a.trim
		                            val volume = if(b != null) b.trim.toInt else -1
		                            val nrFrom = if(d != null) d.trim.toInt else -1
		                            val nrTo = if(e == null) (if(d != null) d.trim.toInt else -1) else e.trim.toInt
		                            val pageFrom = if(g != null) g.trim.toInt else -1
		                            val pageTo = if(i == null) (if(g != null) g.trim.toInt else -1) else i.trim.toInt
		                            val year = k.trim.toInt
		                            
		                            new DetailledSource(
		                                journal, 
		                                volume, 
		                                nrFrom, 
		                                nrTo, 
		                                pageFrom, 
		                                pageTo,
		                                year
		                            )
		                            case soRegShort(journal, a, year, b) => {
		                                new DetailledSource(
			                                journal.trim, 
			                                -1, 
			                                -1, 
			                                -1, 
			                                -1, 
			                                -1,
			                                year.trim.toInt
		                                )
		                            }
		                            case s => 
		                                new OtherSource(s)
		                        }
                            )
                        }
                    }
                }
                case None => List()
            }
        } else {
            throw new RuntimeException("Invalid SO")
        }
    }
    
    def readCC(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "CC") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) => value.split("[;, ]").map(_.trim()).filter(_ != "").toList
                case None => List()
            }
        } else {
            throw new RuntimeException("Invalid CC")
        }
    }

    def readUT(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "UT") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) => value.split(";").map(_.trim()).toList
                case None => List()
            }
        } else {
            throw new RuntimeException("Invalid UT")
        }
    }

    def readAB(lines: List[String]) = {
        if (lineAtt(lines(0)).get == "AB") {
            lineVal(lines(0) + (if (lines.size > 1) lines.tail.mkString(" ") else "")) match {
                case Some(value) =>
                    if (value.length >= 9 && value.substring(0, 9) == "Summary: ")
                        value.substring(9, value.length) + (for (i <- 5 until lines.length) yield lines(i))
                    else
                        value
                case None => ""
            }
        } else {
            throw new RuntimeException("Invalid AB")
        }
    }

    def lineAtt(line: String) = {
        if (line.length < 2) None
        else Some(line.substring(0, 2))
    }

    def lineVal(line: String) = {
        if (line.length < 4 || line.substring(4, line.length()).trim() == "") None
        else Some(line.substring(4, line.length()))
    }
}