package script.formulas

import scala.collection.mutable
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.File
import scala.io.Source

object ReadMSCOnIndexNodes {
    def bestAttributes(numAttributes: Int) = {
        val fis = new BufferedInputStream(new FileInputStream(new File("C:\\Users\\Simon\\Desktop\\id_mscs.txt")))
        
        val nodes = new mutable.HashMap[Int, mutable.HashMap[Int, Int]] {
            override def default(key: Int) = new mutable.HashMap[Int, Int] {
                override def default(key: Int) = 0
            }
        }
        var nodeId = 0
        var mscId = 0
        var mscValue = 0
        
        var state = 0
        val charBuffer = new StringBuilder
        
        var c: Char = ' '
        while({ val i = fis.read(); c = i.asInstanceOf[Char]; i != -1 }) {
            val stateBefore = state
            state match {
                case 0 => c match {
                    case s if s.isWhitespace => 
                    case '{' => state = 1
                    case _ => state = -1
                }
                case 1 => c match {
                    case s if s.isWhitespace =>
                    case c if c.isDigit => 
                        charBuffer.append(c)
                        state = 2
                    case _ => state = -1
                }
                case 2 => c match {
                    case d if d.isDigit => 
                        charBuffer.append(d)
                    case s if s.isWhitespace => state = 3
                        nodeId = charBuffer.toString.toInt
                        charBuffer.clear
                    case ':' => state = 4
                        nodeId = charBuffer.toString.toInt
                        charBuffer.clear
                    case _ => state = -1
                }
                case 3 => c match {
                    case s if s.isWhitespace =>
                    case ':' => state = 4
                    case _ => state = -1
                }
                case 4 => c match {
                    case s if s.isWhitespace => 
                    case '[' => state = 5
                    case _ => state = -1
                }
                case 5 => c match {
                    case s if s.isWhitespace => 
                    case d if d.isDigit => 
                        charBuffer.append(d)
                        state = 6
                    case _ => state = -1
                }
                case 6 => c match {
                    case d if d.isDigit => 
                        charBuffer.append(d)
                    case s if s.isWhitespace => 
                        mscId = charBuffer.toString.toInt
                        charBuffer.clear
                        state = 7
                    case ':' =>
                        mscId = charBuffer.toString.toInt
                        charBuffer.clear
                        state = 8
                    case _ => state = -1
                }
                case 7 => c match {
                    case ':' => state = 8
                    case s if s.isWhitespace =>
                    case _ => state = -1
                }
                case 8 => c match {
                    case d if d.isDigit => {
                        charBuffer.append(d)
                        state = 9
                    }
                    case s if s.isWhitespace => 
                    case _ => state = -1
                }
                case 9 => c match {
                    case d if d.isDigit => {
                        charBuffer.append(d)
                    }
                    case s if s.isWhitespace =>
                        mscValue = charBuffer.toString.toInt
                        charBuffer.clear
                        val map = nodes(nodeId)
                        map(mscId) = mscValue
                        nodes(nodeId) = map
                        state = 10
                    case ',' => 
                        mscValue = charBuffer.toString.toInt
                        charBuffer.clear
                        val map = nodes(nodeId)
                        map(mscId) = mscValue
                        nodes(nodeId) = map
                        state = 5
                    case ']' => 
                        mscValue = charBuffer.toString.toInt
                        charBuffer.clear
                        val map = nodes(nodeId)
                        map(mscId) = mscValue
                        nodes(nodeId) = map
                        state = 11
                    case _ => state = -1
                }
                case 10 => c match {
                    case ',' => state = 5
                    case ']' => state = 11
                    case s if s.isWhitespace =>
                    case _ => state = -1
                }
                case 11 => c match {
                    case s if s.isWhitespace =>
                    case ',' => state = 1
                    case '}' => state = -2
                }
                case -1 =>
                case -2 => c match {
                    case s if s.isWhitespace =>
                    case _ => state = -1
                }
            }
        }
        
        val mappingReader = new ReadDocuments.MappingReader(new File("C:/Users/Simon/Desktop/files.txt"))
        
        val numInstForEachClass = new mutable.HashMap[String, Int]() {
            override def default(key: String) = 0
        }
        
        for(mapping <- mappingReader; cat <- mapping._2) {
            numInstForEachClass(cat) = numInstForEachClass(cat) + 1
        }
        
        val frequentClasses = numInstForEachClass
                .filter(kv => kv._2 > 100 && kv._1.length == 2)
                .map(kv => kv._1.toInt -> kv._2)
        
        nodes.mapValues(
            m => {
                val normedValues = m
                    .filter(kv => frequentClasses.keySet.contains(kv._1))
                    .map(kv => kv._2.toDouble / frequentClasses(kv._1))
                
                val sum = normedValues.reduceLeft(_ + _)
                val avg = sum.toDouble / normedValues.size
                
                normedValues.map(a => math.abs(a - avg)).reduceLeft(_ + _) / normedValues.size
            }).toList
            .sortWith((a, b) => a._2 > b._2)
            .take(numAttributes)
            .map(_._1)
    }
}













