package filter
import java.io.File
import common.Common._
import format.arff_json.ArffJsonInstance
import java.util.TreeSet
import java.util.TreeMap
import java.util.{Set => JavaSet}
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable
import scala.collection.JavaConversions._
import format.arff_json.SparseArffJsonInstance
import format.arff_json.ArffJsonHeader
import format.arff_json.NumericArffJsonAttribute
import weka.core.stemmers.SnowballStemmer
import common.Common.FileConversion._
import scala.collection.mutable.HashMap
import parser.ArffJsonInstancesFile
import parser.ArffJsonInstancesSource
import parser.ArffJsonInstancesMapping
import format.arff_json.HistoryItem

object VectorFromDictFilter {
    def apply(confName: String) = {
        new StorableFilterFactory {
            def apply(trainBase: ArffJsonInstancesSource) = {
                val filter = confName match {
                    case "conf1" => new Conf1(this)
                    case "conf2" => new Conf2(this)
                    case "conf3" => new Conf3(this)
                    case "conf4" => new Conf4(this)
                    case "conf5" => new Conf5(this)
                    case _ => throw new RuntimeException("Unknown VectorFromDictFilter configuration: " + confName)
                }
                filter.buildDict(trainBase)
                filter
            }
            
            val historyAppendix = "vec-" + confName
            
            def load(file: File) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[VectorFromDictFilter]
        }
    }
    
    class Conf1(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].mkString(" ").split("\\s+").toSeq
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = word.filter(c => c.isLetter || c.isDigit).toLowerCase()
        }
    }
    
    class Conf2(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].toSeq
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = word.filter(_.isLetter).toLowerCase()
        }
    }
    
    class Conf3(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        @transient lazy val stemmer = new PorterStemmer
        override def inst2Words(inst: ArffJsonInstance) = {println(inst); inst.data(0).asInstanceOf[String].split("\\s+").toSeq}
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = stemmer.stem(word.filter(c => c.isLetter || c.isDigit).toLowerCase())
            override val stopList = new File("data/util/stoplist.txt").lines.toIndexedSeq
            override def wordCond(word: String) = word.length() >= 2
        }
    }
    
    class Conf4(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        @transient lazy val stemmer = new PorterStemmer
        
        def inst2Words(inst: ArffJsonInstance) = {
            val withoutDoubleDollar = inst.data(0).asInstanceOf[String]
                .replaceAllLiterally("$$", "$") // sometimes formulas are wrapped with two dollar signs - replace with one dollar sign
                
            val formulas = new mutable.ListBuffer[String]
            val withoutFormulas = """\$([^$]+)\$""".r 
                .replaceAllIn(
                    withoutDoubleDollar, 
                    g => {
                        formulas += {"$" + g.group(1).filter(c => !c.isSpaceChar) + "$"}
                        ""
                    }
                )
            
            val withoutQuotes = """\[([^]]+)\]""".r 
                .replaceAllIn(
                    withoutFormulas, 
                    g => ""
                )
            
            val words = withoutQuotes
                .replaceAll("""\\[a-zA-Z]+""", "") // remove all commands
                .replaceAll("""\\[^a-zA-Z]""", "") // remove escaped characters
                .split("[\\s\\.,]+").toSeq // next word on space full stop or comma
                .map(s => s.filter(c => c.isLetter || c.isDigit)) // after split keep only non special characters
                .filter(s => s != "") // and remove all empty words
                
            words ++ formulas
        }
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = stemmer.stem(word.filter(c => c.isLetter || c.isDigit || c == '$').toLowerCase())
            override val stopList = new File("data/util/stoplist.txt").lines.toIndexedSeq
            override def wordCond(word: String) = true
        }
    }
    
    class Conf5(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        @transient lazy val stemmer = new PorterStemmer
        
        def inst2Words(inst: ArffJsonInstance) = {
            val withoutDoubleDollar = inst.data(0).asInstanceOf[String]
                .replaceAllLiterally("$$", "$") // sometimes formulas are wrapped with two dollar signs - replace with one dollar sign
                
            val withoutFormulas = """\$([^$]+)\$""".r 
                .replaceAllIn(
                    withoutDoubleDollar, 
                    g =>  ""
                )
            
            val withoutQuotes = """\[([^]]+)\]""".r 
                .replaceAllIn(
                    withoutFormulas, 
                    g => ""
                )
            
            val words = withoutQuotes
                .replaceAll("""\\[a-zA-Z]+""", "") // remove all commands
                .replaceAll("""\\[^a-zA-Z]""", "") // remove escaped characters
                .split("[\\s\\.,]+").toSeq // next word on space full stop or comma
                .map(s => s.filter(c => c.isLetter || c.isDigit)) // after split keep only non special characters
                .filter(s => s != "") // and remove all empty words
                
            words
        }
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = stemmer.stem(word.filter(c => c.isLetter || c.isDigit || c == '$').toLowerCase())
            override val stopList = new File("data/util/stoplist.txt").lines.toIndexedSeq
            override def wordCond(word: String) = true
        }
    }
}

@serializable
abstract class VectorFromDictFilter extends GlobalFilter {
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    
    val dict: Dictionary
    
    def buildDict(source: ArffJsonInstancesSource) {
        println("train vector-filter with " + source.contentDescription)
        
        for(inst <- source.iterator) {
            for(word <- inst2Words(inst)) {
                dict.add(word)
            }
        }
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        println("use vector-filter on " + source.contentDescription)
        
        source.map(
            elemFun = elements => elements.map(inst => {
                val data = new TreeMap[Int, Double]
                
                for(word <- inst2Words(inst)) {
                    dict.wordIndex(word) match {
                        case Some(index) => data(index) = data.getOrElse(index, 0.0) + 1.0
                        case None => 
                    }
                }
                
                new SparseArffJsonInstance(inst.id, inst.mscClasses, data.toMap, dict.size())
            }),
            headerFun = header => new ArffJsonHeader(
                header.relationName, 
                dict.map(w => new NumericArffJsonAttribute(w)).toList.sortBy(_.name), List()
            ),
            historyAppendix = historyAppendix
        )
    }
}











