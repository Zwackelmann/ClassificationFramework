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
                    case "conf6" => new Conf6(this)
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
    
    class Conf6(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
        @transient lazy val stemmer = new PorterStemmer
    
        def splitMultipleAuthors(tokens: List[Token]) = {
            tokens.flatMap(t => 
                t match {
                    case a: Author => a.split
                    case t => List(t)
                }
            )
        }
    
        def replaceAcronyms(tokenGroups: List[TokenGroup]) = {
            val acronyms = new HashMap[TokenGroup, List[TokenGroup]]
            
            for((g, i) <- tokenGroups.zipWithIndex) {
                if(g.tokens.size == 1) {
                    g.tokens.head match {
                        case BracedText(text) if text.size > 1 && text.forall(c => c.isUpperCase) && i>text.size => 
                            val candidates = tokenGroups.slice(i-text.size, i)
                            if(
                                candidates.forall(c => c.tokens.size == 1 && c.tokens.head.isInstanceOf[TokenString]) && 
                                text.toUpperCase.zip(
                                    candidates.map(c => 
                                        c.tokens.head.asInstanceOf[TokenString].str.charAt(0).toUpper
                                    )
                                ).map(p => p._1 == p._2).reduceLeft(_ && _)) {
                                acronyms += (new TokenGroup(List(new TokenString(text))) -> candidates)
                            }
                        case _ => 
                    }
                }
            }
            
            tokenGroups.flatMap(t => {
                if(acronyms.keySet.contains(t)) 
                    acronyms(t) 
                else if(t.tokens.size == 1 && t.tokens.head.isInstanceOf[BracedText]) {
                    List()
                } else {
                    List(t)
                }
            })
        }
    
        def transformStringTokens(tokenGroups: List[TokenGroup]) = {
            val stopList = new File("data/util/stoplist.txt").lines.toIndexedSeq
            
            tokenGroups.flatMap(g => {
                val newGroup = new TokenGroup(g.tokens.map(t => 
                    t match {
                        case TokenString(text) => TokenString(
                            if(stopList.contains(text.toLowerCase())) ""
                            else {
                                stemmer.stem(
                                    text
                                        .toLowerCase()
                                        .replaceAll("""\\[a-zA-Z]+""", "") // remove all commands
                                        .replaceAll("""\\[^a-zA-Z]""", "") // remove escaped characters
                                        .filter(c => c.isDigit || c.isLetter)
                                )
                            }
                        )
                        case Formula(text) => Formula(text.filter(c => !c.isWhitespace))
                        case t => t 
                    }
                ).filter(t => t match {
                    case TokenString(text) => text != ""
                    case _: BracedText => false
                    case _ => true
                }))
                
                if(newGroup.tokens.size == 0) List()
                else List(newGroup)
            })
        }
    
        def tokenize(text: StringBuffer) = {
            val tokenList = new mutable.ListBuffer[Token]
            
            while(text.length > 0) {
                tokenList += nextToken(text)
            }
            
            Token.compress(tokenList.toList)
        }
    
        def nextToken(text: StringBuffer): Token = {
            text.charAt(0) match {
                case '$' =>
                    try {
                        readFormula(text)
                    } catch {
                        case _ => 
                            val char = new TokenChar('$')
                            text.deleteCharAt(0)
                            char
                    }
                case '[' =>
                    readReference(text)
                case '{' if text.length > 4 && text.substring(1, 4) == "\\it " => 
                    readAuthor(text)
                case '(' => 
                    readBracedText(text)
                case c if c == '.' || c == ':' || c == ',' || c == ';' =>
                    val char = new SeparatorChar(c)
                    text.deleteCharAt(0)
                    char
                case c if c.isWhitespace => 
                    text.deleteCharAt(0)
                    WhiteSpace
                case c => 
                    val char = new TokenChar(c)
                    text.deleteCharAt(0)
                    char
            }
        }
    
        def readFormula(text: StringBuffer) = {
            if(text.charAt(0) != '$') error("first char must be a dollar if readFormula funciton is called")
            
            var i = 0
            var iFrom = 0
            var iTo = 0
            
            while(i < text.length && text.charAt(i) == '$') {
                i += 1
            }
            iFrom = i
            if(iFrom > 2) error("there were 3 dollars to introduce a formula...")
            
            while(i < text.length && text.charAt(i) != '$') {
                i += 1
            }
            iTo = i
            
            while(i < text.length && text.charAt(i) == '$') {
                i += 1
            }
            
            if(iFrom != (i - iTo)) error("the number of opening and closing dollar signs do not match: " + text)
            
            val formula = new Formula(text.substring(iFrom, iTo))
            text.delete(0, i)
            formula
        }
    
        def readReference(text: StringBuffer) = {
            if(text.charAt(0) != '[') error("first char must be a opening square bracket if readReference funciton is called")
            
            var i = 1
            while(i < text.length && text.charAt(i) != ']') {
                i += 1
            }
            
            val sourceLink = new Reference(text.substring(1, i))
            text.delete(0, i+1)
            sourceLink
        }
    
        def readAuthor(text: StringBuffer) = {
            if(text.substring(0, 5) != "{\\it ") error("an author link must start with \"{\\it \"")
            
            var i = 4
            while(i < text.length && text.charAt(i) != '}') {
                i += 1
            }
            
            val authorLink = new Author(text.substring(5, i))
            text.delete(0, i+1)
            authorLink
        }
        
        def readBracedText(text: StringBuffer) = {
            if(text.charAt(0) != '(') error("first char must be a opening brace if readBracedText funciton is called")
            
            var i = 1
            while(i < text.length && text.charAt(i) != ')') {
                i += 1
            }
            
            val bracedText = new BracedText(text.substring(1, i))
            text.delete(0, i+1)
            bracedText
        }
        
        object TokenGroup {
            def group(tokens: List[Token]) = {
                val groupBuffer = new mutable.ListBuffer[Token]
                val groups = new mutable.ListBuffer[TokenGroup]
                
                def flush {
                    if(!groupBuffer.isEmpty) {
                        groups += new TokenGroup(groupBuffer.toList)
                        groupBuffer.clear()
                    }
                }
                
                for(token <- tokens) {
                    token match {
                        case WhiteSpace | _: SeparatorChar => 
                            flush
                        case r: Reference =>
                            flush
                            groups += new TokenGroup(List(r))
                        case a: Author => 
                            flush
                            groups += new TokenGroup(List(a))
                        case t => 
                            groupBuffer += t
                    }
                }
                flush
                
                groups.toList
            }
        }
        
        case class TokenGroup(val tokens: List[Token]) {
            override def hashCode() = tokens.hashCode()
        }
        
        @serializable
        object Token {
            def compress(tokens: List[Token]) = {
                val tokenCharBuffer = new mutable.ListBuffer[TokenChar]
                val tokenBuffer = new mutable.ListBuffer[Token]
                
                for(token <- tokens) {
                    token match {
                        case char: TokenChar => 
                            tokenCharBuffer += char
                        case other =>
                            if(!tokenCharBuffer.isEmpty) {
                                tokenBuffer += new TokenString(tokenCharBuffer.map(c => c.c).mkString)
                                tokenCharBuffer.clear
                            }
                            tokenBuffer += other
                    }
                }
                if(!tokenCharBuffer.isEmpty) {
                    tokenBuffer += new TokenString(tokenCharBuffer.map(c => c.c).mkString)
                }
                
                tokenBuffer.toList
            }
        }
        
        @serializable
        trait Token
        
        @serializable case object WhiteSpace extends Token
        @serializable case class SeparatorChar(val c: Char) extends Token
        @serializable case class TokenChar(val c: Char) extends Token
        @serializable case class TokenString(val str: String) extends Token
        @serializable case class Reference(val text: String) extends Token {
            val zblRe = """.*Zbl ([0-9]+\.[0-9]+).*""".r
            
            override def toString = {
                "Reference(" + (text match {
                    case zblRe(nr) => nr
                    case x => text
                }) + ")"
            }
        }
        case class Author(val text: String) extends Token {
            def split = text.split(", ").toList.map(t => Author(t))
        }
        case class Formula(val text: String) extends Token
        case class BracedText(val text: String) extends Token
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
                
            val sb = new StringBuffer
            sb.append(text)
            
            transformStringTokens(
                replaceAcronyms(
                    TokenGroup.group(
                        //splitMultipleAuthors(
                            tokenize(sb).filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t == WhiteSpace)
                        //)
                    )
                )
            ).map(_.toString)
        }
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = word
            override val stopList = List()
            override def wordCond(word: String) = true
        }
    }
}

@serializable
abstract class VectorFromDictFilter extends GlobalFilter {
    def inst2Words(inst: ArffJsonInstance): Seq[String]
    
    val dict: Dictionary
    
    def buildDict(source: ArffJsonInstancesSource) {
        for(inst <- source.iterator) {
            for(word <- inst2Words(inst)) {
                dict.add(word)
            }
        }
        
        val bw = new BufferedWriter(new FileWriter(new File("dictText")))
        for(word <- dict) {
            bw.write(word + "\n")
        }
        bw.close()
        println(dict.size + " words in dict")
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











