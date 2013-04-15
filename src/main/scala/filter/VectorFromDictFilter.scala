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
import parser.ArffJsonInstancesSource
import parser.History
import classifier.CategoryIs

object VectorFromDictFilter {
    def apply(confName: String, minOcc: Int = 3) = {
        new FilterFactory with Loadable[VectorFromDictFilter] {
            def apply(trainBase: ArffJsonInstancesSource) = {
                val filter = confName match {
                    case "conf1" => new Conf1(minOcc)
                    case "conf2" => new Conf2(minOcc)
                    case "conf3" => new Conf3(minOcc)
                    case "conf4" => new Conf4(minOcc)
                    case "conf5" => new Conf5(minOcc)
                    case "conf6" => new Conf6(minOcc)
                    case "conf7" => new Conf7(minOcc)
                    case "conf8" => new Conf8(minOcc)
                    case "conf9" => new Conf9(minOcc)
                    case "conf10" => new Conf10(minOcc)
                    case "conf11" => new Conf11(minOcc)
                    case "conf12" => new Conf12(minOcc)
                    case "conf13" => new Conf13(minOcc)
                    case _ => throw new RuntimeException("Unknown VectorFromDictFilter configuration: " + confName)
                }
                filter.buildDict(trainBase)
                filter
            }
            
            val historyAppendix = "vec-" + confName + "-min-" + minOcc
        }
    }
    
    @serializable
    trait Appendix extends History {
        val confName: String
        val minOcc: Int = 3
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ VectorFromDictFilter(confName, minOcc)
    }
    
    class Conf1(minOcc: Int) extends VectorFromDictFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].mkString(" ").split("\\s+").toSeq
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = word.filter(c => c.isLetter || c.isDigit).toLowerCase()
            override val minWordCount = minOcc
        }
    }
    
    class Conf2(minOcc: Int) extends VectorFromDictFilter {
        override def inst2Words(inst: ArffJsonInstance) = inst.data(0).asInstanceOf[List[String]].toSeq
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = word.filter(_.isLetter).toLowerCase()
            override val minWordCount = minOcc
        }
    }
    
    class Conf3(minOcc: Int) extends VectorFromDictFilter {
        @transient lazy val stemmer = new PorterStemmer
        override def inst2Words(inst: ArffJsonInstance) = {inst.data(0).asInstanceOf[String].split("[\\s.,;:]+").toSeq}
        
        override val dict = new Dictionary {
            override def wordFun(word: String) = stemmer.stem(word.filter(c => c.isLetter || c.isDigit).toLowerCase())
            override val stopList = new File("data/util/stoplist.txt").lines.toIndexedSeq
            override def wordCond(word: String) = word.length() >= 2
            override val minWordCount = minOcc
        }
    }
    
    class Conf4(minOcc: Int) extends VectorFromDictFilter {
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
            override val minWordCount = minOcc
        }
    }
    
    class Conf5(minOcc: Int) extends VectorFromDictFilter {
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
            override val minWordCount = minOcc
        }
    }
    
    class Conf6(minOcc: Int) extends VectorFromDictFilter {
        import AdvancedTokenizer._
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            transformStringTokens(
                replaceAcronyms(
                    TokenGroup.group(
                        //splitMultipleAuthors(
                            tokenize(text, separators).filter(t => !(t.isInstanceOf[Author] || t.isInstanceOf[Formula] || t.isInstanceOf[Reference]))
                        //)
                    )
                )
            ).map(_.toString)
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf7(minOcc: Int) extends VectorFromDictFilter {
        // only strings - ignore authors/formulas/references
        // strings to lower - filtered for digits and letters - stemmed
        // minus is seperator
        // min count is 10
        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => if(stopList.contains(word.toLowerCase())) "" else { 
            stemmer.stem(word
                .filter(c => c.isDigit || c.isLetter)
                .toLowerCase()
            )
        }
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            val words = tokenize(text, separators)
                .filter(t => t.isInstanceOf[TokenString])
                .map(t => wordTransformFunction(t.asInstanceOf[TokenString].str))
                .filter(t => t != "")
                
            words
        }
        
        val separators = List('.', ':', ';', ',', '-')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf8(minOcc: Int) extends VectorFromDictFilter {
        // strings/authors/formulas - ignore references
        // strings to lower - filtered for digits and letters - stemmed
        // authors to lower - formulas without spaces
        // minus is seperator
        // min count is 10
        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            val words = tokenize(text, separators)
                .flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List("{" + str.toLowerCase() + "}")
                        case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                        case _ => List()
                    }
                    
                )
                
            words
        }
        
        val separators = List('.', ':', ';', ',', '-')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf9(minOcc: Int) extends VectorFromDictFilter {
        // strings/authors/formulas - ignore references
        // strings to lower - filtered for digits and letters - stemmed
        // authors to lower - formulas without spaces
        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            val words = replaceAcronyms2(
                tokenize(text, separators)
                    .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                ).flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List("{" + str.toLowerCase() + "}")
                        case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                        case _ => List()
                    }
                )
                .filter(s => s != "")
            
            words
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf10(minOcc: Int) extends VectorFromDictFilter {        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            val words = replaceAcronyms2(
                tokenize(text, separators)
                    .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                ).flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List()
                        case Formula(str) => List()
                        case _ => List()
                    }
                )
                .filter(s => s != "")
            
            words
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf11(minOcc: Int) extends VectorFromDictFilter {        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            val words = replaceAcronyms2(
                tokenize(text, separators)
                    .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                ).flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List("{" + str.toLowerCase() + "}")
                        case Formula(str) => List()
                        case _ => List()
                    }
                )
                .filter(s => s != "")
            
            words
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf12(minOcc: Int) extends VectorFromDictFilter {        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            val words = replaceAcronyms2(
                tokenize(text, separators)
                    .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                ).flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List()
                        case Formula(str) => List("$" + str.filter(c => c.isDigit || c.isLetter).toLowerCase() + "$")
                        case _ => List()
                    }
                )
                .filter(s => s != "")
            
            words
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    class Conf13(minOcc: Int) extends VectorFromDictFilter {        
        import AdvancedTokenizer._
        
        val wordTransformFunction = (word: String) => stemmer.stem(word
            .filter(c => c.isDigit || c.isLetter)
            .toLowerCase()
        )
        
        def inst2Words(inst: ArffJsonInstance) = {
            val text = inst.data(0).asInstanceOf[String]
            
            val words = replaceAcronyms2(
                tokenize(text, separators)
                    .filter(t => t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t.isInstanceOf[Formula] || t.isInstanceOf[Author])
                ).flatMap(t => 
                    t match {
                        case TokenString(str) => if(stopList.contains(str)) List() else List(wordTransformFunction(str))
                        case Author(str) => List()
                        case Formula(str) => List()
                        case Reference(str) => List("[" + str + "]")
                        case _ => List()
                    }
                )
                .filter(s => s != "")
            
            words
        }
        
        val separators = List('.', ':', ';', ',')
        
        override val dict = new Dictionary() {
            override val minWordCount = minOcc
        }
    }
    
    object AdvancedTokenizer {
        @transient lazy val stemmer = new PorterStemmer
        @transient lazy val stopList = new File("data/util/stoplist.txt").lines.map(_.toLowerCase()).toIndexedSeq
        @serializable trait Token
        
        case object WhiteSpace extends Token
        case class SeparatorChar(val c: Char) extends Token
        case class TokenChar(val c: Char) extends Token
        case class TokenString(val str: String) extends Token {
            override def toString = str
        }
        case class Reference(val text: String) extends Token {
            val zblRe = """.*Zbl ([0-9]+\.[0-9]+).*""".r
            
            override def toString = {
                "[" + (text match {
                    case zblRe(nr) => nr
                    case x => text
                }) + "]"
            }
        }
        case class Author(val text: String) extends Token {
            def split = text.split(", ").toList.map(t => Author(t))
            
            override def toString = "{" + text + "}"
        }
        case class Formula(val text: String) extends Token {
            override def toString = "$" + text + "$"
        }
        case class BracedText(val text: String) extends Token {
            override def toString = "(" + text + ")"
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
            
            override def toString = tokens.map(_.toString).mkString(" <-> ")
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
        
        
        def tokenize(text: String, separators: List[Char] = List('.', ':', ';', ',')) = {
            val sb = new StringBuffer()
            sb.append(text)
            val tokenList = new mutable.ListBuffer[Token]
            
            while(sb.length > 0) {
                tokenList += nextToken(sb, separators)
            }
            
            Token.compress(tokenList.toList)
        }
    
        
        def nextToken(text: StringBuffer, separators: List[Char]): Token = {
            text.charAt(0) match {
                case '$' =>
                    try {
                        readFormula(text)
                    } catch {
                        case _: Throwable => 
                            val char = new TokenChar('$')
                            text.deleteCharAt(0)
                            char
                    }
                case '[' =>
                    try {
                        readReference(text)
                    } catch {
                        case _: Throwable => 
                            val char = new TokenChar('[')
                            text.deleteCharAt(0)
                            char
                    }
                case '{' if text.length > 4 && text.substring(1, 5) == "\\it " => 
                    readAuthor(text)
                case '(' => 
                    readBracedText(text)
                case c if separators.contains(c) =>
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
        
        
        def splitMultipleAuthors(tokens: List[Token]) = {
            tokens.flatMap(t => 
                t match {
                    case a: Author => a.split
                    case t => List(t)
                }
            )
        }
    
        def replaceAcronyms2(tokenList: List[Token]): List[Token] = {
            val acronyms = new HashMap[Token, List[Token]]
            
            for((g, i) <- tokenList.zipWithIndex) {
                g match {
                    case BracedText(text) if text.size > 1 && text.forall(c => c.isUpper) && i>text.size => 
                        val candidates = tokenList.slice(i-text.size, i)
                        if(
                            candidates.forall(c => c.isInstanceOf[TokenString]) && 
                            text.toUpperCase.zip(
                                candidates.map(c => 
                                    c.asInstanceOf[TokenString].str.charAt(0).toUpper
                                )
                            ).map(p => p._1 == p._2).reduceLeft(_ && _)) {
                            acronyms += (new TokenString(text) -> candidates)
                        }
                    case _ => 
                }
            }
            
            tokenList.flatMap(t => {
                if(acronyms.keySet.contains(t)) 
                    acronyms(t) 
                else if(t.isInstanceOf[BracedText]) {
                    List()
                } else {
                    List(t)
                }
            })
        }
        
        def replaceAcronyms(tokenGroups: List[TokenGroup]): List[TokenGroup] = {
            val acronyms = new HashMap[TokenGroup, List[TokenGroup]]
            
            for((g, i) <- tokenGroups.zipWithIndex) {
                if(g.tokens.size == 1) {
                    g.tokens.head match {
                        case BracedText(text) if text.size > 1 && text.forall(c => c.isUpper) && i>text.size => 
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
        
        val defaultWordMapping = ((word: String) => {
            if(stopList.contains(word.toLowerCase())) ""
            else {
                stemmer.stem(
                    word
                        .toLowerCase()
                        .replaceAll("""\\[a-zA-Z]+""", "") // remove all commands
                        .replaceAll("""\\[^a-zA-Z]""", "") // remove escaped characters
                        .filter(c => c.isDigit || c.isLetter)
                )
            }
        })
        
        def transformStringTokens(tokenGroups: List[TokenGroup], wordMapping: (String => String) = defaultWordMapping) = {
            tokenGroups.flatMap(g => {
                val newGroup = new TokenGroup(g.tokens.map(t => 
                    t match {
                        case TokenString(text) => TokenString(
                            wordMapping(text)
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
            var dept = 1
            while(i < text.length && dept > 0) {
                if(text.charAt(i) == ']') dept -= 1
                if(text.charAt(i) == '[') dept += 1
                i += 1
            }
            
            val sourceLink = new Reference(text.substring(1, i-1))
            text.delete(0, i)
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
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = ((inst: ArffJsonInstance) => {
                val data = new TreeMap[Int, Double]
                
                for(word <- inst2Words(inst)) {
                    dict.wordIndex(word) match {
                        case Some(index) => data(index) = data.getOrElse(index, 0.0) + 1.0
                        case None => 
                    }
                }
                
                new SparseArffJsonInstance(inst.id, inst.categories, data.toMap, dict.size())
            }),
            headerFun = header => new ArffJsonHeader(
                header.relationName, 
                dict.map(w => new NumericArffJsonAttribute(w)).toList.sortBy(_.name)
            )
        )
    }
}











