package script
import parser.ArffJsonInstancesFile
import parser.ContentDescription
import parser.ArffJsonInstancesFile2
import java.io.File
import filter.VectorFromDictFilter
import filter.Dictionary
import format.arff_json.ArffJsonInstance
import scala.collection.mutable
import common.Common.FileConversion._
import format.arff_json.HistoryItem
import filter.PorterStemmer
import filter.Dictionary$
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition
import filter.GlobalFilter
import filter.feature_scoreing.OddsRatio
import filter.OddsRatioFilter
import classifier.TopClassIs
import weka.classifiers.meta.AdaBoostM1
import weka.classifiers.trees.J48
import classifier.WekaClassifier
import model.RawClassification
import classifier.TrainSetSelection
import filter.TfIdfFilter
import external.GensimLsiFilter
import filter.NormalizeVectorFilter

object Test6 {
    def main(args: Array[String]) {
        val trainSet = new ArffJsonInstancesFile2(new File("data/arffJson/final-test_proj-abs.json"), ContentDescription("final", ContentDescription.TrainSet, List()))
        val testSet = new ArffJsonInstancesFile2(new File("data/arffJson/final-train_proj-abs.json"), ContentDescription("final", ContentDescription.TrainSet, List()))
        
        def vectorFilters = for(minCount <- List(1, 3, 5, 10, 15, 25, 50, 100)) yield (
            new AdvancedVectorFromDictFilter(HistoryItem("")) {
                def inst2Words(inst: ArffJsonInstance) = {
                    val text = inst.data(0).asInstanceOf[String]
                    
                    transformStringTokens(
                        replaceAcronyms(
                            TokenGroup.group(
                                splitMultipleAuthors(
                                    tokenize(text).filter(t => true /*t.isInstanceOf[TokenString] || t.isInstanceOf[BracedText] || t == WhiteSpace*/)
                                )
                            )
                        )
                    ).map(_.toString)
                }
                
                val dict = new Dictionary {
                    override val minWordCount = minCount
                }
            },
            "minCount: " + minCount.toString()
        )
        // eventuell ist es nicht sinnvoll bei dem experiment tf-idf anzuwenden, da die selten vorkommenden terme dann ein höheres gewicht bekommen
        // es soll ja grad geschaut werden, ob auf die verzichtet werden kann - daher sollten sie vielleicht kein höheres gewicht bekommen???
        for((vectorFilter, vectorFilterName) <- vectorFilters; targetClass <- List(TopClassIs("00"), TopClassIs("15"), TopClassIs("35"))) {
            println("\n\nstart evaluating " + vectorFilterName + " | " + targetClass.filenameExtension)
            
            val (lsiTrainSet, lsiTestSet) = {
                val (normalizedTrainSet, normalizedTestSet) = {
                    val (tfIdfTrainSet, tfIdfTestSet) = {
                        val (vectorTrainSet, vectorTestSet) = {
                            println("buldDict")
                            vectorFilter.buildDict(trainSet)
                            println("apply vector filter")
                            (trainSet.applyFilter(vectorFilter), testSet.applyFilter(vectorFilter))
                        }
                        
                        println("train tf idf")
                        val tfIdfFilter = new TfIdfFilter(vectorTrainSet, HistoryItem(""))
                        println("apply tf idf filter")
                        (vectorTrainSet.applyFilter(tfIdfFilter), vectorTestSet.applyFilter(tfIdfFilter))
                    }
                    
                    val normalizeFilter = new NormalizeVectorFilter(HistoryItem(""))
                    (tfIdfTrainSet.applyFilter(normalizeFilter), tfIdfTestSet.applyFilter(normalizeFilter))
                }
                println("train lsi")
                val lsiFilter = new GensimLsiFilter(500, HistoryItem(""))
                lsiFilter.builtModel(normalizedTrainSet)
                
                println("apply lsi filter")
                (normalizedTrainSet.applyFilter(lsiFilter), normalizedTestSet.applyFilter(lsiFilter))
            }
            
            println("train classifier")
            val boostedC45Classifier = new WekaClassifier(
                lsiTrainSet,
                targetClass
            ) {
                def classifierConfig() = {
                    val ada = new AdaBoostM1()
                    ada.setClassifier(new J48)
                    ada.setNumIterations(10)
                    ada
                }
            }
            
            println("calculate classifications")
            val results = boostedC45Classifier.classifications(lsiTestSet)
            println("save results")
            RawClassification.save(results, new File("results-" + vectorFilterName + "-" + targetClass.filenameExtension))
        }
    }
    
    abstract class AdvancedVectorFromDictFilter(val historyAppendix: HistoryItem) extends VectorFromDictFilter {
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
            val acronyms = new mutable.HashMap[TokenGroup, List[TokenGroup]]
            
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
        
        def tokenize(text: String) = {
            val sb = new StringBuffer
            sb.append(text)
            
            val tokenList = new mutable.ListBuffer[Token]
            
            while(sb.length > 0) {
                tokenList += nextToken(sb)
            }
            
            Token.compress(tokenList.toList)
        }
    }
}


















