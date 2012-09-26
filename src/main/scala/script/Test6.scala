package script

import common.Common.FileConversion._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import filter.PorterStemmer

object Test6 {
    @transient lazy val stemmer = new PorterStemmer
    
    def main(args: Array[String]) {
        val text = new StringBuffer
        text.append("""A new method, called Direct Filter Approach (DFA), is presented in the book. It is intended to solve the well-known signal estimation problem, related with the fast detection of turning-points in time series. Often, signal estimates are subject to significant time delays towards the end point of a finite sample. Therefore, turning-points of the signal cannot be detected in time. The DFA proposed in the book enables one to constrain filters such that the time delay becomes smaller. These issues are studied in the book.\\par The book is organized as follows: Chapter 1 provides an overview on the problem. A brief description of the well-known Model Based Approach (MBA) is given. It is shown that it may be inefficient in the context of fast detection of turning-points in time series. The need of an efficient general signal estimation method for these important applications is demonstrated. \\par In chapter 2, MBAs are presented. The objective is to compare the proposed DFA with established MBAs, and to use two of most popular MBAs as ``benchmarks\" in chapters 7 and 8. The aim of the author is not to provide an exhaustive list of existing methods but to describe established procedures which are implemented in ``widely used\" software packages.\\par In chapter 3 the main concepts are proposed that are needed for the description of filters in the frequency domain (such as transfer functions, amplitude functions or phase functions). A new class of Zero-Pole-Combination (ZPC) filters is derived. Their characteristics ``match\" the signal estimation problem.\\par In chapter 4 properties of the periodogram and technical details related to the DFA are analyzed. For the DFA, an eminent role is awarded to the periodogram (or to statistics directly related to the periodogram). It ``collects\" and transforms the information of the sample $X_1,\\dots, X_N$ into a form suitable for the signal estimation problem. In particular, the statistic is analyzed for integrated processes. Stochastic properties of ``squared\" periodogram ordinates are analyzed in the appendix. Both kinds of results are omitted in the ``traditional\" time series literature and are needed here for proving theoretical results in the next chapter. An explorative instrument for assessing possible ``unit-root misspecifications\" of the filter design for the DFA is also proposed.\\par In chapter 5 the main theoretical results for the DFA are reported: consistency, efficiency, the generalization to non-stationary integrated input processes, the generalized conditional optimization (resulting in asymmetric filters with smaller time delays) and the asymptotic distribution of the estimated filter parameters (which enables hypothesis testing). In particular, a generalized unit-root test is proposed which is designed for the signal estimation problem.\\par In chapter 6 the overfitting problem is analyzed. In order to prove the results in the previous chapter, regularity assumptions are needed. One of these assumptions is directly related to the overfitting problem. Overparameterization and overfitting are distinguished and new procedures are proposed for ``tackling\" their various aspects. An estimation of the order of the asymmetric filter is presented (which avoids more specifically overparameterization), based on the asymptotic distribution of the parameter estimates. The proposed method does not rely on ``traditional\" information criteria, because the DGP of is not of immediate concern. However, it is shown in the appendix that traditional information criteria may be considered as special cases of the proposed method. Also, new procedures ensuring regularity of the DFA solution are proposed which solve specific overfitting problems. The key idea behind these new methods is to modify the original optimization criterion such that overfitting becomes ``measurable\'\'. It is felt that these ideas may also be useful when modeling the DGP for the MBA. \\par In chapter 7 empirical results are presented. They are based on the simulation of artificial processes and on a ``real-world\" time series. The DFA is compared with the MBA with respect to mean square performances. It is shown that the DFA performs as well as maximum likelihood estimates for artificial times series. If the DGP is unknown, as is the case for the ``real-world\" time series, the DFA outperforms two established MBAs. The increased performance is achieved with respect to various signal definitions (two different trend signals and a particular seasonal adjustment) both ``in\" and ``out of sample\". It is also suggested that statistics relying on the one-step ahead forecasts, like ``traditional\" unit-root tests (augmented Dickey-Fuller and Phillips-Perron tests) or diagnostic tests (like for example Ljung-Box tests) may be misleading for the signal estimation problem if the true DGP is unknown. Instead, specific instruments derived in chapters 4, 5 and 6 are used for determining the optimal filter design for the DFA. These instruments, which are based on estimated filter errors (rather than one-step ahead forecasting errors of the model), indicate smaller integration orders for the analyzed time series. A possible explanation for these differences may be seen in the fact that filter errors implicitly account for one- and multi-step ahead forecasts simultaneously. A further analysis of the revision errors (filter approximation errors) is provided.\\par In chapter 8, an empirical comparison of the DFA and the MBA with respect to their ability to detect turning-points (of two different trend components) is conducted. The MBA is compared with the ``original\" DFA and with the result of a generalized constrained optimization (whose filter solution has a smaller time delay). As in the preceding chapter, the DFA generally outperforms the MBA with respect to the proposed criterion. The main advantages of the DFA are efficiency and flexibility. The corresponding filters can be optimized with respect to the traditional mean square error criterion or with respect to another practical, important objective, namely the fast detection of turning-points. The statistical instruments presented in the book solve the finite sample signal estimation problem more efficiently than MBA because, unlike the latter, the optimization criterion for DFA is derived from the estimation problem which has to be solved really.\\par The book can be of great interest for all specialists working in the area of nonlinear systems state and parameter estimation and identification. It will be of significant benefit for time series estimation and prediction in many applications.""")
        
        val tokenGroups = transformStringTokens(
            replaceAcronyms(
                TokenGroup.group(
                    splitMultipleAuthors(
                        tokenize(text)
                    )
                )
            )
        ).map(_.toString)
        
        println(tokenGroups.mkString("\n"))
    }
    
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
                    case BracedText(text) => 
                        text.forall(c => c.isUpperCase)
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
                /*val bracedText = t.tokenParts(0).asInstanceOf[BracedText].text
                val acronymTexts = acronyms.keySet.map(t => t.tokenParts(0).asInstanceOf[TokenString].str)
                
                if(acronymTexts.contains(bracedText)) List()
                else List(t)*/
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
                    case t => t 
                }
            ).filter(t => t match {
                case TokenString(text) => text != ""
                case _ => true
            }))
            
            if(newGroup.tokens.size == 0) List()
            else List(newGroup)
        })
    }
    
    def tokenize(text: StringBuffer) = {
        val tokenList = new ListBuffer[Token]
        
        while(text.length > 0) {
            tokenList += nextToken(text)
        }
        
        Token.compress(tokenList.toList)
    }
    
    def nextToken(text: StringBuffer): Token = {
        text.charAt(0) match {
            case '$' =>
                readFormula(text)
            case '[' =>
                readReference(text)
            case '{' if text.substring(1, 4) == "\\it " => 
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
        
        if(iFrom != (i - iTo)) error("the number of opening and closing dollar signs do not match")
        
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
            val groupBuffer = new ListBuffer[Token]
            val groups = new ListBuffer[TokenGroup]
            
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
    
    object Token {
        def compress(tokens: List[Token]) = {
            val tokenCharBuffer = new ListBuffer[TokenChar]
            val tokenBuffer = new ListBuffer[Token]
            
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
    
    trait Token
    
    case object WhiteSpace extends Token
    case class SeparatorChar(val c: Char) extends Token
    case class TokenChar(val c: Char) extends Token
    case class TokenString(val str: String) extends Token
    case class Reference(val text: String) extends Token {
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
}



















