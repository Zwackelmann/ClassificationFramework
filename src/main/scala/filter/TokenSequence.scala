package filter
import scala.collection.mutable

object TokenSequence {
    def apply(text: String) = new TokenSequence(text)
}

trait Token
case object WhiteSpace extends Token
case class Formula(val content: String) extends Token {
    override def toString = "$" + content + "$"
}
case class Reference(val text: String) extends Token {
    val zblRe = """.*Zbl ([0-9]+\.[0-9]+).*""".r
    
    override def toString = {
        "[" + (text match {
            case zblRe(nr) => nr
            case x => x
        }) + "]"
    }
}
case class SimpleToken(val text: String) extends Token
case class SeparatorChar(val char: Char) extends Token
case class Author(val text: String) extends Token {
    def split = text.split(", ").toList.map(t => Author(t))
    
    override def toString = "{" + text + "}"
}

class TokenSequence(val text: String, separators: List[Char] = List('.', ':', ';', ','), specialCharacters: List[Char] = List('$', '[', '{'), additionalWordChars: List[Char] = List('-')) extends Iterator[Token] {
    val stringBuffer = new StringBuffer()
    stringBuffer.append(text)
    
    val tokenBuffer: mutable.Queue[Token] = new mutable.Queue[Token]
    
    def next: Token = {
        if(!tokenBuffer.isEmpty) tokenBuffer.dequeue
        else {
            val t = bufferNext
            if(t != null) t
            else throw new IndexOutOfBoundsException("there is no next token")
        }
    }
    
    def hasNext: Boolean = {
        if(!tokenBuffer.isEmpty) true
        else {
            val t = bufferNext
            if(t == null) false
            else {
                tokenBuffer.enqueue(t)
                true
            }
        }
    }
    
    def bufferNext: Token = {
        while(stringBuffer.length > 0 && !{
            val c = stringBuffer.charAt(0)
            (c.isWhitespace || c.isLetterOrDigit || separators.contains(c) || specialCharacters.contains(c))
        }) stringBuffer.deleteCharAt(0)
        
        if(stringBuffer.length > 0) {
            stringBuffer.charAt(0) match {
                case '$' =>
                    try {
                        readFormula(stringBuffer)
                    } catch {
                        case _: Throwable => 
                            stringBuffer.deleteCharAt(0)
                            bufferNext
                    }
                case '[' =>
                    try {
                        readReference(stringBuffer)
                    } catch {
                        case _: Throwable => 
                            stringBuffer.deleteCharAt(0)
                            bufferNext
                    }
                case '{' if stringBuffer.length > 4 && stringBuffer.substring(1, 5) == "\\it " => {
                    try {
                        readAuthor(stringBuffer)
                    } catch {
                        case _: Throwable => 
                            stringBuffer.deleteCharAt(0)
                            bufferNext
                    }
                }
                case c if separators.contains(c) =>
                    val char = new SeparatorChar(c)
                    stringBuffer.deleteCharAt(0)
                    char
                case c if c.isWhitespace => 
                    readWhitespace(stringBuffer)
                case c => 
                    readSimpleToken(stringBuffer)
            }
        } else {
            null
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
    
    def readWhitespace(text: StringBuffer) = {
        while(text.length() > 0 && text.charAt(0).isWhitespace) text.deleteCharAt(0)
        WhiteSpace
    }
    
    def readSimpleToken(text: StringBuffer) = {
        var i = 1
        val l = text.length
        while(i < l && { val c = text.charAt(i); c.isLetterOrDigit || additionalWordChars.contains(c) }) i += 1
        
        val t = SimpleToken(text.substring(0, i))
        text.delete(0, i)
        t
    }
}








