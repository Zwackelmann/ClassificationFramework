package parser
import scala.collection.mutable.ListBuffer
import format.arff_json.ArffJsonInstance
import scala.util.control._

/**
 * A performance oriented parser for ArffJson
 */
object ArffJsonParser {
    val DENSE_INSTANCE = 0
    val SPARSE_INSTANCE = 1
    
    val loop = new Breaks;
    
    def parseLazy(str: String, numAttributes: Int, debug: Boolean = false) = {
        //if(debug) common.Time(10000) {
        //    parseLazy2(str, numAttributes, debug)
        //} else {
            parseLazy2(str, numAttributes, debug)
        //}
    }
    
    def parseLazy2(str: String, numAttributes: Int, debug: Boolean = false) = {
        var state = 0
        val valueBuffer = new StringBuilder()
        
        var instanceType = DENSE_INSTANCE
        
        var id: String = null
        val mscClasses = new ListBuffer[String]
        
        var pos = 0
        var dataStartPos = 0
        
        loop.breakable {
            for(c <- str) {
                state match {  
                    case 0 => c match { // start of the line => require '['
                        case '[' => state = 1
                        case ' ' => state = 0
                        case _ => state = -1
                    }
                    case 1 => c match { // start of metadata => require second '['
                        case '[' => state = 2
                        case ' ' => state = 1
                        case _ => state = -1
                    }
                    case 2 => c match { // '"' for start of paper id required
                        case '"' => state = 3
                        case ' ' => state = 2
                        case _ => state = -1
                    }
                    case 3 => c match { // sequence of characters other than '"' expected
                        case c if c != '"' => valueBuffer.append(c)
                        case '"' => 
                            id = valueBuffer.toString; 
                            valueBuffer.clear
                            state = 4
                        case _ => state = -1
                    }
                    case 4 => c match { // ',' expected
                        case ',' => state = 5
                        case ' ' => state = 4
                        case _ => state = -1
                    }
                    case 5 => c match { // '[' for start of mscClass list expected (or a ' ')
                        case '[' => state = 6
                        case ' ' => state = 5
                        case _ => state = -1
                    }
                    case 6 => c match { // '"' for start of mscClass expected (or ']' if there is no mscClass set)
                        case '"' => 
                            state = 7
                        case ']' => 
                            state = 9
                        case ' ' => 
                            state = 6
                        case _ => state = -1
                    }
                    case 7 => c match { // content of mscClass expected
                        case c if c != '"' => valueBuffer.append(c)
                        case '"' =>
                            mscClasses.append(valueBuffer.toString)
                            valueBuffer.clear
                            state = 8
                        case _ => state = -1
                    }
                    case 8 => c match { // either the list of mscClasses is over (']') or there is another mscClass (',')
                        case ']' => state = 9
                        case ',' => state = 6
                        case ' ' => state = 8
                        case _ => state = -1
                    }
                    case 9 => c match { // ']' for closing meta-inf list expected
                        case ']' => state = 10
                        case ' ' => state = 9
                        case _ => state = -1
                    }
                    case 10 => c match { // ',' separating metadata from data expected
                        case ',' => state = 11
                        case ' ' => state = 10
                        case _ => state = -1
                    }
                    case 11 => c match { // either '{' for sparse data or '[' for dense data expected
                        case '{' => 
                            dataStartPos = pos
                            instanceType = SPARSE_INSTANCE
                            state = -2
                            loop.break
                        case '[' => 
                            dataStartPos = pos
                            instanceType = DENSE_INSTANCE
                            state = -2
                            loop.break
                        case ' ' => state = 11
                        case _ => state = -1
                    }
                    case -1 => loop.break
                    case -2 => c match {
                        case ' ' =>
                        case _ => state = -1
                    }
                }
                pos += 1
            }
        }
    
    
        pos = str.length()-1
        while(str.charAt(pos) == ' ') pos -= 1
        if(str.charAt(pos) != ']') throw new RuntimeException("No valid ArffJsonInstance")
        pos -= 1
        while(str.charAt(pos) == ' ') pos -= 1
        if((instanceType == SPARSE_INSTANCE && str.charAt(pos) != '}') || (instanceType == DENSE_INSTANCE && str.charAt(pos) != ']')) {
            throw new RuntimeException("No valid ArffJsonInstance")
        }
        val dataEndPos = pos + 1
        
        if(state == -2) {
            if(instanceType == SPARSE_INSTANCE) {
                ArffJsonInstance(
                    id, 
                    mscClasses.toList,
                    str.substring(dataStartPos, dataEndPos),
                    numAttributes,
                    true
                )
            } else if(instanceType == DENSE_INSTANCE) {
                ArffJsonInstance(
                    id,
                    mscClasses.toList,
                    str.substring(dataStartPos, dataEndPos),
                    numAttributes,
                    false
                )
            } else {
                throw new RuntimeException("instanceType is neither DENSE nor SPARSE")
            }
        } else {
            throw new RuntimeException("parsing failed: " + str)
        }
    }
}






























