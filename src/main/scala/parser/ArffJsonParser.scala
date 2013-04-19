package parser
import scala.collection.mutable.ListBuffer
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import format.arff_json.ArffJsonInstance

/**
 * A performance oriented parser for ArffJson
 */
object ArffJsonParser {
    val DENSE_INSTANCE = 0
    val SPARSE_INSTANCE = 1
    
    def parse(str: String, numAttributes: Int) = {
        var state = 0
        val keyBuffer = new StringBuilder()
        val valueBuffer = new StringBuilder()
        
        var instanceType = DENSE_INSTANCE
        var minus = false
        
        var id: String = null
        val mscClasses = new ListBuffer[String]
        val listValues = new ListBuffer[Any]
        val mapValues = new ListBuffer[Pair[Int, Any]]
        
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
                case 6 => c match { // '"' for start of mscClass expected (or ']' if there is no mscClass set
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
                        instanceType = SPARSE_INSTANCE
                        state = 12
                    case '[' => 
                        instanceType = DENSE_INSTANCE
                        state = 20
                    case ' ' => state = 11
                    case _ => state = -1
                }
                case 12 => c match { // handle sparse data => expect a key => expect '"' (or '}' for empty data)
                    case '"' => 
                        state = 13
                    case '}' =>
                        state = 18
                    case ' ' => state = 12
                    case _ => state = -1
                }
                case 13 => c match { // read key
                    case c if c != '"' => keyBuffer.append(c)
                    case '"' => state = 14
                }
                case 14 => c match { // ':' separating key from value expected
                    case ':' => state = 15
                    case ' ' => state = 14
                    case _ => state = -1
                }
                case 15 => c match { // value expected => either '"' for a string value or a number for a double value
                    case '"' => 
                        state = 16
                    case c if c.isDigit => 
                        valueBuffer.append(c)
                        state = 19
                    case '-' => 
                        valueBuffer.append(c)
                        state = 19
                    case '+' => 
                        valueBuffer.append(c)
                        state = 19
                    case ' ' => state = 15
                }
                case 16 => c match { // expect a char for string value
                    case '\\' => 
                        state = 162
                    case c if c != '"' => valueBuffer.append(c)
                        println(c)
                    case '"' => // key-value pair found
                        mapValues += Pair(keyBuffer.toString.toInt, valueBuffer.toString)
                        valueBuffer.clear
                        keyBuffer.clear
                        state = 17
                }
                case 162 => 
                    valueBuffer.append(c)
                    state = 16
                case 17 => c match { // ',' if there are more key-value pairs or '}' if the list is at end
                    case ',' => state = 12
                    case '}' => state = 18
                    case ' ' => state = 17
                    case _ => state = -1
                }
                case 18 => c match { // data read => expect last ']'
                    case ']' => state = -2
                    case ' ' => state = 18
                    case _ => state = -1
                }
                case 19 => c match { // a double-value was read => expect anything but a ',' or a '}'
                    case c if c != ',' && c != '}' => valueBuffer.append(c)
                    case ',' => 
                        mapValues += Pair(keyBuffer.toString.toInt, valueBuffer.toString.toDouble)
                        valueBuffer.clear
                        keyBuffer.clear
                        state = 12
                    case '}' => 
                        mapValues += Pair(keyBuffer.toString.toInt, valueBuffer.toString.toDouble)
                        valueBuffer.clear
                        keyBuffer.clear
                        state = 18
                }
                case 20 => c match { // reading dense data => expect a '"' for a string value or a digit for a double value
                    case '"' => state = 21
                    case c => 
                        valueBuffer.append(c)
                        state = 23
                }
                case 21 => c match {
                    case '\\' => 
                        state = 212
                    case c if c != '"' => 
                        valueBuffer.append(c)
                    case '"' => 
                        listValues += valueBuffer.toString()
                        valueBuffer.clear
                        state = 22
                }
                case 212 => 
                    valueBuffer.append(c)
                    state = 21
                case 22 => c match { // expect ']' if list is at end or ',' if there is more data
                    case ']' => state = 18
                    case ',' => state = 20
                    case ' ' => state = 22
                }
                case 23 => c match { // a double-value was read => expect anything but a ',' or a ']'
                    case c if c != ',' && c != ']' => valueBuffer.append(c)
                    case ',' => 
                        listValues += valueBuffer.toString.toDouble
                        valueBuffer.clear
                        state = 20
                    case ']' => 
                        listValues += valueBuffer.toString.toDouble
                        valueBuffer.clear
                        state = 18
                }
                case -1 =>
            }
        }
        
        if(state == -2) {
            if(instanceType == SPARSE_INSTANCE) {
                new SparseArffJsonInstance(
                    id, 
                    mscClasses.toList,
                    mapValues.toMap,
                    numAttributes
                )
            } else if(instanceType == DENSE_INSTANCE) {
                new DenseArffJsonInstance(
                    id,
                    mscClasses.toList,
                    listValues.toList
                )
            } else {
                throw new RuntimeException("instanceType is neither DENSE nor SPARSE")
            }
        } else {
            throw new RuntimeException("parsing failed: " + str)
        }
    }
}



