package formulas.index

import java.io.Reader
import scala.collection.mutable
import java.io.StringReader

trait Index {
    val nodeMap: Map[Int, Index.Node]
    val root: Index.Node
    
    val findNode = nodeMap.apply _
    
    override def toString = "Index(" + root.toString + ")"
}

object Index {
    def apply(r: Reader) = new IntermediateIndex(r).toIndex
    
    class Node(val id: Int, val parent: Option[Node]) {
        var children: List[Node] = _
        def isLeaf = children.isEmpty
        override def toString = id + "(" + children.mkString(", ") + ")"
    }
    
    class IntermediateIndex(r: Reader) {
        val nodeMap = new mutable.HashMap[Int, IntermediateIndexNode]
        readIndex
        
        def toIndex = {
            val childIds = nodeMap.values.map(_.childrenIds).flatten.toSet
            val nodeIds = nodeMap.keySet
            
            val diff = nodeIds -- childIds
            val indexNodeMap = new mutable.HashMap[Int, Node]
            
            def mapIntermediateIndexNode(iNode: IntermediateIndexNode, parent: Option[Node]): Node = {
                val n = new Node(
                    iNode.id, 
                    parent
                )
                indexNodeMap(iNode.id) = n
                
                n.children = iNode.childrenIds.map(i => 
                    mapIntermediateIndexNode(nodeMap(i), Some(n))
                )
                
                n
            }
            
            val r = if(diff.size == 1) {
                val intermediateRootNode = nodeMap(diff.head)
                val rootNode = mapIntermediateIndexNode(intermediateRootNode, None)
                rootNode
            } else {
                if(diff.size > 1) {
                    val rootNode = new Node(-1, None)
                    rootNode.children = diff.toList.map(n => mapIntermediateIndexNode(nodeMap(n), Some(rootNode)))
                    rootNode
                }
                else throw new RuntimeException("There is no root node. (There is no node that exists in no children list)")
            }
            
            new Index {
                val nodeMap = indexNodeMap.toMap
                val root = r
            }
        }
        
        def readIndex {
            var i: Int = 0
            var c: Char = ' '
            var state = 0
            val buffer = new StringBuilder    
            var key: Int = 0
            var childrenIds: mutable.ListBuffer[Int] = new mutable.ListBuffer[Int]
            
            while({ i = r.read(); i != -1 }) {
                c = i.asInstanceOf[Char]
                state match {
                    case 0 => c match {
                        case '{' => state = 1 
                        case c if c.isWhitespace =>
                        case _ => state = -1
                    }
                    case 1 => c match {
                        case c if c.isDigit =>
                            buffer.append(c)
                            state = 2
                        case c if c.isWhitespace => 
                        case _ => state = -1
                    }
                    case 2 => c match {
                        case c if c.isDigit => 
                            buffer.append(c)
                        case c if c.isWhitespace =>
                            key = buffer.toString.toInt
                            buffer.clear
                            state = 3
                        case ':' => 
                            key = buffer.toString.toInt
                            buffer.clear
                            state = 4
                        case _ => state = -1
                    }
                    case 3 => c match {
                        case ':' => state = 4
                        case c if c.isWhitespace =>
                        case _ => state = -1
                    }
                    case 4 => c match {
                        case '[' => state = 5
                        case c if c.isWhitespace =>
                        case _ => state = -1
                    }
                    case 5 => c match {
                        case c if c.isWhitespace =>
                        case d if d.isDigit => 
                            buffer.append(d)
                            state = 6
                        case ']' => 
                            val indexNode = new IntermediateIndexNode(key, childrenIds.toList)
                            nodeMap(key) = indexNode
                            childrenIds.clear
                            state = 9
                        case _ => state = -1
                    }
                    case 6 => c match {
                        case d if d.isDigit => 
                            buffer.append(d)
                        case w if w.isWhitespace =>
                            val childId = buffer.toString.toInt
                            childrenIds.append(childId)
                            buffer.clear
                            state = 7
                        case ',' => 
                            val childId = buffer.toString.toInt
                            childrenIds.append(childId)
                            buffer.clear
                            state = 8
                        case ']' =>
                            val childId = buffer.toString.toInt
                            childrenIds.append(childId)
                            buffer.clear
                            
                            val indexNode = new IntermediateIndexNode(key, childrenIds.toList)
                            nodeMap(key) = indexNode
                            childrenIds.clear
                            
                            state = 9
                        case _ => state = -1
                    }
                    case 7 => c match {
                        case w if w.isWhitespace =>
                        case ',' => state = 8
                        case ']' => 
                            val indexNode = new IntermediateIndexNode(key, childrenIds.toList)
                            nodeMap(key) = indexNode
                            childrenIds.clear
                            state = 9
                        case _ => state = -1
                    }
                    case 8 => c match {
                        case d if d.isDigit =>
                            buffer.append(d)
                            state = 6
                        case w if w.isWhitespace =>
                        case _ => state = -1
                    }
                    case 9 => c match {
                        case w if w.isWhitespace =>
                        case ',' => state = 1
                        case '}' => state = -2
                        case _ => state = -1
                    }
                    case -1 => 
                    case -2 => c match {
                        case w if w.isWhitespace => 
                        case _ => state = -1
                    }
                }
            }
        }
    }
    
    class IntermediateIndexNode(val id: Int, val childrenIds: List[Int]) {
        override def toString = id + "(" + childrenIds.mkString(",") + ")"
    }
}