package filter

import scala.collection.mutable
import java.util.TreeMap
import scala.collection.JavaConversions._
import scala.sys.process.Process._

class NGramTreeBuffer {
    val root: NGrammNodeBuffer = new NGrammNodeBuffer(None)
    val nGramSet = new mutable.HashSet[List[String]]
    
    def +=(nGram: List[String]) {
        nGramSet += nGram
        root.add(nGram, List())
    }
    
    def setIndexes = {
        val queue = new mutable.ListBuffer[NGrammNodeBuffer]
        queue += root
        var currentId = 0
        val sortedNGrammList = new mutable.ListBuffer[List[String]]
        
        while(!queue.isEmpty) {
            val first = queue.remove(0)
            if(first.nodeLabel.isDefined && nGramSet.contains(first.nodeLabel.get)) {
                sortedNGrammList += first.nodeLabel.get
                
                first.id = Some(currentId)
                currentId += 1
            } else {
                first.children.values ++=: queue
            }
        }
        
        sortedNGrammList.toList
    }
    
    def toNGrammTree: NGramTree = {
        val sortedNGramList = setIndexes
        new NGramTree(root.toNGrammNode, nGramSet.toSet, sortedNGramList)
    }
}

class NGrammNodeBuffer(val nodeLabel: Option[List[String]]) {
    val children = new TreeMap[String, NGrammNodeBuffer]()
    var id: Option[Int] = None
    
    def add(nGram: List[String], path: List[String]) {
        if(!nGram.isEmpty) {
            val child = children.get(nGram.head) match {
                case null => 
                    val newChild = new NGrammNodeBuffer(Some(path :+ nGram.head))
                    children.put(nGram.head, newChild)
                    newChild
                case c => c
            }
            
            child.add(nGram.tail, path :+ nGram.head)
        }
    }
    
    def toNGrammNode: NGramNode = new NGramNode(children.toMap.map(c => c._1 -> c._2.toNGrammNode), id)
}

@serializable
class NGramTree(root: NGramNode, val nGramSet: Set[List[String]], val sortedNGramList: List[List[String]]) {
    def apply(nGram: Seq[String]) = root(nGram)
}

@serializable
class NGramNode(val children: Map[String, NGramNode], val id: Option[Int]) {
    def apply(nGram: Seq[String]): Option[Int] = {
        if(nGram.isEmpty || id.isDefined) id
        else children.get(nGram.head) match {
            case Some(node) => node(nGram.tail)
            case _ => None
        }
    }
}








