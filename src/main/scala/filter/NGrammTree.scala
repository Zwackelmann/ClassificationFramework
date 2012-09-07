package filter

import scala.collection.mutable
import java.util.TreeMap
import scala.collection.JavaConversions._
import scala.sys.process.Process._

class NGrammTreeBuffer {
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
    
    def toNGrammTree: NGrammTree = {
        val sortedNGramList = setIndexes
        new NGrammTree(root.toNGrammNode, nGramSet.toSet, sortedNGramList)
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
    
    def toNGrammNode: NGrammNode = new NGrammNode(children.toMap.map(c => c._1 -> c._2.toNGrammNode), id)
}

class NGrammTree(root: NGrammNode, val nGrammSet: Set[List[String]], val sortedNGrammList: List[List[String]]) {
    def apply(nGramm: Seq[String]) = root(nGramm)
}

class NGrammNode(val children: Map[String, NGrammNode], val id: Option[Int]) {
    def apply(nGramm: Seq[String]): Option[Int] = {
        if(nGramm.isEmpty || id.isDefined) id
        else children.get(nGramm.head) match {
            case Some(node) => node(nGramm.tail)
            case _ => None
        }
    }
}








