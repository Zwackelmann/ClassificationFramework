package script

import scala.collection.JavaConversions._
import java.util.TreeMap
import scala.collection.mutable

object Test6 {
    class NGrammTreeBuffer {
        val children = new TreeMap[String, NGrammTreeBuffer]()
        var id: Option[Int] = None
        
        def +=(nGram: List[String]) {
            if(!nGram.isEmpty) {
                val child = children.get(nGram.head) match {
                    case null => 
                        val newChild = new NGrammTreeBuffer()
                        children.put(nGram.head, newChild)
                        newChild
                    case c => c
                }
                
                child += nGram.tail
            }
        }
        
        def setIndexes {
            val queue = new mutable.ListBuffer[NGrammTreeBuffer]
            queue ++= this.children.values
            var currentId = 0
            
            while(!queue.isEmpty) {
                val first = queue.remove(0)
                
                if(first.children.isEmpty()) {
                    first.id = Some(currentId)
                    currentId += 1
                } else {
                    first.children.values ++=: queue
                }
            }
        }
        
        def toNGrammTree: NGrammTree = {
            setIndexes
            _toNGrammTree
        }
        
        private def _toNGrammTree: NGrammTree = new NGrammTree(children.toMap.map(c => c._1 -> c._2._toNGrammTree), id)
    }
    
    class NGrammTree(val children: Map[String, NGrammTree], val id: Option[Int]) {
        def apply(nGramm: List[String]): Option[Int] = {
            if(nGramm.isEmpty || id.isDefined) id
            else children.get(nGramm.head) match {
                case Some(tree) => tree(nGramm.tail)
                case _ => None
            }
        }
    }
    
    def main(args: Array[String]) {
        val buffer = new NGrammTreeBuffer
        
        buffer += List("a", "b", "c")
        buffer += List("a", "b", "d")
        buffer += List("a", "c", "c")
        buffer += List("b", "d")
        
        val tree = buffer.toNGrammTree
        
        println(tree(List("a", "b", "c", "d")))
    }
}



















