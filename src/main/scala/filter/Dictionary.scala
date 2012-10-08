package filter
import scala.collection.mutable.HashMap
import java.util.TreeSet
import java.util.{Set => JavaSet}
import scala.collection.JavaConversions._

object Dictionary {
    class DirtyFlag(var isDirty: Boolean = true)
}

@serializable
abstract class Dictionary extends Iterable[String] {
    import Dictionary._
    
    def wordFun(word: String): String = word
    def wordCond(word: String): Boolean = true
    val stopList: Seq[String] = List()
    val minWordCount = 10
    
    @transient lazy val dirtyFlag = new DirtyFlag
    @transient lazy val _dict: JavaSet[String] = new TreeSet[String]()
    @transient lazy val _wordToIndexMap = new HashMap[String, Int]
    @transient lazy val _indexToWordMap = new HashMap[Int, String]
    val dictCount = new HashMap[String, Int]
    
    def iterator = dict.iterator
    
    def update {
        if(dirtyFlag.isDirty) {
            _dict.clear()
            _wordToIndexMap.clear()
            _indexToWordMap.clear()
            
            _dict ++= dictCount.filter(w => w._2 > minWordCount).map(_._1)
            for((word, i) <- (_dict.zipWithIndex)) {
                _wordToIndexMap.put(word, i)
                _indexToWordMap.put(i, word)
            }
            
            dirtyFlag.isDirty = false
            _dict
        }
    }
    
    def dict = {
        update
        _dict
    }
    
    def wordFromIndex(index: Int) = {
        update
        _indexToWordMap(index)
    }
    
    def wordIndex(word: String) = {
        update
        _wordToIndexMap.get(wordFun(word))
    }
    
    def add(word: String) {
        val w = wordFun(word)
        if(w != "" && !stopList.contains(w) && wordCond(w)) {
            dictCount(w) = dictCount.getOrElse(w, 0) + 1
        }
        
        dirtyFlag.isDirty = true
    }
}












