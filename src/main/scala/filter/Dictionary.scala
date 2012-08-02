package filter
import scala.collection.mutable.HashMap
import java.util.TreeSet
import java.util.{Set => JavaSet}
import scala.collection.JavaConversions._

@serializable
abstract class Dictionary extends Iterable[String] {
    def wordFun(word: String): String = word
    def wordCond(word: String): Boolean = true
    val stopList: Seq[String] = List()
    val minWordCount = 3
    
    var dirty = true
    @transient lazy val _dict: JavaSet[String] = new TreeSet[String]()
    @transient lazy val _wordToIndexMap = new HashMap[String, Int]
    val dictCount = new HashMap[String, Int]
    
    def iterator = dict.iterator
    
    def dict = if(dirty) {
        _dict.clear()
        _wordToIndexMap.clear()
        
        _dict ++= dictCount.filter(w => w._2 > minWordCount).map(_._1)
        for((word, i) <- (_dict.zipWithIndex)) {
            _wordToIndexMap.put(word, i)
        }
        
        dirty = false
        _dict
    } else {
        _dict
    }
    
    def wordIndex(word: String) = _wordToIndexMap.get(wordFun(word))
    
    def add(word: String) {
        val w = wordFun(word)
        if(w != "" && !stopList.contains(w) && wordCond(w)) {
            dictCount(w) = dictCount.getOrElse(w, 0) + 1
        }
        
        dirty = true
    }
}












