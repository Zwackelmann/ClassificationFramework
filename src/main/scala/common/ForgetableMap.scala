package common

import scala.collection.mutable._
import scala.collection.generic._
import scala.collection.parallel.mutable.ParHashMap
import scala.collection.CustomParallelizable

class ForgetableMap[A, B]
extends Map[A, B]
   with MapLike[A, B, ForgetableMap[A, B]]
   with HashTable[A, DefaultEntry[A, B]]
   with CustomParallelizable[(A, B), ParHashMap[A, B]]
   with Serializable
{
  val maxSize: Int = -1
  val recentKeys = new Queue[A]
  type Entry = DefaultEntry[A, B]

  override def empty: ForgetableMap[A, B] = ForgetableMap.empty[A, B]
  override def clear() = clearTable()
  override def size: Int = tableSize

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }
  
  def prune() {
      while(maxSize != -1 && recentKeys.size > maxSize) {
          val keyToRemove = recentKeys.dequeue()
          removeEntry(keyToRemove)
      }
  }

  override def put(key: A, value: B): Option[B] = {
    val e = findEntry(key)
    recentKeys += key
    prune()
    if (e == null) { addEntry(new Entry(key, value)); None }
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: A, value: B): Unit = put(key, value)

  override def remove(key: A): Option[B] = throw new RuntimeException("not implemented")

  def += (kv: (A, B)): this.type = {
    val e = findEntry(kv._1)
    recentKeys += kv._1
    prune()
    
    if (e == null) addEntry(new Entry(kv._1, kv._2))
    else e.value = kv._2
    this
  }

  def -=(key: A): this.type = throw new RuntimeException("not implemented")

  def iterator = entriesIterator map {e => (e.key, e.value)}

  override def foreach[C](f: ((A, B)) => C): Unit = foreachEntry(e => f(e.key, e.value))

  override def keySet: collection.Set[A] = new DefaultKeySet {
    override def foreach[C](f: A => C) = foreachEntry(e => f(e.key))
  }

  override def values: collection.Iterable[B] = new DefaultValuesIterable {
    override def foreach[C](f: B => C) = foreachEntry(e => f(e.value))
  }

  override def keysIterator: Iterator[A] = new Iterator[A] {
    val iter = entriesIterator
    def hasNext = iter.hasNext
    def next() = iter.next.key
  }

  override def valuesIterator: Iterator[B] = new Iterator[B] {
    val iter = entriesIterator
    def hasNext = iter.hasNext
    def next() = iter.next.value
  }
}

object ForgetableMap extends MutableMapFactory[ForgetableMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ForgetableMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: ForgetableMap[A, B] = new ForgetableMap[A, B]
}