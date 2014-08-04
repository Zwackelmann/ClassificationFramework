package script.experiment

import parser.ArffJsonInstancesSource
import scala.collection.mutable
import java.io.File
import common.FileManager

object JaccardMatrix {
	def main(args: Array[String]) {
		val corpus = ArffJsonInstancesSource("/home/simon/Projekte/ClassificationFramework/data/arffJson/corpus.json")
		val matrix = jaccardMatrix(
		    corpus.map(inst => {
		        inst.categories.flatMap(c => if(c.length >= 2) List(c.substring(0, 2)) else List())
		            .toList
		            .distinct
		    })
		)
        
		saveMatrixToFile(matrix, new File("jaccard_matrix2"))
		/*val normedMatrix = normalize(loadMatrixFromFile(new File("jaccard_matrix2")))

		val keys = normedMatrix.keySet.toList.sortBy(key => key).toList
		for(key <- keys) {
			println("\n" + key + ":")
			val similaritiesForCategories = normedMatrix(key).toList
			println(similaritiesForCategories.filter(s => s._2 > 0.005 && s._1 != key).sortWith((a, b) => a._2 > b._2).mkString("\n"))
			println("Total: " + (similaritiesForCategories.map(_._2).sum - 1.0)) // -1.0, weil die kategorie selbst auch immernoch dabei ist (und die Similarity ist immer 1.0)
		}*/
	}

	def jaccardMatrix[T](observations: Iterable[Iterable[T]]) = {
		val coocMatrix = new mutable.HashMap[T, mutable.HashMap[T, Int]] {
			override def default(key: T) = new mutable.HashMap[T, Int] {
				override def default(key: T) = 0
			}
		}

		for(observation <- observations) {
			for(o1 <- observation; o2 <- observation) {
				val map = coocMatrix(o1)
				map(o2) = map(o2) + 1
				coocMatrix(o1) = map
			}
		}

		normalizeMatrix(coocMatrix)
	}

	def normalizeMatrix[T](matrix: mutable.HashMap[T, mutable.HashMap[T, Int]]) = {
		val normedMatrix = new mutable.HashMap[T, mutable.HashMap[T, Double]] {
			override def default(key: T) = new mutable.HashMap[T, Double] {
				override def default(key: T) = 0
			}
		}

		val keys = matrix.keySet

		for(i <- keys) {
			val mapI = normedMatrix(i)
			for(j <- keys) {
				mapI(j) = matrix(i)(j).toDouble / (matrix(i)(i) + matrix(j)(j) - matrix(i)(j)).toDouble
			}
			normedMatrix(i) = mapI
		}
		normedMatrix
	}

	def saveMatrixToFile(matrix: mutable.HashMap[String, mutable.HashMap[String, Double]], file: File) {
		val keys = matrix.keySet.toList.sortBy(key => key).toArray
		val data = (for(i <- keys) yield (for(j <- keys) yield matrix(i)(j)).toArray).toArray

		common.ObjectToFile.writeObjectToFile((keys, data), file)
	}

	def loadMatrixFromFile(file: File) = {
		val (keys, data) = common.ObjectToFile.readObjectFromFile(file).asInstanceOf[Pair[Array[String], Array[Array[Double]]]]

		val coocMatrix = new mutable.HashMap[String, mutable.HashMap[String, Double]] {
			override def default(key: String) = new mutable.HashMap[String, Double] {
				override def default(key: String) = 0
			}
		}

		for((key1, i) <- keys.zipWithIndex) {
			for((key2, j) <- keys.zipWithIndex) {
				if(data(i)(j) != 0) {
					val map = coocMatrix(key1)
					map(key2) = data(i)(j)
					coocMatrix(key1) = map
				}
			}
		}

		coocMatrix
	}
}