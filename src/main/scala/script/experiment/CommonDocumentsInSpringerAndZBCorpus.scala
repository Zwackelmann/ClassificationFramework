package script.experiment

import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import scala.collection.mutable
import scala.io.Source
import parser.ArffJsonInstancesSource

object CommonDocumentsInSpringerAndZBCorpus {
	def main(args: Array[String]) {
		val titlesInSpringer = new mutable.ListBuffer[String]

		val springerTitles = Source
			.fromFile("modules/GatherSpringerData/hit_logger")
			.getLines()
			.filter(line => line.length > 7 && line.substring(0, 7) == "TI:    ")
			.map(line => unifyTitleRepresentation(line.substring(7, line.length)))
			.toSet

		val mscTitles = ArffJsonInstancesSource("/home/simon/Simon Backup/Projekte/research/ZBL Full Corpus/fiz_full.json")
			.map(inst => 
				unifyTitleRepresentation(inst.dataAt(0).asInstanceOf[String])
			)
			.toSet
			
		val intersectTitles = (springerTitles intersect mscTitles)
		println("num MSC Titles: " + mscTitles.size)
		println("num Sptinger Titles: " + springerTitles.size)
		println("num intersect Titles: " + intersectTitles.size)
	}

	val unifyTitleRepresentation = (title: String) => title.filter(_.isLetterOrDigit).toLowerCase
}