package common

import scala.collection.mutable
import java.io.File
import java.nio.file.Path

object FileMaker {
    val fileMakerMap = new mutable.HashMap[String, FileMaker]
    
    def isFileMakerSet(path: String) = fileMakerMap.getOrElse(path, None) != None
    
	def registerFileMaker(fm: FileMaker) {
		for((symbol, file) <- fm.makes) {
			FileMaker.fileMakerMap += (file.getCanonicalPath() -> fm)
		}
	}
	
	def apply(needs: Map[Symbol, File], makes: Map[Symbol, File], fun: (Map[Symbol, File]) => Unit) = {
		val fm = new FileMaker(needs, makes) {
			def defineFiles() {
				fun(needs ++ makes)
			}
		}
		FileMaker.registerFileMaker(fm)
		fm
	}
}

abstract class FileMaker(val needs: Map[Symbol, File], val makes: Map[Symbol, File]) {
	def defineFiles()
	
	def make() {
		if(makes.exists(!_._2.exists())) {
			for((sym, file) <- needs) {
				if(!file.exists()) {
				    println(file + " does not exist - call fileMaker")
					val fileMaker = FileMaker.fileMakerMap.getOrElse(
						file.getCanonicalPath(), 
						throw new RuntimeException("No FileMaker for File " + file.getPath() + " gefunden.")
					)
					
					fileMaker.make
				}
			}
			
			defineFiles()
		} else {
		    println("all files: " + makes.map(_._2).mkString(", ") + " allready exist")
		}
	}
}