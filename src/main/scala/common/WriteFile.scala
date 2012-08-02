package common
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.File

object WriteFile {
    def writeFile(file: File)(fun: BufferedWriter => Unit): Unit = {
        val fstream = new FileWriter(file)
        val out = new BufferedWriter(fstream)
        
        fun(out)
        out.close
    }
    
    def writeFile(filename: String)(fun: BufferedWriter => Unit): Unit = writeFile(new File(filename))(fun)
}