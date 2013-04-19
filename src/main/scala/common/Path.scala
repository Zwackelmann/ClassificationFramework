package common
import java.io.File

object Path {
    var rootFolder = "data"
    def rootPath = new File(rootFolder).getCanonicalPath()
    
    val arffJsonPath = new Path("arffJson") !
    val filterPath = new Path("filter") !
    val classifierPath = new Path("classifier") !
    val resultsPath = new Path("results") !
    val thresholdsPath = new Path("thresholds") !
    val tuningPath = new Path("tuning") !
    
    def join(path1: String, path2: String, separator: String): String = {
        if(path1.substring(path1.length()-separator.length, path1.length()) == separator) {
            path1.substring(0, path1.length()-separator.length)
        } else {
            path1
        }
        
        path1 + separator + path2
    }
    
    def join(path1: String, path2: String): String = {
        val sep = separator(path1 + path2)
        join(path1, path2, sep)
    }
    
    def separator(path: String) = {
        if(path.contains('\\') && !path.contains('/'))
            "\\"
        else if(path.contains('/') && !path.contains('\\')) 
            "/"
        else 
            File.separator
    }
    
    implicit def pathToFilename(path: Path) = path.fullPath
}

@serializable
class Path(val relativePath: String) {
    import Path._
    
    def file = new File(fullPath())
    
    def fullPath() = {
        // TODO the separator symbol should be replaced here with respect to the systems separator char
        // for that I should first explore if there are other separator chars than "/" and "\" that should
        // be replaced with File.separator 
        val sep = separator(rootPath + relativePath)
        join(rootPath, relativePath, sep)
    }
    
    def /(pathToAppend: String) = {
        val sep = separator(fullPath)
        val newPath = join(relativePath, pathToAppend, sep)
        
        new Path(newPath)
    }
    
    def require() {
        val file = new File(fullPath)
        if(!file.exists()) {
            file.mkdir()
        }
    }
    
    def ! = {
        require
        this
    }
    
    override def toString = "Path(" + relativePath + ")"
}







