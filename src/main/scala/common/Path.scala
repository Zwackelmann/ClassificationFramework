package common
import java.io.File

object Path {
    val rootPath = new File("data").getCanonicalPath()
    val arffJsonPath = new Path("arffJson") !
    val filterPath = new Path("filter") !
    val classifierPath = new Path("classifier") !
    val resultsPath = new Path("results") !
    
    def join(path1: String, path2: String, separator: String) = {
        if(path1.substring(path1.length()-separator.length, path1.length()) == separator) {
            path1.substring(0, path1.length()-separator.length)
        } else {
            path1
        }
        
        path1 + separator + path2
    }
    
    def separator(path: String) = {
        if(path.contains('\\') && !path.contains('/'))
            "\\"
        else if(path.contains('/') && !path.contains('\\')) 
            "/"
        else 
            File.separator
    }
    
    implicit def pathToFile(path: Path) = new File(path.fullPath)
}

@serializable
class Path(val relativePath: String) {
    import Path._
    
    def file = new File(fullPath())
    
    def fullPath() = {
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







