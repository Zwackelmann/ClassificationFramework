package common


import net.sf.json.JSONObject
import net.sf.json.JSONArray
import scala.collection.JavaConversions._
import java.io.BufferedReader
import java.io.File
import java.io.FileReader

/**
 * A collection of some commonly used functions
 */
object Common {
    /**
     * Escapes a string with special characters to prepare it to be written to a file
     */
    def escape(s: String) = s.replaceAll("\\\\", "\\\\\\\\").replaceAll("'", "\\\\'").replaceAll("\"", "\\\\\"")
    
    /**
     * returns the file extension of a file name (the string after the last dot)
     */
    def getFileExtension(filename: String) = {
        val i = filename.lastIndexOf('.')
        
        if(i != -1)
            filename.substring(i + 1, filename.length())
        else 
            ""
    }
    
    def getFilenameWithoutExtension(filename: String) = {
        val i = filename.lastIndexOf('.')
        
        if(i != -1)
            filename.substring(0, i)
        else 
            filename
    }
    
    /**
     * Converts an arbitrary JSON Object to the respective Scala-Version. <br>
     * <ul>
     *   <li>JSON Object becomes scala.collection.immutable.Map
     *   <li>JSON Array becomes scala.collection.immutable.List
     *   <li>JSON String becomes java.lang.String
     *   <li>JSON Double becomes scala.Double
     *   <li>JSON Int becomes scala.Int
     * </ul>
     */
    def jsonToScalaType(a: Any): Any = {
        a match {
            case arr: JSONArray => (for(i <- 0 until arr.size) yield jsonToScalaType(arr.get(i))).toList
            case obj: JSONObject => {
                val keyIter = obj.keySet().iterator().asInstanceOf[java.util.Iterator[String]]
                
                (for(key <- keyIter) yield (key -> jsonToScalaType(obj.get(key)))).toMap
            }
            case s: String => s
            case i: Int => i
            case d: Double => d
            case _ => throw new RuntimeException()
        }
    }
    
    def randomStream(): Stream[Double] = Stream.cons(math.random, randomStream())
    
    object FileConversion {
        def lines(reader: BufferedReader) = new Iterator[String] {
            var buffer: String = null

            def bufferNext() = {
                buffer = reader.readLine()
                buffer
            }
            
            def hasNext = !(buffer == null && bufferNext() == null)
            
            def next() = if(hasNext) {
                val b = buffer
                buffer = null
                b
            } else {
                reader.close
                null
            }
        }
        
        class FancyFile(val file: File) {
            def lines = new Iterable[String] {
                def iterator = FileConversion.lines(new BufferedReader(new FileReader(file))) 
            }
        }
        implicit def file2FancyFile(file: File) = new FancyFile(file)
        
        
        class FancyBufferedReader(val reader: BufferedReader) {
            def lines = FileConversion.lines(reader)
        }
        implicit def bufferedReader2FancyBufferedReader(reader: BufferedReader) = new FancyBufferedReader(reader)
    }
    
    def resolvePath(baseFile: File, path: String) = {
        val separator = {
            if(path.contains('\\') && !path.contains('/'))
                "\\"
            else if(path.contains('/') && !path.contains('\\')) 
                "/"
            else 
                File.separator
        }
        
        val baseFilePath = {
            val x = baseFile.getCanonicalPath()
            if(x.substring(x.length()-separator.length, x.length()) == separator) {
                x.substring(0, x.length()-separator.length)
            } else {
                x
            }
        }
        
        val newPath = baseFilePath + separator + path
        
        new File(newPath)
    }
    
    import FileConversion._
    def topClasses = new File("data/util/top_classes.txt").lines.toList
}











