package conversion

import java.io.InputStreamReader
import java.io.FileInputStream
import java.lang.RuntimeException
import scala.collection.mutable.ListBuffer
import common.WriteFile._
import common.{ErrorLog, NoticeEvery}
import parser.SelfMadeParser
import net.sf.json.JSONObject;
import net.sf.json.JSONSerializer;


object SelfMadeToJSON {
    val inFile = "data/raw/deliver-math.txt"
    val outFile = "data/json/deliver-math-raw.json"
    
    val logFile = "log/selfMadeToJsonErrors.log"
    
    def main(args: Array[String]) {
        var noticeEvery = new NoticeEvery(10000)
        val errorLog = new ErrorLog[List[String]](noticeEveryXErrors = Some(100), filename = Some(logFile)) {
            override def tToString(t: List[String]) = t.mkString("\n")
        }
        val parser = new SelfMadeParser(inFile)

        writeFile(outFile) { out =>
            def read() {
                val lines = parser.nextLines

                try {
                    val paper = parser.readPaper(lines)
                    out.write(paper.toJson.toString() + "\n")
                } catch {
                    case re: RuntimeException =>
                        errorLog.log(lines.toList, re)
                }

                noticeEvery.count

                if (parser.hasNext) {
                    read()
                }
            }

            read()

        }
        
        errorLog.logErrors
    }
}