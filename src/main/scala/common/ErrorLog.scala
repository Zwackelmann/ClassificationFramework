package common
import scala.collection.mutable.ListBuffer
import WriteFile._

class ErrorLog[T](val name: String = "ErrorLog", val noticeEveryXErrors: Option[Int] = None, val filename: Option[String] = None) {
    val errors = new ListBuffer[Pair[T, Throwable]]
    
    def log(obj: T, ex: Throwable) {
        errors += Pair(obj, ex)
        
        noticeEveryXErrors match {
            case Some(n) => if(errors.size % n == 0) {
                println(name + ": " + errors.size + " Errors")
            }
            case None => 
        }
    }
    
    def tToString(t: T) = t.toString()
    
    def logErrors {
        filename match {
            case Some(filename) => writeFile(filename) { out => 
                for(error <- errors) {
                    out.write("Error in Element:\n" + tToString(error._1) + "\n\n")
                    out.write("Error message:\n" + error._2.getMessage() + "\n" + error._2.getStackTrace().mkString("\n") + "\n\n\n")
                }
                out.write("Total Errors: " + errors.size)
            }
            case None => 
                for(error <- errors) {
                    println("Error in Element:\n" + tToString(error._1) + "\n")
                    println("Error message:\n" + error._2.getMessage() + "\n" + error._2.getStackTrace().mkString("\n") + "\n\n")
                }
                println("Total Errors: " + errors.size)
        }
    }
}




