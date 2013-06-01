package external

import scala.collection.mutable
import java.io.File
import java.util.Properties
import java.io.FileInputStream

object ExternalAlgorithmApplier {
    val commands = new Properties()
    val is = new FileInputStream("util/commands.properties")
    commands.load(is)
    is.close()
    
    def command(commandKey: String) = commands.getProperty(commandKey)
}

class ExternalAlgorithmApplier(val commandKey: String) {
    import ExternalAlgorithmApplier._
    def command = commands.getProperty(commandKey)
}
