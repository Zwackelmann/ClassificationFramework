package script
import java.io.File

class RenamingScript {
    def main(args: Array[String]) {
        val resdir = new File("data/results")
        
        for(file <- resdir.listFiles()) {
            val newName = file.getName().replaceAll("conf9", "conf9-min-10")
            file.renameTo(new File("data/results/" + newName))
        }
    }
}