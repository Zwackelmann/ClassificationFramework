package external

import scala.collection.mutable

object MyProcess {
    import scala.sys.process._
    
    private val processes = new mutable.ListBuffer[Process]
    
    def apply(cmd: Seq[String]): Process = {
        apply(cmd.mkString(" "))
    }
    
    def apply(cmd: String): Process = {
        val p = cmd.run
        processes += p
        p
    }
    
    Runtime.getRuntime().addShutdownHook(
        new Thread() {
            override def run() {
                for(p <- processes) {
                    try {
                        p.destroy()
                    } catch {
                        case _: Throwable => 
                    }
                }
            }
        }
    )
    
    def init() {
        (new Thread() {
            override def run() {
                try {
                    System.in.read();
                } finally {
                    System.exit(0);
                }
            }
        }).start()
    }
}