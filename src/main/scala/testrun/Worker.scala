package testrun
import scala.actors.Actor
import scala.actors.remote.RemoteActor


/*object Worker {
    val hostIp = "127.0.0.1"
    val port = Delegator.port
    
    val worker = new Worker
    
    RemoteActor.classLoader = getClass().getClassLoader
    
    def main(args: Array[String]) {
        worker.start
    }
}*/

/*class Worker extends Actor {
    import Worker._
    val id = Symbol("rftg-client-" + (math.random*Int.MaxValue.round).asInstanceOf[Int])
    val port = (math.random*2000+9000).asInstanceOf[Int]
    /// classLoader = getClass().getClassLoader
}*/