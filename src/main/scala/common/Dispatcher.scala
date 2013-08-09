package common

import scala.actors.Actor
import scala.collection.mutable.Queue
import scala.actors.OutputChannel

object Dispatcher {
    case class Task(val fun: () => Unit)
    case object GiveMeWork
    case object Quit
    case object OffDuty
    
    class Worker(id: Int) extends Actor {
        var quitSender: Option[OutputChannel[Any]] = None
        var processTask: Boolean = false
        
        def act() {
            loop {
                receive {
                    case t: Task =>
                        processTask = true
                        t.fun()
                        processTask = false
                        if(quitSender.isDefined) {
                            quitSender.get ! OffDuty
                            exit()
                        } else {
                            sender ! GiveMeWork
                        }
                    case Quit =>
                        if(!processTask) {
                            sender ! OffDuty
                            exit()
                        } else {
                            quitSender = Some(sender)
                        }
                }
            }
        }
    }
}

class Dispatcher(val numWorkers: Int) extends Actor {
    import Dispatcher._
    
    def act() {
        val workers = (1 to numWorkers).map(id => (new Worker(id)).start())
        val waitingWorkers = new Queue[OutputChannel[Any]]()
        for(worker <- workers) {
            waitingWorkers += worker
        }
        
        val tasks = Queue[Task]()
        val waitingWorkSenders = new Queue[(OutputChannel[Any], Task)]()

        loop {
            receive {
                case GiveMeWork =>
                    if(!waitingWorkSenders.isEmpty) {
                        val waitingWorkSender = waitingWorkSenders.dequeue
                        sender ! waitingWorkSender._2
                        waitingWorkSender._1 ! GiveMeWork
                    } else {
                        waitingWorkers.enqueue(sender)
                    }
                case t: Task =>
                    if (!waitingWorkers.isEmpty) {
                        waitingWorkers.dequeue() ! t
                        sender ! GiveMeWork
                    } else {
                        waitingWorkSenders.enqueue((sender, t))
                    }
                case Quit =>
                    workers.foreach { _ !? Quit }
                    sender ! OffDuty
                    exit()
            }
        }
    }
}