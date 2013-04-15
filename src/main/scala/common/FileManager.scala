package common

import java.io.File
import java.io.Writer
import java.io.BufferedWriter
import java.io.FileWriter
import scala.actors.Actor
import scala.io.Source
import scala.collection.mutable
import Stream._
import java.io.FileNotFoundException
import java.lang.RuntimeException
import scala.actors.OutputChannel

object FileManager extends Actor {
    object Protocol {
        trait GetRequest
        
        case class WriteFile(val fullFilename: String, val overwrite: Boolean = false)
        case class AcceptWriteFile(val writer: Writer)
        case object RejectWriteFile
        case class Write(sessionId: Int, text: String)
        case class Close(val sessionId: Int)
        
        case class CreateFile[T](val fullFilename: String, val overwrite: Boolean, createFun: File => T)
        
        case class FileExists(val fullFilename: String)
        case class Exists(val ex: Boolean)
        
        case class LinesOfFile(val fullFilename: String) extends GetRequest
        case class ReceiveFile(val fullFilename: String, mustExist: Boolean) extends GetRequest
        case class AcceptLinesOfFile(lines: Iterator[String])
        case class AcceptReceiveFile(file: File)
        case object FileNotExists
        
        case class Lock(val fullFilename: String)
        case class Unlock(val fullFilename: String)
        
        case class Error(val message: String)
        case class Success(a: Any)
        case object Failed
        
        case object Quit
    }
    
    import Protocol._
    
    val maxFilenameLength = 127
    
    trait Writer {
        val sessionId: Int
        val canonicalFilename: String
        
        def write(str: String) {
            FileManager ! Write(sessionId, str)
        }
        
        def close() {
            FileManager ! Close(sessionId)
        }
    }
    
    def quit() {
        this ! Quit
    }
    
    def compressFilename(filename: String) = if(filename.length > 124) {
        filename.substring(0, 36) + "_" + common.Common.hash(filename) + "_" + filename.substring(filename.length-18, filename.length) 
    } else {
        filename
    }
    
    def filenameAndPath2File(filename: String, path: Path) = {
        val f = compressFilename(filename)
        path / f
    }
    
    def filename2dirAndFilename(filename: String) = {
        val sep = Path.separator(filename)
        val i = filename.lastIndexOf(sep)
        
        if(i != -1) {
            val dirstr = filename.substring(0, i)
            val dirfile = new File(dirstr)
            val filestr = filename.substring(i+1, filename.length)
            
            if(dirfile.exists && dirfile.isDirectory()) {
                if(filestr.length > maxFilenameLength) {
                    (dirfile, Some(filestr))
                } else {
                    val filefile = new File(filename)
                    if(filefile.exists() && filefile.isDirectory()) {
                        (filefile, None)
                    } else {
                        (dirfile, Some(filestr))
                    }
                }
            } else {
                throw new FileNotFoundException(dirstr + " must be an existing directory to reference " + filename)
            }
        } else {
            if(filename.length > maxFilenameLength) {
                (new File("."), Some(filename))
            } else {
                val file = new File(filename)
                if(file.exists && file.isDirectory()) {
                    (file, None)
                } else {
                    (new File("."), Some(filename))
                }
            }
        }
    }
    
    def fullFilename2SafeFile(fullFilename: String) = {
        val (dir, filename) = filename2dirAndFilename(fullFilename)
        
        if(filename.isDefined) {
            new File(Path.join(dir.getCanonicalPath(), compressFilename(filename.get)))
        } else {
            throw new RuntimeException(filename + " is no file")
        }
    }
    
    def toTmpFile(fullFilename: String) = {
        val safeFile = fullFilename2SafeFile(fullFilename)
        new File(safeFile.getCanonicalPath + ".tmp")
    }
    
    def openWriter(fullFilename: String) = {
        val s = sessionIds.next
        val safeFile = fullFilename2SafeFile(fullFilename)
        
        val writer = new Writer() {
            val sessionId = s
            val canonicalFilename = safeFile.getCanonicalPath()
        }
        
        val w = new BufferedWriter(new FileWriter(toTmpFile(fullFilename)))
        openBufferedWriters(s) = w
        openWriters(s) = writer
        writer
    }
    
    def closeWriter(sessionId: Int) {
        openBufferedWriters(sessionId).close
        openBufferedWriters.remove(sessionId)
        
        val writer = openWriters(sessionId)
        toTmpFile(writer.canonicalFilename).renameTo(new File(writer.canonicalFilename))
        
        processListeners(new File(writer.canonicalFilename))
        openWriters.remove(sessionId)
    }
    
    def inOpenFilenames(fullFilename: String) = {
        val safeFile = fullFilename2SafeFile(fullFilename)
        openWriters.values.toList.map(_.canonicalFilename).contains(safeFile.getCanonicalPath()) || otherOpenFilenames.contains(fullFilename)
    }
    
    def processListeners(safeFile: File) {
        val listeners = fileListeners(safeFile.getCanonicalPath())
        fileListeners.remove(safeFile.getCanonicalPath())
        for(listener <- listeners) {
            listener._1 match {
                case LinesOfFile(_) => listener._2 ! AcceptLinesOfFile(Source.fromFile(safeFile).getLines)
                case ReceiveFile(_, _) => listener._2 ! AcceptReceiveFile(safeFile)
            }
        }
        otherOpenFilenames.remove(safeFile.getCanonicalPath())
    }
    
    def unlock(safeFile: File) {
        val locksListenerr = locksListener(safeFile.getCanonicalPath())
        locksListener.remove(safeFile.getCanonicalPath())
        for(locksListener <- locksListenerr) {
            locksListener._2 ! Success
        }
        openLocks.remove(safeFile.getCanonicalPath())
    }
    
    val openBufferedWriters = new mutable.HashMap[Int, BufferedWriter]
    val openWriters = new mutable.HashMap[Int, Writer]
    val otherOpenFilenames = new mutable.HashSet[String]
    val openLocks = new mutable.HashSet[String]
    val sessionIds = iterate(0)(_ + 1).toIterator
    
    val locksListener = new mutable.HashMap[String, mutable.ListBuffer[(Lock, OutputChannel[Any])]]() {
        override def default(key: String) = new mutable.ListBuffer[(Lock, OutputChannel[Any])]
    }
    
    val fileListeners = new mutable.HashMap[String, mutable.ListBuffer[(GetRequest, OutputChannel[Any])]]() {
        override def default(key: String) = new mutable.ListBuffer[(GetRequest, OutputChannel[Any])]
    }
    
    def act {
        loop {
            receive {
                case WriteFile(fullFilename, overwrite) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(tmpFile.exists || inOpenFilenames(fullFilename)) {
                        if(safeFile.exists) {
                            try { 
                                safeFile.delete()
                                tmpFile.delete()
                                sender ! AcceptWriteFile(openWriter(fullFilename))
                            } catch {
                                case _: Throwable => sender ! Error("For file " + fullFilename + " exists a .tmp file as well as the file itself\nThis should not occur. Therefore a deletion of the file was executed but failed.")
                            }
                        } else {
                            sender ! RejectWriteFile
                        }
                    } else if(safeFile.exists) {
                        if(overwrite) {
                            try {
                                safeFile.delete()
                                sender ! AcceptWriteFile(openWriter(fullFilename))
                            } catch {
                                case _: Throwable => sender ! Error("File " + fullFilename + " exists but should be overwritten.\nTherefore it was attempted to delete the file, which did not work")
                            }
                        } else {
                            sender ! RejectWriteFile
                        }
                    } else {
                        sender ! AcceptWriteFile(openWriter(fullFilename))
                    }
                }
                
                case Write(sessionId: Int, str: String) => {
                    openBufferedWriters(sessionId).write(str)
                }
                
                case Close(sessionId) => {
                    closeWriter(sessionId)
                }
                
                case FileExists(fullFilename: String) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists() || tmpFile.exists || inOpenFilenames(fullFilename)) sender ! Exists(true)
                    else sender ! Exists(false)
                }
                
                case Lock(fullFilename) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    
                    if(openLocks.contains(safeFile.getCanonicalPath())) {
                        val list = locksListener(safeFile.getCanonicalPath())
                        list += Pair(Lock(fullFilename), sender)
                        locksListener(safeFile.getCanonicalPath()) = list
                    } else {
                        openLocks += safeFile.getCanonicalPath()
                        sender ! Success
                    }
                }
                
                case Unlock(fullFilename) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    unlock(safeFile)
                }
                
                case ReceiveFile(fullFilename: String, mustExist: Boolean) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists()) {
                        sender ! AcceptReceiveFile(safeFile)
                    } else if(tmpFile.exists() && !inOpenFilenames(fullFilename)) {
                        try {
                            tmpFile.delete()
                            sender ! FileNotExists
                        } catch {
                            case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                        }
                    } else if(inOpenFilenames(fullFilename)) {
                        val list = fileListeners(safeFile.getCanonicalPath())
                        list += Pair(ReceiveFile(fullFilename, mustExist), sender)
                        fileListeners(safeFile.getCanonicalPath()) = list
                    } else if(!tmpFile.exists && !inOpenFilenames(fullFilename)) {
                        if(mustExist) sender ! FileNotExists
                        else sender ! AcceptReceiveFile(safeFile)
                    } else {
                        sender ! Error("Undefined behaviour")
                    } 
                }
                
                case LinesOfFile(fullFilename: String) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists()) {
                        sender ! AcceptLinesOfFile(Source.fromFile(safeFile).getLines)
                    } else if(tmpFile.exists() && !inOpenFilenames(fullFilename)) {
                        try {
                            tmpFile.delete()
                            sender ! FileNotExists
                        } catch {
                            case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                        }
                    } else if(inOpenFilenames(fullFilename)) {
                        val list = fileListeners(safeFile.getCanonicalPath())
                        list += Pair(LinesOfFile(fullFilename), sender)
                        fileListeners(safeFile.getCanonicalPath()) = list
                    } else if(!tmpFile.exists && !inOpenFilenames(fullFilename)) {
                        sender ! FileNotExists
                    } else {
                        sender ! Error("Undefined behaviour")
                    }
                }
                
                case CreateFile(fullFilename, overwrite, fileFun) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists()) {
                        if(overwrite) {
                            safeFile.delete()
                            otherOpenFilenames += safeFile.getCanonicalPath()
                            val t = fileFun(tmpFile)
                            tmpFile.renameTo(safeFile)
                            processListeners(safeFile)
                            otherOpenFilenames -= safeFile.getCanonicalPath()
                            sender ! Success(t)
                        } else {
                            sender ! Failed
                        }
                    } else if(tmpFile.exists() && !inOpenFilenames(fullFilename)) {
                        try {
                            safeFile.delete()
                            otherOpenFilenames += safeFile.getCanonicalPath()
                            fileFun(tmpFile)
                            tmpFile.renameTo(safeFile)
                            otherOpenFilenames -= safeFile.getCanonicalPath()
                            sender ! Success
                        } catch {
                            case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                        }
                    } else if(inOpenFilenames(fullFilename)) {
                        if(overwrite) {
                            // TODO wait until written and overwrite
                            sender ! Failed
                        } else {
                            sender ! RejectWriteFile
                        }
                    } else if(!tmpFile.exists && !inOpenFilenames(fullFilename)) {
                        otherOpenFilenames += safeFile.getCanonicalPath()
                        fileFun(tmpFile)
                        tmpFile.renameTo(safeFile)
                        otherOpenFilenames -= safeFile.getCanonicalPath()
                        sender ! Success
                    } else {
                        sender ! Error("Undefined behaviour")
                    }
                } 
                case Quit => exit
            }
        }
    }
    
    start
}