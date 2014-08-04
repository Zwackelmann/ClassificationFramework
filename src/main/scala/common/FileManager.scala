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
        case class CreateFile(val fullFilename: String) {
            val thread = Thread.currentThread()
        }
        case class CreateOrReceiveFile(val fullFilename: String) {
            val thread = Thread.currentThread()
        }
        case class ReceiveFile(val fullFilename: String) {
            val thread = Thread.currentThread()
        }
        case class DoesFileExist(val fullFilename: String)
        
        case class AcceptCreateFile(fileHandle: FileHandle)
        case object RejectCreateFile
        case class AcceptReceiveFile(file: File)
        
        case class CloseCreateFile(fileHandle: FileHandle)
        
        case object FileNotExists
        case object FileExists
        case object FileAlreadyExists
        
        case class Error(val message: String)
        case object Quit
    }
    
    import Protocol._
    
    val maxFilenameLength = 127
    
    trait FileHandle {
        val file: File // file to be written (tmp file)
        val _fullFilename: String // query filename
        
        def close() {
            FileManager ! CloseCreateFile(this)
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
    
    
    /* 
     * if file is untouched => 
     *      - create a new FileHandle
     *      - register that FileHandle
     *      - return it
     * if file is being created =>
     *      if file is being created by submitted actor =>
     *          just return the registered FileHandle
     *      if file is being created by another actor =>
     *          return None
     */ 
    def openFileHandle(fullFilename: String, workingThread: Thread) = {
        fileHandles.get(fullFilename) match {
            case Some((fileHandle, fileHandleOwnder)) => {
                if(workingThread == fileHandleOwnder) {
                    Some(fileHandle)
                } else {
                    None
                }
            }
            case None => {
                val tmpFile = toTmpFile(fullFilename)
    
                val fileHandle = new FileHandle() {
                    val file = tmpFile
                    val _fullFilename = fullFilename
                }
                fileHandles(fullFilename) = (fileHandle, workingThread)
                
                Some(fileHandle)
            }
        }
    }
    
    def closeFileHandle(fileHandle: FileHandle) {
        val safeFile = fullFilename2SafeFile(fileHandle._fullFilename)
        val tmpFile = toTmpFile(fileHandle._fullFilename)
        
        if(!(tmpFile.exists() || safeFile.exists())) throw new RuntimeException("After closing a FileHandler the respective File does not exist: " + tmpFile.getCanonicalPath())
        if(!safeFile.exists() && tmpFile.exists()) tmpFile.renameTo(safeFile)
        
        fileHandles.remove(fileHandle._fullFilename)
        val listenerList = fileListeners(fileHandle._fullFilename)
        fileListeners.remove(fileHandle._fullFilename)
        
        for(listener <- listenerList) {
            listener._2 ! AcceptReceiveFile(safeFile)
        }
    }
    
    def fileIsBeingCreated(fullFilename: String) = {
        fileHandles.keySet.contains(fullFilename)
    }
    
    val fileHandles = new mutable.HashMap[String, (FileHandle, Thread)] // fullFilename -> (fileHanle, sender)
    val fileListeners = new mutable.HashMap[String, mutable.HashSet[(Thread, OutputChannel[Any])]] {
        override def default(key: String) = new mutable.HashSet[(Thread, OutputChannel[Any])]
    }
    
    def act {
        loop {
            receive {
                case cf: CreateFile => {
                    val fullFilename = cf.fullFilename
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(!fileIsBeingCreated(fullFilename)) {
                        if(tmpFile.exists()) {
                            try {   
                                tmpFile.delete()
                            } catch {
                                case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                            }
                        }
                        
                        val fileHandle = openFileHandle(fullFilename, cf.thread).get
                        sender ! AcceptCreateFile(fileHandle)
                    } else {
                        val (fileHandle, fileHandleOwner) = fileHandles(fullFilename)
                        if(fileHandleOwner == cf.thread) {
                            sender ! AcceptCreateFile(fileHandle)
                        } else sender ! RejectCreateFile
                    }
                }
                
                case cf: ReceiveFile => {
                    val fullFilename = cf.fullFilename
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists()) {
                        sender ! AcceptReceiveFile(safeFile)
                    } else if(!fileIsBeingCreated(fullFilename)) {
                        if(tmpFile.exists()) {
                            try {
                                tmpFile.delete()
                            } catch {
                                case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                            }
                        }
                        sender ! FileNotExists
                    } else { // safeFile does not exists but fileIsBeingCreated
                        // check if the thread is already listening to that filename
                        if(fileListeners(fullFilename).map(_._1).toList.contains(cf.thread)) {
                            // do nothing
                        } // else check if the requesting thread is the thread that is currently creating the file 
                        else if(fileHandles(fullFilename)._2 == cf.thread) {
                            sender ! FileNotExists
                        } else { // Add the thread to fileListeners
                            val list = fileListeners(fullFilename)
                            list += (Pair(cf.thread, sender))
                            fileListeners(fullFilename) = list
                        }
                    }
                }
                
                case corf: CreateOrReceiveFile => {
                    val fullFilename = corf.fullFilename
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists()) {
                        sender ! AcceptReceiveFile(safeFile)
                    } else if(!fileIsBeingCreated(fullFilename)) {
                        if(tmpFile.exists()) {
                            try {
                                tmpFile.delete()
                            } catch {
                                case _: Throwable => sender ! Error("File " + fullFilename + " is available as .tmp file but is currently not written. Therefore a deletion was attempted but failed.")
                            }
                        }
                        
                        val fileHandle = openFileHandle(fullFilename, corf.thread).get
                        
                        sender ! AcceptCreateFile(fileHandle)
                    } else { // safeFile does not exists but fileIsBeingCreated
                        val (fileHandle, fileHandleOwner) = fileHandles(fullFilename)
                        if(fileHandleOwner == corf.thread) {
                            sender ! AcceptCreateFile(fileHandle)
                        } else {
                            // check if the thread is already listening to that filename
                            if(fileListeners(fullFilename).map(_._1).toList.contains(corf.thread)) {
                                // do nothing
                            } else {
                                val list = fileListeners(fullFilename)
                                list += (Pair(corf.thread, sender))
                                fileListeners(fullFilename) = list
                            }
                        }
                    }
                }
                
                case CloseCreateFile(fileHandle) => {
                    closeFileHandle(fileHandle)
                }
                
                case DoesFileExist(fullFilename) => {
                    val safeFile = fullFilename2SafeFile(fullFilename)
                    val tmpFile = toTmpFile(fullFilename)
                    
                    if(safeFile.exists() || fileIsBeingCreated(fullFilename)) sender ! FileExists
                    else sender ! FileNotExists
                }
                
                case Quit => exit
            }
        }
    }
    
    def withFileManager(f: => Unit) {
        try {
            f
        } finally {
            FileManager.quit()
        }
    }
    
    start
}