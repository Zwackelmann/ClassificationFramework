package common

import java.io.File
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.io.ObjectInputStream
import java.io.FileInputStream
import java.io.EOFException

object ObjectToFile {
    def writeObjectToFile(obj: Any, file: File) {
        try {
            val outputStream = new ObjectOutputStream(new FileOutputStream(file))
            outputStream.writeObject(obj)
            outputStream.close
        } catch {
            case fnf: FileNotFoundException => fnf.printStackTrace()
            case io: IOException => io.printStackTrace()
        }
    }
    
    def readObjectFromFile(file: File) = {
        try {
            val inputStream = new ObjectInputStream(new FileInputStream(file));
            
            val obj: Any = inputStream.readObject()
            if (obj == null) {
                throw new RuntimeException("object could not be read")
            }
            inputStream.close()
            obj
        } catch {
            case eof: EOFException => eof.printStackTrace()
            case cnf: ClassNotFoundException => cnf.printStackTrace()
            case fnf: FileNotFoundException => fnf.printStackTrace()
            case io: IOException => io.printStackTrace()
        }
    }
}