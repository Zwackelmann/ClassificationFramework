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
        using(new ObjectOutputStream(new FileOutputStream(file))) { out => 
            out.writeObject(obj)
        }
    }
    
    def readObjectFromFile(file: File) = {
        using(new ObjectInputStream(new FileInputStream(file))) { in => 
            in.readObject()
        }
    }

    def using[A <% {def close(): Unit}, B](closable: A)(fun: A => B): B = {
        try {
            fun(closable)
        } finally {
            closable.close()
        }
    }
}