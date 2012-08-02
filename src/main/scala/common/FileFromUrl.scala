package common

import java.net.URL
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream

object FileFromUrl {
    def saveUrlAs(urlString: String, dest: File) {
        val url = new URL(urlString)
        val in = new BufferedInputStream(url.openStream())
        
        val out = new BufferedOutputStream(new FileOutputStream(dest))
        
        var buf = new Array[Byte](8192)
        def readWrite() {
            val n = in.read(buf)
            if(n >= 0) {
                out.write(buf, 0, n)
                readWrite
            }
        }
        
        readWrite()
        out.close
    }
}




