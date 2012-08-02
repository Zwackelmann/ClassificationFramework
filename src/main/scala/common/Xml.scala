package common

import javax.xml.parsers.DocumentBuilderFactory
import java.io.InputStream

object Xml {
    val factory = DocumentBuilderFactory.newInstance()
	factory.setNamespaceAware(true)
	val builder = factory.newDocumentBuilder()
	
	def parse(filename: String) = {
        builder.parse(filename)
    }
    
    def parse(stream: InputStream) = {
        builder.parse(stream)
    }
}