package common

import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants
import scala.collection.mutable
import org.w3c.dom.Node
import org.w3c.dom.NodeList

object Xpath {
    val xpFactory = XPathFactory.newInstance()
    
    def eval(doc: Any, xPathString: String) = {
	    val xpath = xpFactory.newXPath()
		val expr = xpath.compile(xPathString)
		val nodes = expr.evaluate(doc, XPathConstants.NODESET).asInstanceOf[NodeList]
	    
	    val nodeList = new mutable.ListBuffer[Node]
	    for(i <- 0 until nodes.getLength()) {
		    nodeList += nodes.item(i)
		}
	    
	    nodeList.toList
	}
}