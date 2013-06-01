package script.formulas
import uk.ac.ed.ph.snuggletex.SnuggleEngine
import uk.ac.ed.ph.snuggletex.SnuggleSession
import uk.ac.ed.ph.snuggletex.SnuggleInput
import uk.ac.ed.ph.snuggletex.utilities.StylesheetCache
import uk.ac.ed.ph.snuggletex.upconversion.MathMLUpConverter
import uk.ac.ed.ph.snuggletex.upconversion.internal.UpConversionPackageDefinitions
import uk.ac.ed.ph.snuggletex.internal.util.XMLUtilities
import uk.ac.ed.ph.snuggletex.DOMOutputOptions
import org.w3c.dom.Element
import org.w3c.dom.NodeList
import uk.ac.ed.ph.snuggletex.utilities.MathMLUtilities
import org.w3c.dom.Node
import uk.ac.ed.ph.snuggletex.SnuggleConstants
import uk.ac.ed.ph.snuggletex.upconversion.UpConversionUtilities
import uk.ac.ed.ph.snuggletex.utilities.SerializationOptions
import org.w3c.dom.Document
import java.io.StringWriter
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerFactory
import javax.xml.transform.OutputKeys
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.TransformerException
import uk.ac.ed.ph.snuggletex.upconversion.UpConvertingPostProcessor

object Snuggle {
    val DEFAULT_UPCONVERSION_OPTIONS = "\\assumeSymbol{e}{exponentialNumber}\n" + 
        "\\assumeSymbol{f}{function}\n" + 
        "\\assumeSymbol{f_n}{function}\n" + 
        "\\assumeSymbol{g}{function}\n" + 
        "\\assumeSymbol{i}{imaginaryNumber}\n" + 
        "\\assumeSymbol{\\pi}{constantPi}\n" + 
        "\\assumeSymbol{\\gamma}{eulerGamma}\n" + 
        "\\setUpConversionOption{addOptionsAnnotation}{true}"
    
    lazy val engine = {
        val e = new SnuggleEngine()
        e.addPackage(UpConversionPackageDefinitions.getPackage())
        e
    }
    
    lazy val session = engine.createSession()
    lazy val sourceSerializationOptions = createMathMLSourceSerializationOptions()
    
    lazy val upConversionOptions = {
        val proc = new UpConvertingPostProcessor()
        proc.getUpconversionOptions()
    }
    lazy val upConverter = new MathMLUpConverter()
    
    lazy val documentBuilder = XMLUtilities.createNSAwareDocumentBuilder()
    lazy val domOptions = {
        val o = new DOMOutputOptions()
        o.setMathVariantMapping(true)
        o.setAddingMathSourceAnnotations(true)
        o
    }
        
    def receiveContentMathML(inputLatex: String) = {
        session.reset()
        session.parseInput(new SnuggleInput(DEFAULT_UPCONVERSION_OPTIONS, "Assumptions Input"))
        session.parseInput(new SnuggleInput("\\[ " + inputLatex + " \\]", "Math Input"))
        
        val resultDocument = documentBuilder.newDocument()
        val resultRoot = resultDocument.createElement("root")
        resultDocument.appendChild(resultRoot)
        
        session.buildDOMSubtree(resultRoot, domOptions)
        val resultNodeList = resultRoot.getChildNodes()
        val mathMLElement = extractMathMLElement(resultNodeList, true)
        
        val upConvertedMathDocument = upConverter.upConvertSnuggleTeXMathML(mathMLElement.getOwnerDocument(), upConversionOptions)
        
        val cMathMLElement = extractMathMLElement(upConvertedMathDocument.getDocumentElement().getChildNodes(), false)
        val cMathMLDocument = MathMLUtilities.isolateAnnotationXML(cMathMLElement, MathMLUpConverter.CONTENT_MATHML_ANNOTATION_NAME)
        
        if(cMathMLDocument != null) {
            Some(nodeToString(cMathMLDocument).filter(c => !c.isWhitespace))
        } else {
            None
        }
    }
    
    
    def serializeDocument(doc: Document) {
        if(doc != null) println(MathMLUtilities.serializeDocument(doc))
    }
    
    def extractMathMLElement(resultNodeList: NodeList, allowUpConversionOptionsElements: Boolean) = {
        val nodes = (0 until resultNodeList.getLength()).map(i => resultNodeList.item(i))
        
        val (element, status) = ((None: Option[Element], true) /: nodes)((old, nextElement) => {
            val (oldElement, oldStatus) = old
            
            if (MathMLUtilities.isMathMLElement(nextElement)) {
                if(!oldElement.isDefined) (Some(nextElement.asInstanceOf[Element]), oldStatus)
                else (oldElement, false)
            } else if (nextElement.getNodeType() == Node.TEXT_NODE && nextElement.getNodeValue().trim().length() == 0) {
                (oldElement, oldStatus)
            } else if (allowUpConversionOptionsElements
                    && nextElement.getNodeType() == Node.ELEMENT_NODE
                    && SnuggleConstants.SNUGGLETEX_NAMESPACE.equals(nextElement.getNamespaceURI())
                    && UpConversionUtilities.UPCONVERSION_OPTIONS_XML_LOCAL_NAME.equals(nextElement.getLocalName())) {
                (oldElement, oldStatus)
            } else {
                (oldElement, false)
            }
        })
        
        if(status && element.isDefined) {
            element.get
        } else {
            null
        }
    }
    
    def createMathMLSourceSerializationOptions() = {
        val result = new SerializationOptions()
        result.setIndenting(true)
        result.setUsingNamedEntities(true)
        result
    }
    
    def nodeToString(node: Node) = {
        val sw = new StringWriter()
        try {
            val t: Transformer =  TransformerFactory.newInstance().newTransformer()
            t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
            t.setOutputProperty(OutputKeys.INDENT, "yes")
            t.transform(new DOMSource(node), new StreamResult(sw))
        } catch {
            case te: TransformerException => println("nodeToString Transformer Exception")
        }
        
        sw.toString()
    }
}