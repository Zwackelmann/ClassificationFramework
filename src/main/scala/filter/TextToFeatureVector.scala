package filter

import format.arff_json.ArffJsonInstance
import parser.ArffJsonInstancesSource
import java.util.TreeMap
import scala.collection.JavaConversions._
import format.arff_json.ArffJsonHeader
import format.arff_json.NumericArffJsonAttribute
import java.io.File
import scala.io.Source
import classifier.CategoryIs
import parser.History

object TextToFeatureVectorFilter {
    def main(args: Array[String]) {
        
        val header = ArffJsonHeader(1)
       
        val corpus = ArffJsonInstancesSource(
            List(
                ArffJsonInstance("1036.35021", List("35B27","35B25","74Q05","35B40"), List("Asymptotic analysis of periodically-perforated nonlinear media."), false),
                ArffJsonInstance("1042.37031", List("37E30","37G20","37J10"), List("Reconnection scenarios and the threshold of reconnection in the dynamics of non-twist maps."), false),
                ArffJsonInstance("1045.90001", List("90-06","90B06","00B15"), List("Distribution logistics. Advanced solutions to practical problems."), false),
                ArffJsonInstance("1047.62068", List("62J05","62F03","65C05"), List("An improved test for heteroskedasticity using adjusted modified profile likelihood inference"), false),
                ArffJsonInstance("1048.30026", List("30H05","30D55","46J20","46H10"), List("On a generalized corona problem on the unit disc"), false),
                ArffJsonInstance("1048.62055", List("62Hxx","62-01","00A06","62H99"), List("Multivariate statistical methods. A Primer"), false),
                ArffJsonInstance("1049.5006" , List("05A20","05A10","26D20","33B15"), List("The best bounds in Wallis' inequality"), false),
                ArffJsonInstance("1049.14040", List("14N05","14N20"), List("Joins of projective varieties and multisecant spaces"), false),
                ArffJsonInstance("1049.68027", List("68M20","68U20","68U99","68-06"), List("High performance computing in science and engineering"), false),
                ArffJsonInstance("1050.43004", List("43A20","46J05","43A10"), List("A B.A.I. proof of the non-Arens regularity of $L^1(\\cal G)$"), false),
                ArffJsonInstance("""[["1047.62068",["62J05","62F03","65C05"]],["An improved test for heteroskedasticity using adjusted modified profile likelihood inference This paper addresses the issue of testing for heteroskedasticity in linear regression models. We derive a Bartlett adjustment to the modified profile likelihood ratio test [see {\\it D. R. Cox} and {\\it N. Reid}, J. R. Stat. Soc., Ser. B 49, 1--39 (1987; Zbl 0616.62006)] for heteroskedasticity in the normal linear regression model. Our results generalize those of {\\it S. L. P. Ferrari} and {\\it F. Cribari-Neto} [Stat. Probab. Lett. 57, 353--361 (2002; Zbl 1023.62064)], since they allow for a vector-valued structure for the parameter that defines the skedastic function. Monte Carlo evidence shows that the proposed test displays reliable finite-sample behavior, outperforming the original likelihood ratio test, the Bartlett-corrected likelihood ratio test, and the modified profile likelihood ratio test."]]""", header)
            ),
            header
        )
        
        
        val filter = SimpleTextToFeatureVectorFilter(1, true, true, true)(corpus)
        
        filter.dict.update
        println(filter.dict._wordToIndexMap.toList.sortBy(_._1).mkString("\n"))
        
        println(filter.applyFilter(corpus).mkString("\n"))
    }
}

abstract class TextToFeatureVectorFilter extends GlobalFilter {
    def inst2Words(inst: ArffJsonInstance): Iterator[String]
    
    val dict: Dictionary
    
    def buildDict(source: ArffJsonInstancesSource) {
        for(inst <- source.iterator) {
            for(word <- inst2Words(inst)) {
                dict.add(word)
            }
        }
    }
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        source.map(
            elemFun = ((inst: ArffJsonInstance) => {
                val data = new TreeMap[Int, Double]
                
                for(word <- inst2Words(inst)) {
                    dict.wordIndex(word) match {
                        case Some(index) => data(index) = data.getOrElse(index, 0.0) + 1.0
                        case None => // ignore tokens that are not in the dictionary
                    }
                }
                
                ArffJsonInstance(inst.id, inst.categories, data.toMap, dict.size())
            }),
            headerFun = header => ArffJsonHeader(dict.size)
        )
    }
}

object SimpleTextToFeatureVectorFilter {
    def apply(minOcc: Int, applyStemming: Boolean, ignoreFormulae: Boolean, ignoreReferences: Boolean) = new FilterFactory with Loadable[TextToFeatureVectorFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = { 
            val filter = new SimpleTextToFeatureVectorFilter(minOcc, applyStemming, ignoreFormulae, ignoreReferences) {
                override val trainingParams = Filter.trainingParams(historyAppendix, trainBase)
            }
            
            filter.buildDict(trainBase)
            filter
        }
        
        val historyAppendix = "svec-" + minOcc + (if(applyStemming) 1 else 0) + (if(ignoreFormulae) 1 else 0) + (if(ignoreReferences) 1 else 0)
    }
    
    trait Appendix extends History with Serializable {
        val minOcc: Int = 3
        val applyStemming: Boolean = true
        val ignoreFormulae: Boolean = true
        val ignoreReferences: Boolean = true
        
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ SimpleTextToFeatureVectorFilter(minOcc, applyStemming, ignoreFormulae, ignoreReferences)
    }
}

abstract class SimpleTextToFeatureVectorFilter(val minOcc: Int, applyStemming: Boolean, ignoreFormulae: Boolean, ignoreReferences: Boolean) extends TextToFeatureVectorFilter with Serializable {
    @transient lazy val stemmer = new PorterStemmer
    
    def inst2Words(inst: ArffJsonInstance): Iterator[String] = {
        inst.dataAt(0) match {
            case text: String => {
                TokenSequence(text).flatMap(token => token match { 
                    case SimpleToken(text) => List(text)
                    case Formula(content) => if(ignoreFormulae) List() else List("$" + content.filter(_.isLetterOrDigit) + "$")
                    case r: Reference => if(ignoreReferences) List() else List(r.toString)
                    case _ => List()
                })
            }
            case x => throw new RuntimeException("TextToFeatureVectorFilter could not trainsform an ArffJsonInstance into a sequence of text tokens, because the first item is no String.\nFirst item is: " + inst.dataAt(0))
        }
    }
    
    override val dict = new Dictionary {
        override def wordFun(word: String) = stemmer.stem(word.filter(c => c.isLetter || c.isDigit).toLowerCase())
        override val stopList = Source.fromFile("util/stoplist.txt").getLines.toIndexedSeq
        override def wordCond(word: String) = true
        override val minWordCount = minOcc
    }
}
















