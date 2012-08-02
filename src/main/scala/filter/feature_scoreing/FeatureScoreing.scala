package filter.feature_scoreing
import parser.ArffJsonInstancesSource
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object FeatureScoreing {
    trait CatCondition
    case class IsCat(val x: Int) extends CatCondition
    case class IsNotCat(val x: Int) extends CatCondition
    
    def not(x: Int) = IsNotCat(x)
    implicit def int2Is(x: Int) = IsCat(x)
    
    
    case class PropCondition(val x: Int, val within: CatCondition)
    
    class FancyInt(val x: Int) {
        def |-(within: CatCondition) = new PropCondition(x, within)
    }
    
    implicit def int2FancyInt(x: Int): FancyInt = new FancyInt(x)
    
    object Fraction {
        implicit def int2Fraction(x: Int): Fraction = new Fraction(x, 1)
        implicit def pair2Fraction(x: Pair[Int, Int]): Fraction = new Fraction(x._1, x._2)
    }
    
    class Fraction(n: Long, d: Long) {
        require(d != 0)
     
        private val g = gcd(n, d)
        val numerator : Long = n / g
        val denominator : Long = d / g
     
        private def gcd(a: Long, b: Long): Long =
            if (b == 0) a else gcd(b, a % b)
     
        def +(f2: Fraction) = new Fraction(numerator*f2.denominator + f2.numerator*denominator, denominator*f2.denominator)
        def -(f2: Fraction) = new Fraction(numerator*f2.denominator - f2.numerator*denominator, denominator*f2.denominator)
        def *(f2: Fraction) = new Fraction(numerator*f2.numerator, denominator * f2.denominator)
        def /(f2: Fraction) = new Fraction(numerator*f2.denominator, denominator * f2.numerator)   
        
        def ^(x: Int) = {
            require(x == -1, "Only ^(-1) is allowed :-)")
            new Fraction(denominator, numerator)
        }
        
        def approx() = numerator.toDouble / denominator
        override def toString = numerator + "/" + denominator
    }
}

@serializable
abstract class FeatureScoreing(inst: ArffJsonInstancesSource) {
    import FeatureScoreing._
    
    def score(t: Int, c: Int): Double
    def rankedFeatureList(c: Int): List[Pair[Int, Double]] = termMap.keys.map(t => Pair(t, score(t, c))).toList.sort((x1, x2) => x1._2 > x2._2)
    
    val (termCatMap, termMap, catMap, numDocs) = {
        val termCatMap = new HashMap[Int, HashMap[Int, Int]] {
            override def default(key: Int) = new HashMap[Int, Int] {
                override def default(key: Int) = 0
            }
        }
        
        val termMap = new HashMap[Int, Int] {
            override def default(key: Int) = 0
        }
        
        val catMap = new HashMap[Int, Int] {
            override def default(key: Int) = 0
        }
        
        var numDocs = 0
        
        for(inst <- inst.iterator) {
            numDocs = numDocs + 1
            for((termIndex, value) <- inst.sparseData) {
                termMap(termIndex) = termMap(termIndex) + 1
            }
            
            for(topClass <- inst.mscClasses.map(_.substring(0, 2).toInt).distinct) {
                catMap(topClass) = catMap(topClass) + 1
            }
            
            for(topClass <- inst.mscClasses.map(_.substring(0, 2).toInt).distinct) {
                for((termIndex, value) <- inst.sparseData) {
                    termCatMap(termIndex) = {
                        val termsCatMap = termCatMap(termIndex)
                        termsCatMap(topClass) = termsCatMap(topClass) + 1
                        termsCatMap
                    }
                }
            }
        }
        
        (termCatMap, termMap, catMap, numDocs)
    }
    
    def p(pr: PropCondition) = {
        val t = pr.x
        val c = pr.within
        
        c match {
            case IsCat(c) => new Fraction(termCatMap(t)(c).toLong, catMap(c).toLong)
            case IsNotCat(c) => {
                // as P(A) = P(A | B) * P(B) + P(A | -B) * P(-B)
                // <=> P(A | -B) = (P(A) - P(A | B) * P(B)) / P(-B)
                
                // p(t)
                val pT = new Fraction(termMap(t).toLong, numDocs)
                
                // p(c)
                val pC = new Fraction(catMap(c), numDocs)
                
                // p(-c)
                val pNotC = 1 - pC
                
                // p(t | c)
                val pTGivenC = {
                    val numDocumentsWithTermAndCat = termCatMap(t)(c).toLong
                    val numDocumentsWithCat = catMap(c).toLong
                    
                    if(numDocumentsWithCat == 0) {
                        new Fraction(0, 1)
                    } else {
                        new Fraction(numDocumentsWithTermAndCat, numDocumentsWithCat)
                    }
                }
                
                (pT - (pTGivenC * pC))./(pNotC)
            }
        }
    }
    
}






