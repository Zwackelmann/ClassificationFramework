package filter.feature_scoreing
import parser.ArffJsonInstancesSource
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import classifier.CategoryIs

object FeatureScoreing {
    trait CatCondition
    case object IsCat extends CatCondition
    case object IsNotCat extends CatCondition
    
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
abstract class FeatureScoreing(inst: ArffJsonInstancesSource, categoryIs: CategoryIs) {
    import FeatureScoreing._
    
    def score(t: Int): Double
    def rankedFeatureList(): List[Pair[Int, Double]] = termSet.filter(t => !score(t).isNaN).map(t => Pair(t, score(t))).toList.sortWith((x1, x2) => x1._2 > x2._2)
    
    val (numTargetInst, numOtherInst, termMapTarget, termMapOther, termSet) = {
        var numTargetInst = 0
        var numOtherInst = 0
        
        val termMapTarget = new HashMap[Int, Int].withDefault(_ => 0)
        val termMapOther = new HashMap[Int, Int].withDefault(_ => 0)
        
        val termSet = new HashSet[Int]
        
        for(inst <- inst) {
            for((key, value) <- inst.sparseData) {
                termSet += key
            }
            
            val isTarget = categoryIs.matchesForTraining(inst.categories)
            if(isTarget.isDefined && isTarget.get) {
                numTargetInst += 1
                for((key, value) <- inst.sparseData) {
                    termMapTarget(key) += 1
                }
            } else if(isTarget.isDefined){
                numOtherInst += 1
                for((key, value) <- inst.sparseData) {
                    termMapOther(key) += 1
                }
            }
        }        
        
        (numTargetInst, numOtherInst, termMapTarget.toMap, termMapOther.toMap, termSet.toSet)
    }
    
    def numDocsCatAndTerm(term: Int) = termMapTarget.getOrElse(term, 0)
    def numDocsCatAndNotTerm(term: Int) = numTargetInst - termMapTarget.getOrElse(term, 0)
    def numDocsNotCatAndTerm(term: Int) = termMapOther.getOrElse(term, 0)
    def numDocsNotCatAndNotTerm(term: Int) = numOtherInst - termMapOther.getOrElse(term, 0)
    
    def p(pr: PropCondition) = {
        val t = pr.x
        val c = pr.within
        
        c match {
            case IsCat => new Fraction(termMapTarget.getOrElse(t, 0).toLong, numTargetInst)
            case IsNotCat => new Fraction(termMapOther.getOrElse(t, 0).toLong, numOtherInst)
        }
    }
    
    /*def p(pr: PropCondition) = {
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
    }*/
    
}






