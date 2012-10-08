package filter.feature_scoreing
import parser.ArffJsonInstancesSource
import classifier.TargetClassDefinition

@serializable
class OddsRatio(source: ArffJsonInstancesSource, targetClassDef: TargetClassDefinition) extends FeatureScoreing(source, targetClassDef) {
    import FeatureScoreing._
    
    // override def score(t: Int) = (p(t |- IsCat) * (1 - p(t |- IsNotCat)) / (1 - p(t |- IsCat)) * p(t |- IsNotCat)).approx
    override def score(term: Int) = 
        if(numDocsCatAndTerm(term) == 0 || numDocsNotCatAndNotTerm(term) == 0) 0
        else if(numDocsCatAndNotTerm(term) == 0 || numDocsNotCatAndTerm(term) == 0) Double.PositiveInfinity
        else (numDocsCatAndTerm(term).toLong * numDocsNotCatAndNotTerm(term)).toDouble / (numDocsCatAndNotTerm(term).toLong * numDocsNotCatAndTerm(term))
}