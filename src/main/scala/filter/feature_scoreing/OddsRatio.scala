package filter.feature_scoreing
import parser.ArffJsonInstancesSource
import classifier.CategoryIs

@serializable
class OddsRatio(source: ArffJsonInstancesSource, categoryIs: CategoryIs) extends FeatureScoreing(source, categoryIs) {
    import FeatureScoreing._
    
    // override def score(t: Int) = (p(t |- IsCat) * (1 - p(t |- IsNotCat)) / (1 - p(t |- IsCat)) * p(t |- IsNotCat)).approx
    override def score(term: Int) = 
        if(numDocsCatAndTerm(term) == 0 || numDocsNotCatAndNotTerm(term) == 0) 0
        else if(numDocsCatAndNotTerm(term) == 0 || numDocsNotCatAndTerm(term) == 0) Double.PositiveInfinity
        else (numDocsCatAndTerm(term).toLong * numDocsNotCatAndNotTerm(term)).toDouble / (numDocsCatAndNotTerm(term).toLong * numDocsNotCatAndTerm(term))
}