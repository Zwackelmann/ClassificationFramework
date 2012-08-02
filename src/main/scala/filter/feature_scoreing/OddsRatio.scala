package filter.feature_scoreing
import parser.ArffJsonInstancesSource

@serializable
class OddsRatio(source: ArffJsonInstancesSource) extends FeatureScoreing(source) {
    import FeatureScoreing._
    
    override def score(t: Int, c: Int) = (p(t |- c) * (1 - p(t |- not(c))) / (1 - p(t |- c)) * p(t |- not(c))).approx
}