package format.arff_json

object Point {
    def mapMerge(a: Map[Int, Double], b: Map[Int, Double], fun: (Double, Double) => Double) = {
        (for(key <- a.keys ++ b.keys) yield {
            val aValue = a.getOrElse(key, 0.0)
            val bValue = b.getOrElse(key, 0.0)
            key -> fun(aValue, bValue)
        }).toMap
    }
    
    def pointMerge(p1: Point, p2: Point, fun: (Double, Double) => Double) = new Point {
        def data = p1.data.zip(p2.data).map(d => fun(d._1, d._2))
        def sparseData = Point.mapMerge(p1.sparseData, p2.sparseData, fun)
    }
    
    def apply(values: Map[Int, Double], numDimensions: Int) = new Point {
        def data = for(i <- 0 until numDimensions) yield values.getOrElse(i, 0.0)
        def sparseData = values
    }
    
    def apply(values: Seq[Double]) = new Point {
        def data = values
        def sparseData = (0 until values.size).zip(values).toMap
    }
}

trait Point {
    import Point.pointMerge
    
    def data: Seq[Double]
    def sparseData: Map[Int, Double]
    
    def +(p: Point) = pointMerge(this, p, (a, b) => a + b)
    def -(p: Point) = pointMerge(this, p, (a, b) => a - b)
    def *(p: Point) = pointMerge(this, p, (a, b) => a * b)
    def /(p: Point) = pointMerge(this, p, (a, b) => a / b)
    
    def norm = math.sqrt(sparseData.map(a => a._2 * a._2).reduceLeft(_ + _))
}












