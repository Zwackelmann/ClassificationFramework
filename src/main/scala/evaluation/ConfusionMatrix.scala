package evaluation

import classifier.CategoryIsMsc
import java.awt.image.BufferedImage
import java.awt.GraphicsConfiguration
import java.awt.GraphicsEnvironment
import java.awt.Color

object ConfusionMatrix {
    trait ConfusionMatrixImageConfig {
        val cellWidth: Double
        val cellHeight: Double
        
        val lowBound: Double
        val highBound: Double
        
        val colors: List[(Double, Double, Double)]
    }
    
    def confusionMap(confusionMeta: ConfusionMetadata, cats: List[CategoryIsMsc]) = 
        (for(c1 <- cats.map(_.topClass.get); c2 <- cats.map(_.topClass.get)) yield {
            (c1, c2) -> confusionMeta.confusion(c1, c2)
        }).toMap
        
    def confusionMatrix(confusionMeta: ConfusionMetadata, cats: List[CategoryIsMsc]) = 
        confusionMap(confusionMeta, cats)
            .groupBy(_._1._1)
            .toList
            .sortBy(_._1)
            .map(kv => (
                kv._2.toList.sortBy(_._1._2).map(_._2)
            ))
            
    def confusionToExcelString(confusionMeta: ConfusionMetadata, cats: List[CategoryIsMsc]) = {
        val matrix = confusionMatrix(confusionMeta, cats)
        
        "\t" + cats.map(_.topClass.get).mkString("\t") + "\n" + 
        (matrix zip cats).map(x => {
            val (row, cat) = x
            cat.topClass.get + "\t" + row.mkString("\t")
        }).mkString("\n")
    }
        
    def confusionToImage(confusionMeta: ConfusionMetadata, cats: List[CategoryIsMsc], conf: ConfusionMatrixImageConfig) = {
        def color(value: Double) = {
            val f = if(value <= conf.lowBound || value.isNaN()) 0.0
            else if(value >= conf.highBound) 1.0
            else (value - conf.lowBound) / (conf.highBound - conf.lowBound)
            
            val stepWidth = 1.0 / (conf.colors.size - 1)
            val (bucket, f2) = {
                val x = math.floor(f / stepWidth).toInt
                if(x == conf.colors.size - 1) (x-1, 1.0)
                else (x, f % stepWidth)
            }
            
            val c1 = {
                val x = conf.colors(bucket)
                (x._1 * (1-f2), x._2 * (1-f2), x._3 * (1-f2))
            }
            
            val c2 = {
                val x = conf.colors(bucket+1)
                (x._1 * f2, x._2 * f2, x._3 * f2)
            }
            
            new Color((c1._1 + c2._1).toInt, (c1._2 + c2._2).toInt, (c1._3 + c2._3).toInt)
        }
        
        val image: BufferedImage = 
            new BufferedImage((conf.cellWidth*cats.size).toInt, (conf.cellHeight*cats.size).toInt, BufferedImage.TYPE_INT_RGB)
        val gr = image.getGraphics()
        
        val matrix = confusionMatrix(confusionMeta, cats)
        /*val (min, max, avg, stddev) = {
            val x = matrix.flatten.filter(!_.isNaN())
            val avg = x.sum / x.size
            (x.min, x.max, avg, x.map(y => math.abs(y-avg)).sum / x.size)
        }*/
        
        for((row, rowIndex) <- (matrix zipWithIndex)) {
            for((value, colIndex) <- (row zipWithIndex)) {
                gr.setColor(color(value))
                gr.fillRect(
                    (rowIndex*conf.cellWidth).toInt, 
                    (colIndex*conf.cellHeight).toInt, 
                    conf.cellWidth.toInt, 
                    conf.cellHeight.toInt
                )
            }
        }
        
        image
    }
}




















