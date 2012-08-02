package script
import java.io.File
import common.Common.FileConversion._
import filter.feature_scoreing.OddsRatio
import parser.ArffJsonInstancesFile
import java.io.BufferedWriter
import java.io.FileWriter


object Test4 {
    def main(args: Array[String]) {
        val topClasses = new File("data/util/top_classes.txt").lines.toList
        val trainSource = new ArffJsonInstancesFile("abstract-only", "train", List())
        
        for(topClass <- topClasses) {
            val or = new OddsRatio(trainSource).rankedFeatureList(topClass.toInt)
            val firstOr = or.take(1000)
            
            val writer = new BufferedWriter(new FileWriter(new File("../../odds_ratio_results/odds_ratio_abstract_" + topClass)))
            // val writer = new BufferedWriter(new FileWriter(new File("odds_ratio_abstract_" + topClass)))
            
            for(or <- firstOr) {
                writer.write("(\"" + trainSource.header.attributes(or._1).name + "\", " + or._2 + ")\n")
            }
            writer.close
        }
        
    }
}








