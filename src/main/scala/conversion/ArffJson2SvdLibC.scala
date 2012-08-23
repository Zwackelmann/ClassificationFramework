package conversion

import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import format.arff_json.ArffJsonInstances
import java.io.BufferedWriter
import java.io.FileWriter
import format.arff_json.ArffJsonInstance
import format.arff_json.SparseArffJsonInstance
import format.arff_json.SparseArffJsonInstance
import format.arff_json.DenseArffJsonInstance
import format.arff_json.SparseArffJsonInstance
import parser.ArffJsonInstancesFile
import parser.ContentDescription

object ArffJson2SvdLibC {
    def main(args: Array[String]) {
        val inst = new ArffJsonInstancesFile("title-only", ContentDescription.TestSet, List(/* history */))
        
        val out = new BufferedWriter(new FileWriter(new File("svdLibCInput.txt")))
        
        val numNonZeroValues = (for(i <- inst) yield {
            i match {
                case s: SparseArffJsonInstance => s.dataMap.size
                case d: DenseArffJsonInstance => d.dataList.size
                case _ => throw new RuntimeException()
            }
        }).reduceLeft(_ + _)
        
        val numCols = inst.numAttributes
        val numRows = inst.numInstances
        
        out.write(numRows + " " + numCols + " " + numNonZeroValues + "\n")
        
        for(i <- inst) {
            out.write((i match {
                case s: SparseArffJsonInstance => s.dataMap.size + " " + s.dataMap.toList.sortBy(_._1).map(m => m._1 + " " + m._2).mkString(" ")
                case d: DenseArffJsonInstance => d.dataList.size + " " + (for(i <- 0 until d.dataList.size) yield (i + " " + d.dataList(i))).mkString(" ")
                case _ => throw new RuntimeException()
            }) + "\n")
        }
            
        out.close
    }
}







