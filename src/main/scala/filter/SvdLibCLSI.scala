package filter

import parser.ArffJsonInstancesSource
import parser.History
import classifier.CategoryIs
import java.io.DataOutputStream
import format.arff_json.SparseData
import java.io.File
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.BufferedReader
import java.io.FileReader
import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.FileInputStream
import common.Common
import common.Common
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader

object SvdLibCLsiFilter {
    def apply(numDims: Int) = new FilterFactory with Loadable[SvdLibCLsiFilter] {
        def apply(trainBase: ArffJsonInstancesSource) = new SvdLibCLsiFilter(trainBase, numDims) {
            override val trainingParams = Filter.trainingParams(historyAppendix, trainBase)
        }
        val historyAppendix = "clsi-" + numDims
    }
    
    @serializable
    trait Appendix extends History {
        val numLsiDims: Int
        abstract override def apply(categoryIs: CategoryIs) = super.apply(categoryIs) :+ SvdLibCLsiFilter(numLsiDims)
    }
    
    def dumpInstancesForSvdLibC(source: ArffJsonInstancesSource, file: File) {
        val stream = new DataOutputStream(
            new BufferedOutputStream(new FileOutputStream(file))
        )
        val totalNonZeroValues = source.map(_.asInstanceOf[SparseData].dataMap.size).sum
        
        stream.writeInt(source.numAttributes)
        stream.writeInt(source.numInstances)
        stream.writeInt(totalNonZeroValues)
        
        for(inst <- source) {
            val nonZeroValues = inst.asInstanceOf[SparseData].dataMap.asInstanceOf[Map[Int, Double]]
            
            stream.writeInt(nonZeroValues.size)
            for((index, value) <- nonZeroValues.toList.sortBy(_._1)) {
                stream.writeInt(index)
                stream.writeFloat(value.floatValue)
            }
        }
        stream.close()
    }
    
    def readInvSingularValues(sigmaMatrixFile: File) = {
        val br = new BufferedReader(new FileReader(sigmaMatrixFile))
        
        val dims = br.readLine().toInt
        val s = Array.ofDim[Double](dims)
        println("singular values: " + s.toList)
        for(i <- 0 until dims) {
            s(i) = -1d / br.readLine().toDouble
        }
        br.close()
        s
    }
    
    def readUAndMultiplyWithS(matrixFile: File, s: Array[Double]) = {
        val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(matrixFile)))

        val rows = dis.readInt()
        val cols = dis.readInt()
        
        val m = Array.ofDim[Double](rows, cols)
        
        for(row <- (0 until rows)) {
            val singularValue = s(row)
            for(col <- (0 until cols)) {
                m(row)(col) = singularValue * dis.readFloat()
            }
        }
        
        dis.close()
        m
    }
    
    def determineM(source: ArffJsonInstancesSource, numDims: Int) = {
        val tmpInstancesFilename = "svdlibc-inst-" + Common.randomStream.map(r => (r*10).toInt).take(10).mkString
        val tmpSvdLibCResultsFilenamePrefix = "svdlibc-res-" + Common.randomStream.map(r => (r*10).toInt).take(10).mkString
        dumpInstancesForSvdLibC(source, new File(tmpInstancesFilename))
        
        val process = Runtime.getRuntime().exec("svd -o %s -r sb -w db -d %d %s".format(tmpSvdLibCResultsFilenamePrefix, numDims, tmpInstancesFilename))
        process.waitFor()
        
        val sFile = new File(tmpSvdLibCResultsFilenamePrefix + "-S")
        val utFile = new File(tmpSvdLibCResultsFilenamePrefix + "-Ut")
        val vtFile = new File(tmpSvdLibCResultsFilenamePrefix + "-Vt")
        
        val m = readUAndMultiplyWithS(utFile, readInvSingularValues(sFile))
        
        sFile.delete()
        utFile.delete()
        vtFile.delete()
        new File(tmpInstancesFilename).delete()
        
        m
    }
    
    def projectDoc(inst: ArffJsonInstance, m: Array[Array[Double]]) = {
        val dims = m.length
        
        val docMap = inst.asInstanceOf[SparseData].dataMap.asInstanceOf[Map[Int, Double]]
        val dataList = (for(i <- 0 until dims) yield {
            docMap.map(kv => m(i)(kv._1) * kv._2).sum
        }).toList
        
        val mappedInst = ArffJsonInstance(inst.id, inst.categories, dataList, false)
        
        mappedInst
    }
    
    def main(args: Array[String]) {
        val source = ArffJsonInstancesSource(List(
                 ArffJsonInstance("1", List(), Map( 0 -> 9.2670, 1 -> 11.1720,  2 -> 8.8520,  6 -> 0.5850,  8 -> 8.5871), 14),
                 ArffJsonInstance("2", List(), Map( 2 -> 5.2651, 6 ->  0.5850,  9 -> 4.0971                            ), 14), 
                 ArffJsonInstance("3", List(), Map( 4 -> 8.5466, 6 ->  0.5850,  9 -> 3.1699                            ), 14),
                 ArffJsonInstance("4", List(), Map( 3 -> 9.2670, 7 ->  9.2670, 12 -> 9.2670, 13 -> 8.5871              ), 14),
                 ArffJsonInstance("5", List(), Map(10 -> 4.7549                                                        ), 14),
                 ArffJsonInstance("6", List(), Map( 4 -> 6.8501, 5 -> 10.3399,  6 -> 0.5850, 10 -> 7.6195, 11 -> 9.8419), 14)
            ),
            ArffJsonHeader(14)
        )
        
        val lsiFilter = SvdLibCLsiFilter(4)(source)
        
        val newInst = lsiFilter.applyFilter(
            ArffJsonInstancesSource(List(
                ArffJsonInstance("7", List(), Map(4 -> 1.5850, 11 -> 2.5850), 14)
            ), ArffJsonHeader(14))
        )
        
        println(newInst)
    }
}



@serializable
abstract class SvdLibCLsiFilter(source: ArffJsonInstancesSource, numDims: Int) extends GlobalFilter {
    val m: Array[Array[Double]] = SvdLibCLsiFilter.determineM(source, numDims)
    
    def applyFilter(source: ArffJsonInstancesSource) = {
        val newHeader = ArffJsonHeader(numDims)
        
        println("numNaNs in m: " + m.map(_.count(_.isNaN())).sum)
        
        source.map(
            elemFun = ((inst: ArffJsonInstance) => SvdLibCLsiFilter.projectDoc(inst, m)),
            headerFun = header => newHeader
        )
    }
}












