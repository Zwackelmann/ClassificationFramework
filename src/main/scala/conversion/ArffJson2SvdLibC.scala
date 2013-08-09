package conversion

import parser.ArffJsonInstancesSource
import java.io.File
import format.arff_json.ArffJsonInstance
import java.io.DataOutputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.RandomAccessFile
import format.arff_json.SparseData
import parser.ArffJsonInstancesSource
import format.arff_json.ArffJsonHeader
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.BufferedReader
import java.io.FileReader

object ArffJson2SvdLibC {
    /*
     List(
         Map(1 -> 1.0, 3 -> 2.0, 5 -> 1.0),
         Map(2 -> 2.0, 5 -> 2.0, 7 -> 1.0, 9 -> 1.0), 
         Map(5 -> 1.0, 8 -> 1.0),
         Map(6 -> 3.0, 7 -> 1.0, 9 -> 1.0),
         Map(0 -> 1.0, 2 -> 1.0, 7 -> 2.0, 9 -> 1.0)
     )
     */
    
    // docVec.add(1, 2.0)
    // docVec.add(3, 1.0)
    // docVec.add(7, 4.0)
    // [0.4824821208265865, -1.007963959258292, 0.7675708990162, 0.46871155259168323, -0.590912166907916]
    // svd -o output-filename-prefix -r sb -w db -d 5 input-filename
    
    def main(args: Array[String]) {
        main2(args)
        /*val source = ArffJsonInstancesSource(List(
                 ArffJsonInstance("1", List(), Map(1 -> 1.0, 3 -> 2.0, 5 -> 1.0), 10),
                 ArffJsonInstance("2", List(), Map(2 -> 2.0, 5 -> 2.0, 7 -> 1.0, 9 -> 1.0), 10), 
                 ArffJsonInstance("3", List(), Map(5 -> 1.0, 8 -> 1.0), 10),
                 ArffJsonInstance("4", List(), Map(6 -> 3.0, 7 -> 1.0, 9 -> 1.0), 10),
                 ArffJsonInstance("5", List(), Map(0 -> 1.0, 2 -> 1.0, 7 -> 2.0, 9 -> 1.0), 10)
            ),
            ArffJsonHeader(10)
        )
        
        dumpInstancesForSvdLibC(source, new File("test"))*/
        
        
        /*val source = ArffJsonInstancesSource(List(
                 ArffJsonInstance("1", List(), Map( 0 -> 9.2670, 1 -> 11.1720,  2 -> 8.8520,  6 -> 0.5850,  8 -> 8.5871), 14),
                 ArffJsonInstance("2", List(), Map( 2 -> 5.2651, 6 ->  0.5850,  9 -> 4.0971                            ), 14), 
                 ArffJsonInstance("3", List(), Map( 4 -> 8.5466, 6 ->  0.5850,  9 -> 3.1699                            ), 14),
                 ArffJsonInstance("4", List(), Map( 3 -> 9.2670, 7 ->  9.2670, 12 -> 9.2670, 13 -> 8.5871              ), 14),
                 ArffJsonInstance("5", List(), Map(10 -> 4.7549                                                        ), 14),
                 ArffJsonInstance("5", List(), Map( 4 -> 6.8501, 5 -> 10.3399,  6 -> 0.5850, 10 -> 7.6195, 11 -> 9.8419), 14)
            ),
            ArffJsonHeader(14)
        )*/
        
        //dumpInstancesForSvdLibC(source, new File("test2"))
    }
    
    def main2(args: Array[String]) {
        val s = readInvSingularValues(new File("matrix2-S"))
        val m = readUAndMultiplyWithS(new File("matrix2-Ut"), s)
        
        // val m = multiplySu(s, u)
        //println(m.toList.map(_.toList).mkString("\n"))
        /*println(u.toList.map(_.toList).mkString("\n"))
        println()
        println(s.toList)
        println()*/
        // println(m.toList.map(_.toList).mkString("\n"))
        
        // println(projectDoc(ArffJsonInstance("1", List(), Map(1 -> 2.0, 3 -> 1.0, 7 -> 4.0), 10), m))
        
        println(projectDoc(ArffJsonInstance("1", List(), Map(4 -> 1.5850, 11 -> 2.5850), 14), m))
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
    
    def readInvSingularValues(sigmaMatrixFile: File) = {
        val br = new BufferedReader(new FileReader(sigmaMatrixFile))
        
        val dims = br.readLine().toInt
        val s = Array.ofDim[Double](dims)

        for(i <- 0 until dims) {
            s(i) = -1d / br.readLine().toDouble
        }
        
        s
    }
    
    def projectDoc(inst: ArffJsonInstance, m: Array[Array[Double]]) = {
        val dims = m.length
        
        val docMap = inst.asInstanceOf[SparseData].dataMap.asInstanceOf[Map[Int, Double]]
        
        (for(i <- 0 until dims) yield {
            docMap.map(kv => m(i)(kv._1) * kv._2).sum
        }).toList
    }
    
    /*public DoubleVector project(SparseDoubleVector docVec, int numDims) {
        // Transform the vector according to this instance's transform's state,
        // which should normalize the vector as the original vectors were.
        DoubleVector transformed = transform.transform(docVec);

        // Represent the document as a 1-column matrix        
        Matrix queryAsMatrix = new ArrayMatrix(1, numDims);
        for (int nz : docVec.getNonZeroIndices())
            queryAsMatrix.set(0, nz, docVec.get(nz));
        
        // Project the new document vector, d, by using
        //
        //   d * U_k * Sigma_k^-1
        //
        // where k is the dimensionality of the LSA space
        
        Matrix UtimesSigmaInv = null;
            
        // We cache the reuslts of the U_k * Sigma_k^-1 multiplication since
        // this will be the same for all projections.
        while (UtimesSigmaInv == null) {
            if (UtimesSigmaInvRef != null
                    && ((UtimesSigmaInv = UtimesSigmaInvRef.get()) != null))
                break;
            
            int rows = sigma.rows();
            double[] sigmaInv = new double[rows];
            for (int i = 0; i < rows; ++i)
                sigmaInv[i] = 1d / sigma.get(i, i);
            DiagonalMatrix sigmaInvMatrix = new DiagonalMatrix(sigmaInv);

            UtimesSigmaInv =
                Matrices.multiply(U, sigmaInvMatrix);
            // Update the field with the new reference to the precomputed matrix
            UtimesSigmaInvRef = new WeakReference<Matrix>(UtimesSigmaInv);
        }

        // Compute the resulting projected vector as a matrix
        Matrix result = Matrices.multiply(queryAsMatrix, UtimesSigmaInv);

        // Copy out the vector itself so that we don't retain a reference to the
        // matrix as a result of its getRowVector call, which isn't guaranteed
        // to return a copy.
        int cols = result.columns();
        DoubleVector projected = new DenseVector(result.columns());
        for (int i = 0; i < cols; ++i)
            projected.set(i, result.get(0, i));
        return projected;
    }
    
    
    private static Matrix multiplyRightDiag(Matrix m1, Matrix m2) {
        Matrix resultMatrix = create(m1.rows(), m2.columns(), true);
        for (int r = 0; r < m1.rows(); ++r) {
            double[] row = m1.getRow(r);
            for (int c = 0; c < m2.columns(); ++c) {
                double value = m2.get(c, c);
                resultMatrix.set(r, c, value * row[c]);
            }
        }
        return resultMatrix;
    }*/
}









