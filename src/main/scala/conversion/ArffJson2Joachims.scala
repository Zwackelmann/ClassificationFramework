package conversion

import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import format.arff_json.DenseArffJsonInstance
import format.arff_json.SparseArffJsonInstance
import parser.ArffJsonInstancesSource

object ArffJson2Joachims {
    private def convert(inst: ArffJsonInstancesSource, outFile: File, classFun: List[String] => String) {
        val out = new BufferedWriter(new FileWriter(outFile), 102400);
        
        for(arffJsonInstance <- inst.iterator) {
            val isClass = classFun(arffJsonInstance.categories)
            
            val data = (arffJsonInstance match {
                case d: DenseArffJsonInstance => {
                    (1 to d.dataList.size).zip(d.dataList)
                }
                case s: SparseArffJsonInstance => {
                    s.dataMap.toList.sortBy(_._1)
                }
            })
            .map(t => t._1+1 + ":" + t._2)
            .mkString(" ")
            
            out.write(isClass + " " + data + "\n")
        }
        
        out.close
    }
    
    def apply(inst: ArffJsonInstancesSource, outFile: File, classFun: List[String] => Boolean) {
        convert(inst, outFile, ((mscClasses) => if(classFun(mscClasses)) "+1" else "-1"))
    }
    
    def apply(inst: ArffJsonInstancesSource, outFile: File) {
        convert(inst, outFile, (_ => "0"))
    }
}






