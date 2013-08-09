package conversion

import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import parser.ArffJsonInstancesSource
import format.arff_json.DenseData
import format.arff_json.SparseData

object ArffJson2Joachims {
    private def convert(inst: ArffJsonInstancesSource, outFile: File, classFun: List[String] => String) {
        val out = new BufferedWriter(new FileWriter(outFile), 102400);
        
        for(arffJsonInstance <- inst.iterator) {
            val isClass = classFun(arffJsonInstance.categories)
            
            val data = (arffJsonInstance match {
                case d: DenseData => {
                    (1 to d.dataList.size).zip(d.dataList)
                }
                case s: SparseData => {
                    s.dataMap.toList.sortBy(_._1)
                }
            })
            .map(t => t._1+1 + ":" + t._2)
            .mkString(" ")
            
            out.write(isClass + " " + data + "\n")
        }
        
        out.close
    }
    
    def apply(inst: ArffJsonInstancesSource, outFile: File, classFun: List[String] => Option[Boolean]) {
        convert(inst, outFile, (
            (mscClasses) => {
                val isTarget = classFun(mscClasses)
                (if(isTarget.isDefined && isTarget.get) "+1" else if(isTarget.isDefined) "-1" else "0")
            }
        ))
    }
    
    def apply(inst: ArffJsonInstancesSource, outFile: File) {
        convert(inst, outFile, (_ => "0"))
    }
}






