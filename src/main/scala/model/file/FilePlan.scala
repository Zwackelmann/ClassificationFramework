package model.file
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import format.arff_json.DenseArffJsonInstance
import format.arff_json.SparseArffJsonInstance


class FilePlan(_file: File, fun: FilePlan => Unit) {
    def transform(outFile: File)(fun: (FilePlan, File) => Unit) = 
        new FilePlan(outFile, FilePlan => fun(this, outFile))
    
    def fileref = _file
    
    def file = {
        if(!_file.exists()) {
            println("Creating " + _file.getName())
            fun(this)
            if(!_file.exists()) {
                throw new RuntimeException("A function for a fileplan did not create the file the file plan is intended for")
            }
        }
        _file
    }
}












