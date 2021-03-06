package parser
import format.arff_json.ArffJsonInstance
import format.arff_json.ArffJsonHeader
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import filter.Filter
import common.Path.arffJsonPath
import filter.GlobalFilter
import filter.FilterFactory
import scala.collection.mutable
import format.arff_json.Point
import java.io.BufferedReader
import java.io.FileReader
import classifier.CategoryIs
import common.FileManager
import common.FileManager.Protocol._
import common.Path
import com.google.gson.JsonParser
import com.google.gson.JsonObject
import com.google.gson.JsonParseException
import common.Gson

object ArffJsonInstancesSource {
    def apply(_source: Iterable[ArffJsonInstance], _header: ArffJsonHeader, _contentDescription: ContentDescription): ArffJsonInstancesSource with ContentDescribable = {
        new ArffJsonInstancesSource with ContentDescribable {
            val header = _header
            def iterator = _source.iterator
            val contentDescription = _contentDescription
        }
    }
    
    def apply(_source: Iterable[ArffJsonInstance], _header: ArffJsonHeader): ArffJsonInstancesSource = {
        new ArffJsonInstancesSource {
            val header = _header
            def iterator = _source.iterator
        }
    }
    
    def apply(cd: ContentDescription) = (FileManager !? ReceiveFile(cd.fullFilename)) match {
        case AcceptReceiveFile(file) => {
            val inst = new ArffJsonInstancesSource() with ContentDescribable {
                def reader = new BufferedReader(new FileReader(file))
            
                lazy val header: ArffJsonHeader = {
                    val r = reader
                    
                    val h = try {
                        Gson.fromJson(r.readLine, classOf[ArffJsonHeader])
                    } catch {
                        case jsonEx: JsonParseException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                        case e: Throwable => throw e
                    }
                    r.close()
                    h
                }
                
                def iterator = {
                    val r = reader
                    r.readLine()
                    new ArffJsonInstancesIterator(r, header)
                }
                
                val contentDescription = cd
            }
            Some(inst)
        }
        case FileNotExists => None
    }
    
    def apply(base: String, set: ContentDescription.Set, formatHistory: List[(FilterFactory, ContentDescription)]): Option[ArffJsonInstancesSource with ContentDescribable] = apply(ContentDescription(base, set, formatHistory))
    
    def apply(_fullFilename: String, cd: ContentDescription) = new ArffJsonInstancesSource() with ContentDescribable {
        def reader = (FileManager !? ReceiveFile(_fullFilename)) match {
            case AcceptReceiveFile(file) => new BufferedReader(new FileReader(file))
            case FileNotExists => throw new RuntimeException(_fullFilename + " does not exist")
        }
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                Gson.fromJson(r.readLine(), classOf[ArffJsonHeader])
            } catch {
                case jsonEx: JsonParseException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e: Throwable => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
        
        override def fullFilename = _fullFilename
        
        val contentDescription = cd
    }
    
    def apply(_fullFilename: String): ArffJsonInstancesSource = new ArffJsonInstancesSource() {
        override def fullFilename = _fullFilename
        override def saved = true
        override def save(fullFilename: String) = error("File cannot be saved")
        override def save() = error("File cannot be saved")
        
        def reader = (FileManager !? ReceiveFile(_fullFilename)) match {
            case AcceptReceiveFile(file) => new BufferedReader(new FileReader(file))
            case FileNotExists => throw new RuntimeException(_fullFilename + " does not exist")
        }
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                Gson.fromJson(r.readLine(), classOf[ArffJsonHeader])
            } catch {
                case jsonEx: JsonParseException => 
                    throw jsonEx
                case e: Throwable => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
    }
    
    def centroids(inst: ArffJsonInstancesSource): Map[Int, Point] = {
        def merge(a: Map[Int, Double], b: Map[Int, Double], fun: (Double, Double) => Double) = {
            (for(key <- a.keys ++ b.keys) yield {
                val aValue = a.getOrElse(key, 0.0)
                val bValue = b.getOrElse(key, 0.0)
                key -> fun(aValue, bValue)
            }).toMap
        }
        
        val map = new mutable.HashMap[Int, Pair[Map[Int, Double], Int]] {
            override def default(key: Int) = (Map[Int, Double](), 0)
        }
        
        for(i <- inst) {
            for(group <- i.categories.map(_.substring(0, 2).toInt).distinct) {
                val mapItem = map(group)
                map(group) = Pair(merge(mapItem._1, i.sparseData, (a, b) => a + b), mapItem._2 + 1)
            }
        }
        map.mapValues(m => Point(m._1.mapValues(v => v / m._2), inst.numAttributes)).toMap
    }
    
    def save(source: ArffJsonInstancesSource, filename: String) {
        (FileManager !? CreateFile(filename)) match {
            case AcceptCreateFile(fileHandle) => {
                val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                writer.write(Gson.toJson(source.header, classOf[ArffJsonHeader]) + "\n")
                for(inst <- source.iterator) {
                    writer.write(inst.toJson + "\n")
                }
                writer.close()
                fileHandle.close
            }
            case RejectCreateFile => throw new RuntimeException("Could not save ArffJsonInstancesSource") 
        }
    }
}

trait ArffJsonInstancesSource extends Iterable[ArffJsonInstance] {
    def header: ArffJsonHeader
    def iterator: Iterator[ArffJsonInstance]
    def applyFilter(filter: GlobalFilter) = {
        filter.applyFilter(this)
    }
    
    def applyFilter(filter: Filter, categoryIs: CategoryIs) = {
        filter.applyFilter(this, categoryIs)
    }
    
    def numAttributes = header.numAttributes
    
    lazy val numInstances = iterator.size
    
    def flatMap(elemFun: ArffJsonInstance => List[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: FilterFactory): ArffJsonInstancesSource with ContentDescribable = {
        this match {
            case c: ContentDescribable => {
                val thisInst = this
                
                new ArffJsonInstancesSource with ContentDescribable {
                    def header = headerFun(header)
                    def iterator = thisInst.iterator.flatMap(elemFun)
                    val contentDescription = c.contentDescription.addHistoryItem((historyAppendix,c.contentDescription))
                }
            }
        }
    }
    
    def flatMap(elemFun: ArffJsonInstance => List[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader): ArffJsonInstancesSource = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def header = headerFun(header)
            def iterator = thisInst.iterator.flatMap(elemFun)
        }
    }
    
    def map(elemFun: ArffJsonInstance => ArffJsonInstance, headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: FilterFactory): ArffJsonInstancesSource with ContentDescribable = this match {
        case co: ContentDescribable => {
            val thisInst = this
            
            new ArffJsonInstancesSource with ContentDescribable {
                def header = headerFun(thisInst.header)
                def iterator = thisInst.iterator.map(elemFun)
                val contentDescription = co.contentDescription.addHistoryItem((historyAppendix, co.contentDescription))
            }
        }
        
        case _ => throw new RuntimeException("map(ArffJsonInstance => ArffJsonInstance, ArffJsonHeader => ArffJsonHeader, FilterFactory) cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
    
    def map(elemFun: ArffJsonInstance => ArffJsonInstance, headerFun: ArffJsonHeader => ArffJsonHeader) = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.map(elemFun)
            def header = headerFun(thisInst.header)
        }
    }
    
    def project(ids: Set[Int]): ArffJsonInstancesSource = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.map(inst => inst.project(ids))
            
            val _header = if(thisInst.header.explicitAttributes) {
                ArffJsonHeader(
                    thisInst.header.relationName, 
                    ids.map(id => thisInst.header.attribute(id))
                )
            } else {
                ArffJsonHeader(
                    thisInst.header.relationName, 
                    ids.size
                )
            }
            
            def header = _header
        }
    }
    
    override def filter(filterFun: ArffJsonInstance => Boolean) = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.filter(filterFun)
            
            def header = thisInst.header
        }
    } 
    
    override def filterNot(filterFun: ArffJsonInstance => Boolean) = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.filterNot(filterFun)
            
            def header = thisInst.header
        }
    } 
    
    def ++(inst: ArffJsonInstancesSource) = {
        require(this.header.numAttributes == inst.header.numAttributes, "Headers do not match in concatenation")
        
        val thisInst = this
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator ++ inst.iterator
            
            val _header = ArffJsonHeader(
                thisInst.header.relationName + " ++ " + inst.header.relationName, 
                thisInst.header.attributes
            )
            
            def header = thisInst.header
        }
    }
    
    override def take(num: Int) = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.take(num)
            def header = thisInst.header
        }
    }
    
    def fullFilename: String = this match {
        case co: ContentDescribable => co.contentDescription.fullFilename
        case _ => throw new RuntimeException("file() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
    
    def save(fullFilename: String) {
        (FileManager !? CreateFile(fullFilename)) match {
            case AcceptCreateFile(fileHandle) => {
                val writer = new BufferedWriter(new FileWriter(fileHandle.file))
                writer.write(Gson.toJson(header, classOf[ArffJsonHeader]) + "\n")
                for(inst <- iterator) {
                    writer.write(inst.toJson + "\n")
                }
                writer.close()
                fileHandle.close
            }
            case RejectCreateFile => throw new RuntimeException("Could not save ArffJsonInstancesSource") 
        }
    }
    
    def save() {
        this match {
            case co: ContentDescribable => save(co.contentDescription.fullFilename)
            case _ => throw new RuntimeException("save() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
        }
    }
    
    def saved() = this match {
        case co: ContentDescribable => (FileManager !? DoesFileExist(co.contentDescription.fullFilename)) match {
            case FileExists => true
            case FileNotExists => false
        }
        case _ => throw new RuntimeException("saved() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
}












