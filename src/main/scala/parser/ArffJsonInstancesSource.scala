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
import net.sf.json.JSONSerializer
import java.io.FileReader
import net.sf.json.JSONObject
import net.sf.json.JSONException

object ArffJsonInstancesSource {
    def apply(_source: Iterable[ArffJsonInstance], _header: ArffJsonHeader, _contentDescription: ContentDescription): ArffJsonInstancesSource with ContentDescribable = {
        new ArffJsonInstancesSource with ContentDescribable {
            val header = _header
            val iterator = _source.iterator
            val contentDescription = _contentDescription
        }
    }
    
    def apply(cd: ContentDescription) = new ArffJsonInstancesSource() with ContentDescribable {
        def reader = new BufferedReader(new FileReader(file))
    
        lazy val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("File: " + contentDescription.file + ": The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
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
    
    def apply(base: String, set: ContentDescription.Set, formatHistory: List[FilterFactory]): ArffJsonInstancesSource with ContentDescribable = apply(ContentDescription(base, set, formatHistory))
    
    def apply(_file: File, cd: ContentDescription) = new ArffJsonInstancesSource() with ContentDescribable {
        def reader = new BufferedReader(new FileReader(file))
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
            }
            r.close()
            h
        }
        
        def iterator = {
            val r = reader
            r.readLine()
            new ArffJsonInstancesIterator(r, header)
        }
        
        override def file = _file
        
        val contentDescription = cd
    }
    
    def apply(_file: File): ArffJsonInstancesSource = new ArffJsonInstancesSource() {
        override def file = _file
        override def saved = true
        override def save(file: File) = error("File cannot be saved")
        override def save() = error("File cannot be saved")
        
        def reader = new BufferedReader(new FileReader(file))
    
        val header: ArffJsonHeader = {
            val r = reader
            
            val h = try {
                JSONSerializer.toJSON(r.readLine) match {
                    case o: JSONObject => ArffJsonHeader.jsonToArffJsonHeader(o)
                    case _ => throw new RuntimeException("The first line in file cannot be interpeted as a JSON object")
                }
            } catch {
                case jsonEx: JSONException => throw new RuntimeException("The first line in file does not contain a valid JSON string")
                case e => throw e
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
}

trait ArffJsonInstancesSource extends Iterable[ArffJsonInstance] {
    def header: ArffJsonHeader
    def iterator: Iterator[ArffJsonInstance]
    def applyFilter(filter: GlobalFilter) = {
        filter.applyFilter(this)
    }
    
    def numAttributes = header.attributes.size
    
    lazy val numInstances = iterator.size
    
    def flatMap(elemFun: ArffJsonInstance => List[ArffJsonInstance], headerFun: ArffJsonHeader => ArffJsonHeader, historyAppendix: FilterFactory): ArffJsonInstancesSource with ContentDescribable = {
        this match {
            case c: ContentDescribable => {
                val thisInst = this
                
                new ArffJsonInstancesSource with ContentDescribable {
                    def header = headerFun(header)
                    def iterator = thisInst.iterator.flatMap(elemFun)
                    val contentDescription = c.contentDescription.addFilterFactory(historyAppendix)
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
                val contentDescription = co.contentDescription.addFilterFactory(historyAppendix)
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
    
    def project(ids: List[Int]): ArffJsonInstancesSource = {
        val thisInst = this
        
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator.map(inst => inst.project(ids))
            
            val _header = new ArffJsonHeader(
                thisInst.header.relationName, 
                ids.map(id => thisInst.header.attributes(id))
            )
            
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
    
    def ++(inst: ArffJsonInstancesSource) = {
        require(this.header.attributes == inst.header.attributes, "Headers do not match in concatenation")
        
        val thisInst = this
        new ArffJsonInstancesSource {
            def iterator = thisInst.iterator ++ inst.iterator
            
            val _header = new ArffJsonHeader(
                thisInst.header.relationName + " ++ " + inst.header.relationName, 
                thisInst.header.attributes
            )
            
            def header = thisInst.header
        }
    }
    
    def file: File = this match {
        case co: ContentDescribable => co.contentDescription.file
        case _ => throw new RuntimeException("file() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
    
    def save(_file: File) {
        if(_file.exists()) {
            // do not save the same file you are reading from (doesn't make sence??)
        } else {
            val writer = new BufferedWriter(new FileWriter(_file))
            writer.write(header.toJson + "\n")
        
            for(inst <- iterator) {
                writer.write(inst.toJson + "\n")
            }
            writer.close()
        }
    }
    
    def save() {
        this match {
            case co: ContentDescribable => save(file)
            case _ => throw new RuntimeException("file() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
        }
    }
    
    def saved() = this match {
        case co: ContentDescribable => file.exists()
        case _ => throw new RuntimeException("saved() cannot be called if ArffJsonInstancesSource is not ContentDescribable")
    }
}












